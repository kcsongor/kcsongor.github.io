#!/usr/bin/env python3
"""Writes the stages of the post's schedules, as Cachet's stepper prints them.

Run it from a Cachet checkout, with cachet_datadir set to that checkout:

    python3 <site>/scripts/cachet-schedule-stages.py <cachetc> <site>

Each viewer is a program, and its stages are steps of the program's
schedules: `cachetc scheduled --steps --json` gives each schedule's function
after each of its steps, and for each line the steps it comes from. A stage
shows a schedule up to its step, and links each step taken to its lines. A
step that lays out only with a later one shows the function before it, and
says why. A viewer may also show several schedules side by side, each after
its last step.

PROVISIONAL: the post shows no folds. Each fold is printed as the loop it
means: one that keeps its running total in a `var` and stores it at the end,
or, where the fold starts from its own cell, one that adds into the cell at
every step, as the checker reads it (`per cell c fold`). A line written out
from a fold comes from every step any of the fold's lines comes from.
"""
import json
import os
import re
import subprocess
import sys

CACHETC, SITE = sys.argv[1:]
FILE = "test/run/blog-stage.cachet"

HEAD = """    (reads a: [S.Value; M, K])
    (reads b: [S.Value; K, N])
    -> out c: [S.Value; M, N] ="""
MATMUL = f"""contraction matmul {{S : Semiring}}
{HEAD}
  c[i, j] = sum k. a[i, k] * b[k, j]"""
MATMUL_F32 = MATMUL.replace(" {S : Semiring}", "").replace("S.Value", "f32")
ATTEND = """contraction attend
    (reads q: [f32; 64, 16])
    (reads kt: [f32; 16, 32])
    (reads v: [f32; 32, 16])
    -> out y: [f32; 64, 16] =
  s[i, j] = sum c. q[i, c] * kt[c, j]
  y[i, d] =
    normalised (reduce[reassoc] Online j. key (s[i, j]) (v[j, d]))"""

IMPORTS = """module (platform = AppleSilicon)
import "../../platforms/apple-silicon.cachet"
import "cachet:monoid"
import "cachet:simd"
import "cachet:steps"
import "../../examples/lib/panel-major.cachet"
"""
ATTENTION = IMPORTS + 'import "cachet:attention"\n'


def blocks(depth, rows, columns, inner, rows_first=False):
    """A tower's steps: blocks of depth, rows and columns, groups of `inner`
    rows within each block of rows, and an order with the depth blocks
    outside the cells and the columns outside the rows unless `rows_first`."""
    order = "gk, gi, gj" if rows_first else "gk, gj, gi"
    return ["zero (i, j) c",
            f"split k by {depth} as gk ragged",
            f"split i by {rows} as gi, {inner} as ui ragged",
            f"split j by {columns} as gj ragged",
            f"order ({order}, ui, i, j)"]


def packed(name, rows, depth, columns, inner, rows_first=False, copy=False):
    """`packed`'s schedule, with its blocks, order and register tile changed."""
    head = f"""schedule {name} of matmul @(S = PlusTimes f32)
    (mut packA: [f32; {rows}, {depth}] in PanelMajor 4 at DRAM)
    (mut packB: [f32; {depth}, {columns}] at DRAM) ="""
    steps = (blocks(depth, rows, columns, inner, rows_first)
             + ["pack a into packA along gi over (a0, a1)"]
             # Each panel of A copied four rows at a time, so a panel's rows
             # are stored together, as the tuned towers copy them.
             + (["strip_blocks_ragged a0 4 (pa, ra)", "interchange (ra, a1)"] if copy else [])
             + ["keep packA along gi within L2 into packA_L2",
                "pack b into packB along gj over (b0, b1)",
                "keep packB along gj within L1 into packB_L1",
                f"registers (i, j) into c rows 4 lanes {columns} through fma"])
    return head, steps


TOWER = ("schedule tower of matmul =", blocks(16, 16, 8, 4))
PACKED = packed("packed", 16, 16, 8, 4)
SME = ("""schedule sme of matmul @(S = PlusTimes f64)
    (mut packA: [f64; M, 3072] in PanelMajor 32)
    (mut packB: [f64; 3072, 256] in ColumnPanels 16 at DRAM) =""",
       ['split k by 3072 as gk ragged',
        'split j by 256 as gj, 16 as uj',
        'split i by 1536 as gi, 32 as ui',
        'order (gk, gj, gi, ui, uj, i, j)',
        'parallel gi',
        'pack a into packA along gk over (a0, a1)',
        'strip a0 32 (pa, ra)',
        'parallel pa',
        'pack b into packB along gj over (b0, b1)',
        'strip b1 16 (pb, rb)',
        'interchange (b0, pb)',
        'parallel pb',
        'registers (i, j) into c rows 32 lanes 16 through fma',
        'outer_products i c 8 8 outer_fma_primitive'])
# Column panels, as the example defines them, for `sme`'s packed B.
COLUMN_PANELS = """
layout ColumnPanels (P : Nat) where

impl Layout2D A Rows Cols for ColumnPanels P requires 0 < P, Cols % P == 0 where
  fn byte_offset i j = sizeof A * (floor (j / P) * (Rows * P) + i * P + j % P)
  const allocation_bytes = sizeof A * (floor (Cols / P) * Rows) * P
"""

CONTRACTION = "Each output adds up its products in ascending order of k, starting from zero."

# name: (prelude, contraction, [schedule], [stage]). A schedule is (head,
# steps). A stage is (schedule, step, title, note), where step 0 is the
# contraction's own loop nest; a stage whose title is None is titled by its
# step. A viewer whose stages are each a different schedule's last step
# marks what changed from the stage before it.
VIEWERS = {
    "swap": (IMPORTS, MATMUL_F32, [
        ("schedule swapped of matmul =", ["zero (i, j) c", "hoist (j, k) c"]),
    ], [
        ("swapped", 0, "The contraction", CONTRACTION),
        ("swapped", 1, None,
         '`zero` stores zero in every cell first, in a loop of its own. The listing shows each sum as updates to its cell, with the same products in the same order.'),
        ("swapped", 2, None,
         "`hoist` runs the sum's steps outside the loop over j, and each cell holds its running total between visits. Each output receives the same multiply-adds in the same order."),
    ]),
    "rows": (IMPORTS, MATMUL, [
        ("schedule rows of matmul @(S = PlusTimes f32) on M4Gpu =", ["grid over (i, j)"]),
    ], [
        ("rows", 0, "The contraction", CONTRACTION),
        ("rows", 1, None,
         "Each pair (i, j) runs as one GPU thread. Its write window is one element of c; the inputs may be read by many threads. Each thread's loop over k is unchanged."),
    ]),
    "tiled": (IMPORTS, MATMUL, [
        ("schedule tiled of matmul @(S = PlusTimes f32) on M4Gpu =",
         ["grid over (i, j)", "tiles (i, j, k) (16, 16) (8, 8)"]),
    ], [
        ("tiled", 0, "The contraction", CONTRACTION),
        ("tiled", 1, None, "Each pair (i, j) gets its own grid iteration, as in `rows`."),
        ("tiled", 2, None,
         "`tiles` groups the outputs into 16 × 16 tiles, one for each grid iteration, shared among four SIMD groups in 8 × 8 parts, and works through the sum eight steps of k at a time. t starts at zero and holds the tile's running totals across the k loop; x and y are the next pair of input blocks, and `mma` multiplies them and adds the product to t with the device's `simdgroup_mma8`."),
    ]),
    "tower": (IMPORTS, MATMUL, [TOWER, PACKED], [
        ("tower", 0, "The contraction",
         "The generic contraction, and the loop nest it means. Each output adds up its products in ascending order of k."),
        ("tower", 1, None,
         "Every cell is zeroed first, and its sum then starts from what the cell holds."),
        ("tower", 2, None,
         "The sum runs in blocks of at most 16 steps, gk, each continuing from what the cell holds. As in the earlier chunking example, each output keeps one running total and receives the same products in the same order."),
        ("tower", 3, None,
         "The rows are cut into blocks of 16, gi, and each block into groups of four rows, ui. i now picks a row within those four."),
        ("tower", 4, None, "The columns are cut into blocks of 8, gj, and j picks a column within a block."),
        ("tower", 5, None,
         '`order` puts the depth blocks outside the output loops. Within each depth block, it visits 16 × 8 output blocks, the blocks of columns outside the blocks of rows. Each output continues from its previous total, receiving the same products in the same order.'),
        ("packed", 6, None,
         "`pack` copies the block of A that the loops gi and gk pick out into a scratch buffer, packA, which becomes a parameter. The kernel reads the copy instead of a."),
        ("packed", 7, None, "`keep` says that packA stays in L2 while it's used, and adds that to the function's working set."),
        ("packed", 8, None, '`pack` copies the block of B into packB, which becomes another parameter of the kernel.'),
        ("packed", 9, None, '`keep` records that packB is to stay in L1 while it is used.'),
        ("packed", 10, None,
         '`registers` holds a 4 × 8 block of c across the depth loop: four rows of two four-lane vectors, updated with vector `fma`. This ties the schedule to `PlusTimes f32`.'),
    ]),
    "fast": (IMPORTS, MATMUL, [
        packed("packed", 16, 16, 8, 4),
        packed("larger", 512, 256, 8, 4, copy=True),
        packed("rows_first", 512, 256, 8, 4, rows_first=True, copy=True),
        packed("fast", 512, 256, 16, 4, rows_first=True, copy=True),
    ], [
        ("packed", None, "packed",
         "`packed` as we left it: blocks of 16 rows, 8 columns and 16 steps of depth, and a 4 × 8 register tile."),
        ("larger", None, "Larger blocks",
         "A's blocks grow to 512 rows by 256 columns, half a megabyte each, kept in L2, and A is copied four rows at a time. B's grow to 256 by 8, kept in L1. The loop order is unchanged."),
        ("rows_first", None, "Rows first",
         "`order` now puts the blocks of rows, gi, outside the blocks of columns, gj, so each panel of A is packed once per depth block and then used with every panel of B."),
        ("fast", None, "A wider tile",
         "The register tile is 4 × 16: four rows of four vectors, giving us sixteen independent chains. The columns are blocked by 16 to match, and so are B's panels."),
    ]),
    "sme": (IMPORTS + COLUMN_PANELS, MATMUL, [SME], [
        ("sme", 0, 'The contraction', 'The same contraction as before.'),
        ("sme", 1, None, 'The sum is split into blocks of up to 3072 steps, gk. The first starts each running total at −0.0; later blocks continue from the value stored in c. At 3072 × 3072, one block covers the whole sum, so c is only written, never cleared or read.'),
        ("sme", 2, None, 'The columns are cut into blocks of 256, gj, one packed block of B each, and each block into groups of 16, uj.'),
        ("sme", 3, None, "The rows are cut into bands of 1536, gi, each visited 32 rows at a time, ui. At 3072 × 3072 that's two bands, one for each of the M4 Max's two performance clusters, each with its own matrix unit."),
        ("sme", 4, None, '`order` puts the depth blocks first, then the blocks of columns and the bands of rows. Within a band, uj runs inside ui, so we visit the tiles across the same 32 rows of c before moving down to the next 32.'),
        ("sme", 5, None, 'The bands run as a grid on the host, one thread each.'),
        ("sme", 6, None, '`pack` copies each depth block of A into packA, for every row.'),
        ("sme", 7, None, "`strip` divides A's copying loop into panels of 32 rows."),
        ("sme", 8, None, 'The panels of A are copied in parallel.'),
        ("sme", 9, None, '`pack` copies each block of B into packB.'),
        ("sme", 10, None, "`strip` divides B's columns into panels of 16."),
        ("sme", 11, None, '`interchange` moves the panel loop, pb, outside the row loop, b0, so the copy visits one panel at a time.'),
        ("sme", 12, None, 'The panels of B are copied in parallel.'),
        ("sme", 13, None, '`registers` keeps the running totals for a 32 × 16 block of c in registers throughout the depth loop, updating them with vector `fma`.'),
        ("sme", 14, None, '`outer_products` uses the matrix unit to hold those totals as eight 8 × 8 blocks. For each step of k, it updates them with outer products from a column of A and a row of B. The LLVM backend selects the matrix instructions.'),
    ]),
    "flash": (ATTENTION, ATTEND, [
        ("schedule flash of attend on M4Gpu =",
         ["split i by 32, j by 8, c by 16", "grid over i.outer", "compute s at j.outer",
          "reduce j.inner by (max: halving, sum: order)", "parts [8, 16]", "element [f32; 8, 8]"]),
    ], [
        ("flash", 0, "The contraction", "One contraction, with the scores as an intermediate."),
        ("flash", 6, "The schedule",
         'Each grid iteration handles 32 queries. `compute s at j.outer` keeps a block of scores within the SIMD groups. `reduce` combines eight keys at a time: find the new maximum, rescale the summary, then add the new terms in order. These steps are shown together because the compiler needs the final `element` choice to construct the kernel.'),
    ]),
}


def program(prelude, contraction, schedules):
    return (f"{prelude}\n{contraction}\n\n"
            + "\n\n".join(head + "\n" + "".join(f"  {step}\n" for step in steps)
                          for head, steps in schedules))


def run(arguments, text, main=False):
    with open(FILE, "w") as file:
        file.write(text + ("\nexport fn main () = ()\n" if main else ""))
    try:
        done = subprocess.run([CACHETC, *arguments, FILE], capture_output=True, text=True)
    finally:
        os.remove(FILE)
    if done.returncode:
        raise SystemExit(f"refused:\n{text}\n{done.stderr}{done.stdout}")
    return done.stdout


PARAMETER = re.compile(r"\((?:reads|mut) (\w+): ([^()]*)\)")
FOLD = re.compile(r"^(\s*)\(?fold (\S+) in (.+?) from (.+) as (\S+): (.+) do$")
STORE = re.compile(r"^(\s*)store (.+) =$")
# The one fold that sits inside an expression, in `attend`, as its loops.
ATTEND_FOLDS = """      store y[i, d] =
        normalised
          (fold j in 0 .. 32 from Online.empty () as acc'1: Online.Value do
             Online.combine
               acc'1
               (key
                  (fold c'0 in 0 .. 16 from -0.0 as acc'0: f32 do
                     fma (load q[i, c'0]) (load kt[c'0, j]) acc'0)
                  (load v[j, d])))""".split("\n")
ATTEND_LOOPS = """      var acc'1: Online.Value = Online.empty ()
      for j in 0 .. 32 do
        var acc'0: f32 = -0.0
        for c'0 in 0 .. 16 do
          acc'0 = fma (load q[i, c'0]) (load kt[c'0, j]) acc'0
        acc'1 = Online.combine acc'1 (key acc'0 (load v[j, d]))
      store y[i, d] = normalised acc'1""".split("\n")


def union(lines):
    return sorted({step for _, steps in lines for step in steps})


def depth(line):
    return len(line) - len(line.lstrip())


def joined(lines):
    """`lines` with a long fold header, which is printed broken over lines,
    on one line, up to its `do`."""
    out = []
    for text, steps in lines:
        last = out[-1][0] if out else ""
        if re.search(r"\bfold \S+ in ", last) and not last.endswith(" do"):
            last, before = out.pop()
            out.append((f"{last} {text.strip()}", sorted(set(before) | set(steps))))
        else:
            out.append((text, steps))
    return out


def unfolded(lines):
    """`lines` with each fold written as the loop it means."""
    lines = joined(lines)
    texts = [text for text, _ in lines]
    for at in range(len(texts) - len(ATTEND_FOLDS) + 1):
        if texts[at:at + len(ATTEND_FOLDS)] == ATTEND_FOLDS:
            steps = union(lines[at:at + len(ATTEND_FOLDS)])
            lines = lines[:at] + [(text, steps) for text in ATTEND_LOOPS] + lines[at + len(ATTEND_FOLDS):]
            break
    out, at = [], 0
    while at < len(lines):
        store = STORE.match(lines[at][0])
        fold = at + 1 < len(lines) and FOLD.match(lines[at + 1][0])
        if not (store and fold):
            out.append(lines[at])
            at += 1
            continue
        end = at + 2
        while end < len(lines) and depth(lines[end][0]) > depth(lines[at + 1][0]):
            end += 1
        steps = union(lines[at:end])
        texts = loop(store.group(1), store.group(2), lines[at + 1][0], [text for text, _ in lines[at + 2:end]])
        out += [(text, steps) for text in texts]
        at = end
    if any("fold " in text for text, _ in out):
        raise SystemExit("a fold is left:\n" + "\n".join(text for text, _ in lines))
    return out


def loop(indent, cell, header, body, total=None):
    """The fold in `header`, with `body`, stored to `cell` at `indent`."""
    _, k, bounds, start, acc, kind = FOLD.match(header).groups()
    inner = indent + "  "
    if total is not None:
        # A fold that carries on from the one around it: one running total.
        body = [re.sub(rf"(?<![\w']){re.escape(acc)}(?![\w'])", total, line) for line in body]
        return [f"{indent}for {k} in {bounds} do"] + statement(inner, total, body, cell=None)
    if start == f"(load {cell})":
        return ([f"{indent}for {k} in {bounds} do", f"{inner}let {acc} = load {cell}"]
                + statement(inner, f"store {cell}", body, cell=None, assign=False))
    return ([f"{indent}var {acc}: {kind} = {start}", f"{indent}for {k} in {bounds} do"]
            + statement(inner, acc, body, cell=cell)
            + [f"{indent}store {cell} = {acc}"])


def statement(indent, target, body, cell, assign=True):
    """`target = body` at `indent`, where `body` may be a nested fold."""
    nested = FOLD.match(body[0])
    if nested and nested.group(4) in (target, f"({target})"):
        return loop(indent, cell, body[0], body[1:], total=target)
    if len(body) == 1:
        return [f"{indent}{target} = {body[0].strip()}"]
    shift = depth(body[0]) - len(indent) - 2
    return [f"{indent}{target} ="] + [line[shift:] if shift > 0 else " " * -shift + line for line in body]


def resugared(head):
    """The function's head with its result place written as the contraction
    writes it.

    PROVISIONAL: `cachetc scheduled` prints `-> out c: T` as the parameter
    `(mut c: T)` and `produces Init c`, the latter twice. Where `c` is still
    the last parameter, this writes it back as `-> out c: T`; where a
    schedule added parameters after it, it keeps `(mut c: T)` and says
    `Init c` once."""
    parameters = list(PARAMETER.finditer(head))
    last = parameters[-1] if parameters else None
    if (last and head[last.start() + 1:].startswith("mut ")
            and not any(match.group(1) == last.group(1) for match in parameters[:-1])
            and re.search(rf"^produces Init {last.group(1)}$", head, re.M)):
        name = last.group(1)
        head = head[:last.start()] + f"-> out {name}: {last.group(2)}" + head[last.end():]
        head = re.sub(rf"^produces Init {name}\n?", "", head + "\n", flags=re.M).rstrip("\n")
    lines, seen = [], set()
    for line in head.split("\n"):
        single = re.match(r"^produces (Init \w+)$", line)
        if single and single.group(1) in seen:
            continue
        if single:
            seen.add(single.group(1))
        lines.append(line)
    return lines


def written(function, provenance):
    """The function as the post shows it, each line with its steps."""
    lines = list(zip(function.rstrip("\n").split("\n"), provenance))
    at = next(n for n, (text, _) in enumerate(lines) if text == "=")
    head, body = lines[:at], lines[at:]
    by_text = {}
    for text, steps in head:
        by_text.setdefault(text.strip(), steps)
    printed = resugared("\n".join(text for text, _ in head))
    head = [(text, by_text.get(text.strip(), union(head))) for text in printed]
    return head + unfolded(body)


def claim(lines):
    # Reasons run to a sentence each. The post says that they're shortened.
    return "\n".join(re.sub(r' = "[^"]*"', " = ...", line) for line in lines) or None


def source(contraction, schedule, taken):
    """The contraction and the schedule, with the steps after `taken` marked
    as still to come, and the line of each step."""
    head, steps = schedule
    lines = contraction.split("\n") + [""] + head.split("\n")
    first = len(lines)
    lines += [f"  {step}" for step in steps]
    return "\n".join(lines), [first + at for at in range(len(steps))], taken


def wrote_by(entry, j):
    """The lines step `j` of a schedule wrote that weren't there before it,
    as the post shows them."""
    state, before = entry["steps"][j], entry["steps"][j - 1]
    if "function" not in state or "function" not in before:
        return set()
    old = {text.strip() for text, _ in written(before["function"], before["lines"])}
    return {text.strip() for text, of in written(state["function"], state["lines"])
            if j in of and text.strip() not in old}


stages = {}
for name, (prelude, contraction, schedules, plan) in VIEWERS.items():
    text = program(prelude, contraction, schedules)
    run(["check"], text, main=True)
    stepped = {entry["schedule"]: entry for entry in json.loads(run(["scheduled", "--steps", "--json"], text))}
    by_name = {re.match(r"schedule (\w+)", head).group(1): (head, steps) for head, steps in schedules}
    stages[name] = []
    side_by_side = all(step is None for _, step, _, _ in plan)
    for schedule, step, title, note in plan:
        entry = stepped[schedule]
        head, steps = by_name[schedule]
        last = len(entry["steps"]) - 1
        k = last if step is None else step
        state = entry["steps"][k]
        laid_later = state.get("laidOutLater")
        shown = state
        while "function" not in shown:
            shown = entry["steps"][shown["step"] - 1]
        lines = written(shown["function"], shown["lines"])
        output = "\n".join(text for text, _ in lines)
        code, step_lines, taken = source(contraction, (head, steps), k)
        links = []
        for j in range(1, k + 1):
            # The stepper gives each line its latest step only, so a step
            # also keeps the lines it wrote that a later step only moved,
            # such as a split's loops after an `order`.
            wrote = wrote_by(entry, j)
            linked = [n for n, (text, of) in enumerate(lines)
                      if (j in of or (text.strip() in wrote and max(of) >= j)) and text.strip() and not text.startswith("fn ")]
            # A link to nearly everything doesn't say anything.
            if linked and len(linked) <= 0.9 * len(lines):
                links.append({"source": step_lines[j - 1], "lines": linked})
        changed = None
        if not side_by_side:
            changed = [] if laid_later else [n for n, (_, of) in enumerate(lines) if k and k in of]
        stages[name].append({
            "title": title or steps[k - 1],
            "source": code,
            "future": step_lines[k:],
            "current": step_lines[k - 1] if k else None,
            "output": output,
            "changed": changed,
            "verdict": claim(entry["claim"]) if k == last else None,
            "note": laid_later and f"This step lays out only with a later one: {laid_later}." if laid_later and not note else note or "",
            "links": links,
        })

path = os.path.join(SITE, "src/components/figures/cachet-schedule-stages.json")
with open(path, "w") as file:
    json.dump(stages, file, indent=2, ensure_ascii=False)
    file.write("\n")
print("wrote", path, {name: len(plan) for name, plan in stages.items()})
