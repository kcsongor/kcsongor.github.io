#!/usr/bin/env python3
"""Benchmarks the post's schedules, and the ceilings they're compared with.

    bench/run.py --cachet ~/Dev/Cachet [--cachetc PATH] [--sizes 1024]
                 [--only tower,packed,...] [--out bench/results/NAME]

Each schedule is the one in the post's complete example,
`public/code/cachet-contractions.cachet`, so the benchmark always times the
code the post shows. For each schedule and size, the script writes a source
that is the example without its `main`, plus one exported wrapper that calls
the schedule at N x N, and builds it with `cachetc build --target llvm
--emit-llvm --release`. It compiles the emitted module and its runtime (and
the Metal bridge, for a GPU schedule) with `harness.c`, which allocates
through Cachet's allocator, checks the result and times the call. The
ceilings use the same harness on the same inputs: OpenBLAS on all cores and
on one, Accelerate, whose BLAS uses the M4's matrix unit, and MLX on the GPU.

Everything is built before anything is timed, then each image runs on its
own, in a fixed order. `OUT/results.json` and `OUT/results.md` hold the
measurements; `OUT/manifest.json` records the Cachet revision and whether its
tree was clean, the compiler's hash, the library versions, the machine and
the commands, so that a later run can be compared with this one.
"""
import argparse
import datetime
import hashlib
import json
import os
import platform
import re
import shlex
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SITE = HERE.parent
EXAMPLE = SITE / "public" / "code" / "cachet-contractions.cachet"
STRICT = ["-O3", "-std=gnu11", "-fno-fast-math", "-ffp-contract=off", "-fno-strict-aliasing"]

# name: (element, check, where, wrapper body, scratch elements or None). The
# body names the operands `a`, `b`, `c`, and the scratch `scratch`.
KERNELS = {
    "matmul": ("f32", "exact", "CPU", "matmul @(S = PlusTimes f32) a b c", None),
    "swapped": ("f32", "exact", "CPU", "swapped @(S = PlusTimes f32) a b c", None),
    "tower": ("f32", "exact", "CPU", "tower @(S = PlusTimes f32) a b c", None),
    "packed": ("f32", "exact", "CPU", """temp w = allocate @DRAM View Workspace
  packed a b c (view w.packA) (view w.packB)""", None),
    "fast": ("f32", "exact", "CPU", """temp w = allocate @DRAM View Fast
  fast a b c (view w.packA) (view w.packB)""", None),
    "bands": ("f32", "exact", "CPU, all cores", """temp w = allocate @DRAM View Bands
  bands a b c (view w.packA) (view w.packB)""", None),
    "sme": ("f64", "exact", "CPU matrix units, all cores", """temp t = allocate @DRAM View Tiles
  sme a b c (view t.packA) (view t.packB)""", None),
    "rows": ("f32", "exact", "GPU", "rows @(S = PlusTimes f32) a b c", None),
    "tiled": ("f32", "exact", "GPU", "tiled a b c", None),
    "big_tiles": ("f32", "exact", "GPU", "big_tiles a b c", None),
    # The loop nest `sme` starts from.
    "matmul_f64": ("f64", "exact", "CPU", "matmul @(S = PlusTimes f64) a b c", None),
}

# Stages: a schedule cut off after some of its steps, `NAME_sK` for its first
# K, as the post's viewers show them. Each is read off the example, with
# calls of its schedule functions written out, so it times what the post
# shows. name: (element, its workspace, or None for a schedule with no
# scratch, its cuts).
STAGES = {
    "swapped": ("f32", None, [1]),
    "tower": ("f32", None, [1, 2, 3, 4]),
    "packed": ("f32", "Workspace", [6, 7, 8, 9]),
    "sme": ("f64", "Tiles", list(range(1, 14))),
}
EXTRA = {}


def schedule_steps(text, name):
    """`name`'s head and steps in the example, with each call of one of the
    example's schedule functions written out."""
    functions = {}
    for match in re.finditer(r"^schedule fn (\w+) (.*?) =\n((?:  .*\n)+)", text, flags=re.M):
        functions[match.group(1)] = (re.findall(r"\w+", match.group(2)), match.group(3).splitlines())
    match = re.search(rf"^(schedule {name} of [^\n]*(?:\n    [^\n]*)*=)\n((?:  [^\n]*\n)+)", text, flags=re.M)
    head, steps = match.group(1), []
    for line in match.group(2).splitlines():
        call = re.match(r"  (\w+) (.*)$", line)
        if call and call.group(1) in functions:
            parameters, body = functions[call.group(1)]
            arguments = re.findall(r"\w+", call.group(2))
            names = dict(zip(parameters, arguments))
            steps += [re.sub(r"\w+", lambda word: names.get(word.group(0), word.group(0)), step).strip()
                      for step in body]
        else:
            steps.append(line.strip())
    return head, steps


def stages():
    text = EXAMPLE.read_text()
    for name, (element, workspace, cuts) in STAGES.items():
        head, steps = schedule_steps(text, name)
        for cut in cuts:
            stage = f"{name}_s{cut}"
            taken = steps[:cut]
            # A stage takes only the scratch views its steps use.
            lines = head.split("\n")
            kept = [line for line in lines[1:]
                    if not (m := re.search(r"\(mut (\w+):", line)) or any(m.group(1) in step for step in taken)]
            first = lines[0].replace(f"schedule {name} of", f"schedule {stage} of")
            if not kept:
                first = first.rstrip(" =") + " ="
            elif not kept[-1].endswith("="):
                kept[-1] += " ="
                first = first.rstrip(" =")
            EXTRA[stage] = "\n".join([first, *kept]) + "\n" + "".join(f"  {step}\n" for step in taken)
            views = [re.search(r"\(mut (\w+):", line).group(1) for line in kept]
            where = "CPU, all cores" if any(step.startswith("parallel") and step != "parallel zi"
                                            for step in taken) else "CPU"
            if workspace and views:
                body = (f"temp w = allocate @DRAM View {workspace}\n  {stage} a b c"
                        + "".join(f" (view w.{view})" for view in views))
                if "@(S" not in first:
                    body = body.replace(f"  {stage} a b c", f"  {stage} @(S = PlusTimes {element}) a b c")
            else:
                body = f"{stage} a b c" if "@(S" in first else f"{stage} @(S = PlusTimes {element}) a b c"
            KERNELS[stage] = (element, "exact", where, body, None)
    # The tuning steps between `packed` and `fast`, in "Making it fast": its
    # larger blocks with the 4 x 8 tile, columns outside rows (`big`) and
    # then rows first (`big_order`).
    head, steps = schedule_steps(text, "fast")
    for name, order in (("big", "order (gk, gj, gi, ui, i, j)"), ("big_order", "order (gk, gi, gj, ui, i, j)")):
        taken = [order if step.startswith("order") else
                 step.replace("split j by NR", "split j by 8").replace("lanes NR", "lanes 8") for step in steps]
        EXTRA[name] = (head.replace("schedule fast of", f"schedule {name} of").replace("256, NR]", "256, 8]") + "\n"
                       + "".join(f"  {step}\n" for step in taken)
                       + "\nstruct Big = {\n  packA: [f32; 512, 256] in PanelMajor MR,\n  packB: [f32; 256, 8],\n}\n")
        KERNELS[name] = ("f32", "exact", "CPU", f"""temp w = allocate @DRAM View Big
  {name} a b c (view w.packA) (view w.packB)""", None)


stages()

# The post's attention, in `attention.cachet`, timed by `attention_harness.c`
# at its one size. name: (where, call).
ATTENTION = {
    "attend": ("CPU", "attend q kt v y"),
    "flash": ("GPU", "flash q kt v y"),
}
# name: (element, where, environment). OpenBLAS picks its `vortexm4` kernels
# on an M4, which use the matrix unit (SME); `OPENBLAS_CORETYPE=NEOVERSEN1`
# makes it use its NEON kernels, the ceiling for the CPU's vector units.
NEON = {"OPENBLAS_CORETYPE": "NEOVERSEN1"}
ONE = {"OPENBLAS_NUM_THREADS": "1", "OMP_NUM_THREADS": "1"}
CEILINGS = {
    "openblas_neon": ("f32", "CPU, all cores", NEON),
    "openblas_neon_1": ("f32", "CPU, one core", {**NEON, **ONE}),
    "openblas_sme": ("f32", "CPU matrix unit", {}),
    "accelerate": ("f32", "CPU matrix unit", {}),
    "openblas_neon_f64": ("f64", "CPU, all cores", NEON),
    "openblas_neon_f64_1": ("f64", "CPU, one core", {**NEON, **ONE}),
    "accelerate_f64": ("f64", "CPU matrix unit", {}),
    "mlx": ("f32", "GPU", {}),
    "mlx_attention": ("f32", "GPU", {}),
}
C_TYPE = {"f32": "float", "f64": "double"}


def run(command, **kwargs):
    return subprocess.run(command, capture_output=True, text=True, **kwargs)


def fail(message):
    sys.exit(f"bench: {message}")


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def source(cachet, name, n):
    """The example without its main, and one wrapper calling `name` at n."""
    element, _, _, body, scratch = KERNELS[name]
    text = EXAMPLE.read_text()
    text = text[:text.index("export fn main")] + (EXTRA[name] + "\n" if name in EXTRA else "")
    # The example's imports are relative to a file two levels inside Cachet.
    text = text.replace('import "../../', f'import "{cachet}/')
    extra = ""
    if scratch:
        shape = ", ".join(map(str, scratch(n)))
        extra = f" (mut scratch: [{element}; {shape}])"
    t = element
    # An export is emitted only if something calls it, so `main` does; the
    # harness renames `main` and never runs it.
    operands = "".join(f"  temp {x} = allocate @DRAM [{t}; {n}, {n}]\n  {x}[.., ..] = 0.0\n" for x in "abc")
    argument = ""
    if scratch:
        operands += f"  temp scratch = allocate @DRAM [{t}; {shape}]\n  scratch[.., .., ..] = 0.0\n"
        argument = " scratch"
    return (text + f"""
export fn bench_{name} (reads a: [{t}; {n}, {n}]) (reads b: [{t}; {n}, {n}]) (mut c: [{t}; {n}, {n}]){extra} =
  {body}

export fn main () =
{operands}  bench_{name} a b c{argument}
""")


def attention_source(cachet, name):
    """`attention.cachet` and one wrapper calling `name`."""
    text = (HERE / "attention.cachet").read_text().replace('import "../../', f'import "{cachet}/')
    shapes = "(reads q: [f32; 64, 16]) (reads kt: [f32; 16, 32]) (reads v: [f32; 32, 16]) (mut y: [f32; 64, 16])"
    return (text + f"""
export fn bench_{name} {shapes} =
  {ATTENTION[name][1]}

export fn main () =
  temp q = allocate @DRAM [f32; 64, 16]
  q[.., ..] = 0.0
  temp kt = allocate @DRAM [f32; 16, 32]
  kt[.., ..] = 0.0
  temp v = allocate @DRAM [f32; 32, 16]
  v[.., ..] = 0.0
  temp y = allocate @DRAM [f32; 64, 16]
  y[.., ..] = 0.0
  bench_{name} q kt v y
""")


def build_cachet(cachetc, cachet, name, n, directory, cc):
    directory.mkdir(parents=True, exist_ok=True)
    attention = name in ATTENTION
    element, check, _, _, scratch = ("f32", "close", None, None, None) if attention else KERNELS[name]
    program = directory / f"{name}.cachet"
    module = directory / f"{name}.ll"
    program.write_text(attention_source(cachet, name) if attention else source(cachet, name, n))
    environment = dict(os.environ, cachet_datadir=str(cachet))
    emitted = run([str(cachetc), "build", "--target", "llvm", "--emit-llvm", "--release", str(program),
                   "-o", str(module)], env=environment)
    (directory / f"{name}.build.log").write_text(emitted.stdout + emitted.stderr)
    if emitted.returncode != 0:
        fail(f"{name} at N={n} did not build; see {directory / (name + '.build.log')}\n"
             + (emitted.stdout + emitted.stderr)[-2000:])
    text = module.read_text()
    if text.count("define i32 @main(") != 1:
        fail(f"{name}: expected one main in {module}")
    module.write_text(text.replace("define i32 @main(", "define i32 @cachet_generated_main("))
    entries = re.findall(rf"^define void @(cachet_bench_{name}_\w*)\(", module.read_text(), flags=re.M)
    if len(entries) != 1:
        fail(f"{name}: expected one emitted wrapper, found {len(entries)}")
    image = directory / name
    defines = [f"-DKERNEL_CACHET={entries[0]}", f"-DSIZE={n}", f"-DELEM={C_TYPE[element]}", f'-DNAME="{name}"']
    if check == "exact":
        defines.append("-DCHECK_EXACT")
    if scratch:
        count = 1
        for extent in scratch(n):
            count *= extent
        defines.append(f"-DSCRATCH_BYTES={count * (4 if element == 'f32' else 8)}")
    bridge = Path(f"{module}.metal.m")
    if bridge.exists():
        defines.append("-DMETAL")
    steps = [
        [cc, *STRICT, "-Wno-override-module", "-c", "-x", "ir", str(module), "-o", str(directory / f"{name}.o")],
        [cc, *STRICT, "-c", "-x", "c", f"{module}.runtime.c", "-o", str(directory / f"{name}-runtime.o")],
        [cc, *STRICT, *defines, "-c", str(HERE / ("attention_harness.c" if attention else "harness.c")),
         "-o", str(directory / f"{name}-harness.o")],
    ]
    objects = [directory / f"{name}.o", directory / f"{name}-runtime.o", directory / f"{name}-harness.o"]
    frameworks = []
    if bridge.exists():
        steps.append([cc, "-O2", "-fobjc-arc", "-c", str(bridge), "-o", str(directory / f"{name}-metal.o")])
        objects.append(directory / f"{name}-metal.o")
        frameworks = ["-framework", "Metal", "-framework", "Foundation"]
    steps.append([cc, *map(str, objects), "-o", str(image), "-lm", "-pthread", *frameworks])
    for step in steps:
        compiled = run(step)
        if compiled.returncode != 0:
            fail(f"{name}: {shlex.join(step)} failed\n{compiled.stderr[-2000:]}")
    (directory / f"{name}.commands.txt").write_text("\n".join(shlex.join(step) for step in steps) + "\n")
    return image


def build_ceiling(name, n, directory, cc, openblas):
    directory.mkdir(parents=True, exist_ok=True)
    element = CEILINGS[name][0]
    image = directory / name
    defines = [f"-DSIZE={n}", f"-DELEM={C_TYPE[element]}", f'-DNAME="{name}"']
    if name.startswith("openblas"):
        flags = ["-DKERNEL_OPENBLAS", f"-I{openblas}/include", f"-L{openblas}/lib", "-lopenblas"]
    else:
        flags = ["-DKERNEL_ACCELERATE", "-framework", "Accelerate"]
    step = [cc, *STRICT, *defines, str(HERE / "harness.c"), "-o", str(image), *flags, "-lm"]
    compiled = run(step)
    if compiled.returncode != 0:
        fail(f"{name}: {shlex.join(step)} failed\n{compiled.stderr[-2000:]}")
    (directory / f"{name}.commands.txt").write_text(shlex.join(step) + "\n")
    return image


def measure(name, command, environment=None):
    ran = run(command, env=environment)
    rows = [line for line in ran.stdout.splitlines() if line.startswith("result ")]
    if len(rows) != 1:
        fail(f"{name} printed no result (exit {ran.returncode}):\n{ran.stdout[-1000:]}{ran.stderr[-1000:]}")
    _, label, n, median, best, gflops, reps, check = rows[0].split()
    if check == "WRONG":
        fail(f"{name} at N={n} computed a wrong result")
    return {"name": name, "n": int(n), "median_s": float(median), "min_s": float(best),
            "gflops": float(gflops), "reps": int(reps), "check": check}


def manifest(cachet, cachetc, openblas, cc, python):
    revision = run(["git", "-C", str(cachet), "rev-parse", "HEAD"]).stdout.strip()
    dirty = run(["git", "-C", str(cachet), "status", "--porcelain", "--untracked-files=no"]).stdout.strip() != ""
    site = run(["git", "-C", str(SITE), "rev-parse", "HEAD"]).stdout.strip()
    mlx = run([str(python), "-c", "import mlx.core as mx; print(mx.__version__)"]).stdout.strip()
    pkg = run(["pkg-config", "--modversion", "openblas"],
              env=dict(os.environ, PKG_CONFIG_PATH=f"{openblas}/lib/pkgconfig")).stdout.strip()
    return {
        "date": datetime.datetime.now().astimezone().isoformat(timespec="seconds"),
        "cachet_revision": revision,
        "cachet_tree_dirty": dirty,
        "cachetc_sha256": sha256(cachetc),
        "site_revision": site,
        "example_sha256": sha256(EXAMPLE),
        "harness_sha256": sha256(HERE / "harness.c"),
        "cc": run([cc, "--version"]).stdout.splitlines()[0],
        "openblas": pkg or str(openblas),
        "mlx": mlx,
        "machine": run(["sysctl", "-n", "machdep.cpu.brand_string"]).stdout.strip(),
        "cores": run(["sysctl", "-n", "hw.perflevel0.physicalcpu"]).stdout.strip() + " performance, "
                 + run(["sysctl", "-n", "hw.perflevel1.physicalcpu"]).stdout.strip() + " efficiency",
        "macos": platform.mac_ver()[0],
        "power": run(["pmset", "-g", "batt"]).stdout.splitlines()[0] if Path("/usr/bin/pmset").exists() else "",
    }


def table(results):
    lines = ["| Kernel | Runs on | Type | N | Median ms | GFLOP/s | Result |", "| --- | --- | --- | --- | --- | --- | --- |"]
    for row in results:
        where, element = row["where"], row["element"]
        lines.append(f"| {row['name']} | {where} | {element} | {row['n']} | {row['median_s'] * 1e3:.2f} | "
                     f"{row['gflops']:.1f} | {row['check']} |")
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--cachet", type=Path, required=True, help="a Cachet checkout, for its library and platforms")
    parser.add_argument("--cachetc", type=Path, help="the compiler (default: cabal list-bin exe:cachetc in --cachet)")
    parser.add_argument("--sizes", default="1024")
    parser.add_argument("--only", help="comma-separated kernels and ceilings")
    parser.add_argument("--out", type=Path)
    parser.add_argument("--openblas", type=Path, default=Path("/opt/homebrew/opt/openblas"))
    arguments = parser.parse_args()

    cachet = arguments.cachet.resolve()
    cachetc = arguments.cachetc
    if cachetc is None:
        listed = run(["cabal", "list-bin", "exe:cachetc"], cwd=cachet)
        cachetc = Path(listed.stdout.strip())
    if not cachetc.exists():
        fail(f"no compiler at {cachetc}")
    cachetc = cachetc.resolve()
    python = HERE / ".venv" / "bin" / "python"
    cc = os.environ.get("CC", "clang")
    sizes = [int(size) for size in arguments.sizes.split()]
    chosen = set(arguments.only.split(",")) if arguments.only else set(KERNELS) | set(CEILINGS) | set(ATTENTION)
    unknown = chosen - set(KERNELS) - set(CEILINGS) - set(ATTENTION)
    if unknown:
        fail(f"unknown kernels: {', '.join(sorted(unknown))}")
    stamp = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
    out = (arguments.out or HERE / "results" / stamp).resolve()
    out.mkdir(parents=True, exist_ok=True)

    images = []
    # Attention has one size of its own, whatever --sizes says.
    for name in ATTENTION:
        if name in chosen:
            print(f"building {name}", flush=True)
            images.append((name, 64, [str(build_cachet(cachetc, cachet, name, 64, out / "build" / "attention", cc))], None))
    if "mlx_attention" in chosen:
        images.append(("mlx_attention", 64, [str(python), str(HERE / "mlx_attention.py")], None))
    for n in sizes:
        for name in KERNELS:
            if name in chosen:
                print(f"building {name} at N={n}", flush=True)
                images.append((name, n, [str(build_cachet(cachetc, cachet, name, n, out / "build" / str(n), cc))], None))
        for name in CEILINGS:
            if name not in chosen or name == "mlx_attention":
                continue
            if name == "mlx":
                images.append((name, n, [str(python), str(HERE / "mlx_matmul.py"), str(n)], None))
                continue
            print(f"building {name} at N={n}", flush=True)
            image = build_ceiling(name, n, out / "build" / str(n), cc, arguments.openblas)
            images.append((name, n, [str(image)], dict(os.environ, **CEILINGS[name][2])))

    results = []
    for name, n, command, environment in images:
        print(f"timing {name} at N={n}", flush=True)
        row = measure(name, command, environment)
        if name in ATTENTION:
            element, where = "f32", ATTENTION[name][0]
        else:
            element, where = (KERNELS[name][0], KERNELS[name][2]) if name in KERNELS else CEILINGS[name][:2]
        settings = {key: value for key, value in (CEILINGS.get(name, (None, None, {}))[2]).items()}
        row.update(element=element, where=where, command=command, environment=settings)
        results.append(row)
        print(f"  {row['median_s'] * 1e3:.2f} ms, {row['gflops']:.1f} GFLOP/s, {row['check']}", flush=True)

    (out / "manifest.json").write_text(json.dumps(manifest(cachet, cachetc, arguments.openblas, cc, python), indent=2) + "\n")
    (out / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    (out / "results.md").write_text(table(results))
    print(f"wrote {out}")
    print(table(results))


if __name__ == "__main__":
    main()
