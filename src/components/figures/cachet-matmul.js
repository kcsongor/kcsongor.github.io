import { MatrixMotion, MOVE, FLIGHT, LAND } from "./matrix-motion.js";
import { productsFor, stepCount, scalarHistory } from "./matmul-model.mjs";
const colours = [
  "var(--matrix-first)",
  "var(--matrix-second)",
  "var(--matrix-third)",
];
const idle = "var(--matrix-idle)";
const tint = (colour, amount) =>
  `color-mix(in srgb, ${colour} ${amount}%, ${idle})`;
class MatmulFigure extends HTMLElement {
  connectedCallback() {
    this.dims = {
      m: Number(this.dataset.m),
      k: Number(this.dataset.kSize),
      n: Number(this.dataset.n),
    };
    this.scalar = this.tagName === "CACHET-MATMUL";
    this.mode = this.scalar ? "scalar" : this.dataset.mode || "outputs";
    this.progress = new Map();
    this.index = 0;
    this.output = this.scalar
      ? Number(this.dataset.i) * this.dims.n + Number(this.dataset.j)
      : 0;
    this.cells = Object.fromEntries(
      ["a", "b", "c"].map(m => [
        m,
        [...this.querySelectorAll(`[data-matrix=${m}] > [data-row]`)],
      ])
    );
    this.motion = new MatrixMotion(this.querySelector(".matmul-layout"), () => {
      this.commit();
      if (this.pending) {
        const action = this.pending;
        this.pending = null;
        action();
      } else if (this.playing) this.advance(1);
      this.controls();
    });
    this.playing = false;
    this.querySelectorAll("[data-play], [data-step]").forEach(button => {
      button.disabled = false;
    });
    this.querySelector("[data-play]").onclick = () => {
      this.playing = !this.playing;
      if (this.playing && this.motion.time >= this.motion.frame.duration)
        this.advance(1);
      else this.motion.play(this.playing);
      this.controls();
    };
    this.querySelectorAll("[data-step]").forEach(
      button =>
        (button.onclick = () => {
          this.playing = false;
          const delta = Number(button.dataset.step);
          // A paused figure starts before its first product. The first Next
          // plays that product; later presses move on from the current one.
          if (delta > 0 && this.motion.time === 0) {
            this.motion.play(true);
            this.controls();
          } else this.request(() => this.advance(delta));
        })
    );
    this.querySelectorAll("[data-mode-button]").forEach(
      button =>
        (button.onclick = () => {
          this.pending = null;
          this.playing = false;
          this.mode = button.dataset.modeButton;
          this.index = 0;
          this.querySelectorAll("[data-mode-button]").forEach(b =>
            b.setAttribute("aria-pressed", String(b === button))
          );
          this.render();
        })
    );
    this.cells.c.forEach((cell, index) => {
      if (cell.tagName === "BUTTON")
        cell.onclick = () => {
          this.playing = false;
          this.request(() => {
            this.output = index;
            this.index = Math.min(
              this.progress.get(index) || 0,
              this.dims.k - 1
            );
            this.render();
          });
        };
    });
    this.render(this.motion.reduced);
    this.motion.play(false);
    this.controls();
  }
  disconnectedCallback() {
    this.motion.destroy();
  }
  controls() {
    const button = this.querySelector("[data-play]");
    button.textContent = this.playing ? "Pause" : "Play";
    button.setAttribute("aria-pressed", String(this.playing));
    this.querySelector("[data-playback-status]").textContent = this.playing
      ? "Playing through the multiplication"
      : this.motion.running
        ? "Playing one step"
        : this.motion.time === 0
          ? "Press Play, or advance one step."
          : "Paused · accumulated outputs stay in place";
  }
  cell(matrix, r, c) {
    return this.cells[matrix][
      r * (matrix === "a" ? this.dims.k : this.dims.n) + c
    ];
  }
  commit() {
    if (
      this.scalar &&
      (this.motion.reduced || this.motion.time >= this.motion.frame.duration)
    )
      this.progress.set(
        this.output,
        Math.max(this.progress.get(this.output) || 0, this.index + 1)
      );
  }
  request(action) {
    if (
      !this.motion.reduced &&
      this.motion.time > 0 &&
      this.motion.time < this.motion.frame.duration
    ) {
      this.pending = action;
      this.motion.play(true);
    } else {
      this.commit();
      action();
    }
    this.controls();
  }
  advance(delta) {
    this.commit();
    const count = stepCount(this.mode, this.dims.k);
    if (this.scalar) {
      const total = this.dims.m * this.dims.n * count,
        flat = (this.output * count + this.index + delta + total) % total;
      if (
        delta > 0 &&
        this.progress.size === this.dims.m * this.dims.n &&
        [...this.progress.values()].every(value => value === count)
      ) {
        this.progress.clear();
        this.output = 0;
        this.index = 0;
      } else {
        this.output = Math.floor(flat / count);
        this.index = flat % count;
        if (delta < 0) this.progress.set(this.output, this.index);
        else
          this.index = Math.min(
            Math.max(this.index, this.progress.get(this.output) || 0),
            count - 1
          );
      }
    } else this.index = (this.index + delta + count) % count;
    this.render();
  }
  render(immediate = false) {
    const { m, k, n } = this.dims,
      s = this.index,
      mode = this.mode;
    const inputs = new Map(
      [...this.cells.a, ...this.cells.b].map(cell => [cell, idle])
    );
    const outputs = new Map(
      this.cells.c.map(cell => [cell, { before: idle, after: idle }])
    );
    const before = new Map(),
      after = new Map();
    const add = (map, product) => {
      const target = this.cell("c", product.i, product.j),
        values = map.get(target) || [0, 0, 0];
      values[product.group]++;
      map.set(target, values);
    };
    const history = this.scalar
      ? scalarHistory(this.progress, this.dims, this.output, s)
      : Array.from({ length: s }, (_, t) =>
          productsFor(mode, t, this.dims, this.output)
        ).flat();
    history.forEach(p => {
      add(before, p);
      add(after, p);
      inputs.set(this.cell("a", p.i, p.k), tint(colours[p.group], 28));
      inputs.set(this.cell("b", p.k, p.j), tint(colours[p.group], 28));
    });
    const finished = this.scalar && (this.progress.get(this.output) || 0) >= k;
    const current = finished
      ? []
      : productsFor(mode, s, this.dims, this.output);
    current.forEach(p => add(after, p));
    const paint = values => {
      if (!values) return idle;
      const total = values.reduce((a, b) => a + b, 0);
      // Separate partial sums stay visibly separate until their combine step.
      if (mode === "split" && s < 2)
        return `linear-gradient(to right, ${values.map((v, q) => `${tint(colours[q], (v / 2) * 85)} ${(q / 3) * 100}% ${((q + 1) / 3) * 100}%`).join(",")})`;
      const mixed =
        mode === "split"
          ? "var(--matrix-sum)"
          : `color-mix(in srgb, ${colours[0]} ${(values[0] / total) * 100}%, ${colours[1]})`;
      return tint(mixed, (total / k) * 90);
    };
    for (const cell of this.cells.c)
      outputs.set(cell, {
        before: paint(before.get(cell)),
        after: paint(after.get(cell)),
      });
    const focus = {};
    if (mode === "scalar") {
      const i = Math.floor(this.output / n),
        j = this.output % n;
      for (let h = 0; h < k; h++) {
        inputs.set(this.cell("a", i, h), tint(colours[h < k / 2 ? 0 : 1], 20));
        inputs.set(this.cell("b", h, j), tint(colours[h < k / 2 ? 0 : 1], 20));
      }
      focus.a = [this.cell("a", i, s)];
      focus.b = [this.cell("b", s, j)];
      focus.c = [this.cell("c", i, j)];
      this.cells.c.forEach((cell, index) =>
        cell.setAttribute("aria-pressed", String(index === this.output))
      );
      this.querySelector("[data-formula]").textContent =
        `C[${i}, ${j}] ← C[${i}, ${j}] + A[${i}, ${s}] × B[${s}, ${j}]`;
      this.querySelector("[data-output-label]").textContent = `C[${i}, ${j}]`;
      this.querySelector("[data-output-sources]").textContent =
        `Row ${i} of A\ncolumn ${j} of B`;
      this.querySelector("[data-count]").textContent = finished
        ? "Complete · accumulated outputs stay in place"
        : `Product ${s + 1} of ${k} · completed outputs stay in place`;
    } else if (mode === "outputs") {
      focus.a = Array.from({ length: m }, (_, i) => this.cell("a", i, s));
      focus.b = Array.from({ length: n }, (_, j) => this.cell("b", s, j));
      this.cells.c.forEach((cell, q) => (focus[`c${q}`] = [cell]));
    } else if (mode === "split") {
      for (let group = 0; group < 3; group++) {
        focus[`a${group}`] = [this.cell("a", 1, group * 2 + Math.min(s, 1))];
        focus[`b${group}`] = [this.cell("b", group * 2 + Math.min(s, 1), 3)];
      }
      focus.c = [this.cell("c", 1, 3)];
    } else {
      for (let i = 0; i < 2; i++)
        for (let j = 0; j < 2; j++) {
          const rows = [i * 2, i * 2 + 1],
            cols = j ? [3, 4] : [0, 1, 2],
            ks = [s * 3, s * 3 + 1, s * 3 + 2];
          focus[`a${i}`] = rows.flatMap(r => ks.map(h => this.cell("a", r, h)));
          focus[`b${j}`] = ks.flatMap(h => cols.map(c => this.cell("b", h, c)));
          focus[`c${i}${j}`] = rows.flatMap(r =>
            cols.map(c => this.cell("c", r, c))
          );
        }
    }
    current.forEach(p => {
      inputs.set(this.cell("a", p.i, p.k), tint(colours[p.group], 75));
      inputs.set(this.cell("b", p.k, p.j), tint(colours[p.group], 75));
    });
    let products = current.map(p => ({
      sources: [this.cell("a", p.i, p.k), this.cell("b", p.k, p.j)],
      target: this.cell("c", p.i, p.j),
      colour: colours[p.group],
      delay: p.delay,
    }));
    if (mode === "split" && s === 2) {
      const target = this.cell("c", 1, 3);
      outputs.set(target, {
        before: `linear-gradient(to right, ${colours.map((c, q) => `${tint(c, 85)} ${(q / 3) * 100}% ${((q + 1) / 3) * 100}%`).join(",")})`,
        after: tint("var(--matrix-sum)", 90),
      });
      // The partial sums are already in the selected cell: merge the stripes
      // locally, without pretending to reread A and B.
      products = [{ sources: [], target, colour: colours[2], delay: 0 }];
    }
    this.dataset.mode = mode;
    this.dataset.frame = String(s);
    const caption = this.querySelector("[data-caption]");
    if (caption) {
      this.querySelector("[data-work-value]").textContent =
        mode === "split"
          ? "3 threads"
          : mode === "blocks"
            ? "4 groups"
            : `${m * n} threads`;
      this.querySelector("[data-work-unit]").textContent =
        mode === "split"
          ? "one output, three partial sums"
          : mode === "blocks"
            ? "one group per output block"
            : "one thread per output";
      caption.textContent =
        mode === "outputs"
          ? `Product ${s + 1} of ${k} · column ${s} of A and row ${s} of B feed all ${m * n} outputs.`
          : mode === "blocks"
            ? `Block product ${s + 1} of 2 · keep the outlined inputs through all three inner products.`
            : s < 2
              ? `Step ${s + 1} of 3 · three partial sums for C[1, 3]. Each stripe fills as its pair arrives.`
              : "Step 3 of 3 · add the three partial sums. All six products are already computed.";
    }
    this.querySelectorAll("[data-axis]").forEach(
      label =>
        (label.dataset.active = String(
          label.dataset.axis === "k" && Number(label.dataset.index) === s
        ))
    );
    this.querySelector("[data-matrix=a]").setAttribute(
      "aria-label",
      `A: inputs for ${mode === "scalar" ? "output " + Math.floor(this.output / n) + ", " + (this.output % n) : mode}, step ${s + 1}`
    );
    this.querySelector("[data-matrix=b]").setAttribute(
      "aria-label",
      `B: inputs for step ${s + 1}`
    );
    this.motion.transition(
      {
        followSources: mode === "blocks",
        inputs,
        outputs,
        focus,
        products,
        duration: MOVE + FLIGHT + LAND + (mode === "blocks" ? 440 : 0) + 550,
      },
      immediate
    );
    this.controls();
  }
}
for (const name of ["cachet-matmul", "cachet-matmul-schedules"])
  if (!customElements.get(name))
    customElements.define(name, class extends MatmulFigure {});
