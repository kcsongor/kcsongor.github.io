import { MatrixMotion, MOVE, FLIGHT, LAND } from "./matrix-motion.js";
const idle = "var(--matrix-idle)",
  blue = "var(--matrix-first)",
  pink = "var(--matrix-second)";
const tint = (c, p) => `color-mix(in srgb, ${c} ${p}%, ${idle})`;
class TileParts extends HTMLElement {
  connectedCallback() {
    this.completed = new Set();
    this.motion = new MatrixMotion(
      this.querySelector(".operand-layout"),
      () => {
        this.completed.add(this.selectedPart);
        if (this.pending) {
          const action = this.pending;
          this.pending = null;
          action();
        }
      }
    );
    const buttons = [...this.querySelectorAll("[data-part]")];
    const select = (part, immediate = false) => {
      this.selectedPart = part;
      const row = Math.floor(part / 2),
        col = part % 2,
        inputs = new Map(),
        outputs = new Map(),
        focus = {};
      let sources, target;
      this.querySelectorAll("[data-operand]").forEach(operand => {
        const name = operand.dataset.operand,
          index = name === "x" ? row : name === "y" ? col : part;
        const cells = [...operand.querySelectorAll("[data-block]")];
        focus[name] = [cells[index]];
        if (name === "acc") {
          target = cells[index];
          cells.forEach(cell =>
            outputs.set(cell, {
              before: this.completed.has(Number(cell.dataset.block))
                ? tint("var(--matrix-sum)", 85)
                : idle,
              after:
                cell === target ||
                this.completed.has(Number(cell.dataset.block))
                  ? tint("var(--matrix-sum)", 85)
                  : idle,
            })
          );
        } else {
          cells.forEach((cell, q) =>
            inputs.set(
              cell,
              q === index ? tint(name === "x" ? blue : pink, 75) : idle
            )
          );
          sources = [...(sources || []), cells[index]];
        }
        operand.setAttribute(
          "aria-label",
          `${name}: block ${index} belongs to SIMD group ${row}, ${col}`
        );
      });
      buttons.forEach(button =>
        button.setAttribute(
          "aria-pressed",
          String(Number(button.dataset.part) === part)
        )
      );
      this.querySelector("[data-part-detail]").textContent =
        `SIMD group (${row}, ${col}) holds rows ${row * 8}–${row * 8 + 7} of x, columns ${col * 8}–${col * 8 + 7} of y, and the ${row ? "bottom" : "top"}-${col ? "right" : "left"} 8 × 8 part of acc.`;
      this.motion.transition(
        {
          inputs,
          outputs,
          focus,
          products: this.completed.has(part)
            ? []
            : [
                { sources: [sources[0]], target, colour: blue, delay: 0 },
                { sources: [sources[1]], target, colour: pink, delay: 0 },
              ],
          duration: MOVE + FLIGHT + LAND + 250,
        },
        immediate
      );
    };
    buttons.forEach(button => {
      button.disabled = false;
      button.onclick = () => {
        const action = () => select(Number(button.dataset.part));
        if (
          !this.motion.reduced &&
          this.motion.time < this.motion.frame.duration
        ) {
          this.pending = action;
          this.motion.play(true);
        } else {
          this.completed.add(this.selectedPart);
          action();
        }
      };
    });
    select(0, this.motion.reduced);
  }
  disconnectedCallback() {
    this.motion.destroy();
  }
}
class Phases extends HTMLElement {
  connectedCallback() {
    this.phase = 0;
    this.tile = 0;
    this.retained = idle;
    this.next = this.querySelector("[data-next]");
    this.previous = this.querySelector("[data-previous]");
    this.motion = new MatrixMotion(this.querySelector(".phase-tiles"), () => {
      this.next.disabled = false;
      if (this.phase === 2) this.retained = this.tileResult();
    });
    this.next.onclick = () => {
      if (this.phase === 3) {
        this.tile = (this.tile + 1) % 2;
        if (this.tile === 0) this.retained = idle;
      }
      this.phase = (this.phase + 1) % 4;
      this.render();
    };
    this.previous.onclick = () => {
      this.phase = Math.max(0, this.phase - 1);
      this.render();
    };
    this.render(true);
  }
  disconnectedCallback() {
    this.motion.destroy();
  }
  tileResult() {
    return this.tile === 0 ? tint(blue, 45) : tint("var(--matrix-sum)", 85);
  }
  render(immediate = false) {
    const phase = this.phase,
      inputs = new Map(),
      outputs = new Map(),
      focus = {},
      products = [];
    const target = this.querySelector("[data-acc]");
    const cell = (buffer, r, c) =>
      this.querySelector(
        `[data-buffer=${buffer}] [data-row="${r}"][data-col="${c}"]`
      );
    this.querySelectorAll("[data-phase]").forEach(step => {
      if (Number(step.dataset.phase) === phase)
        step.setAttribute("aria-current", "step");
      else step.removeAttribute("aria-current");
    });
    this.querySelectorAll("[data-buffer]").forEach(buffer => {
      const name = buffer.dataset.buffer;
      buffer.querySelectorAll("[data-row]").forEach(el => {
        const r = Number(el.dataset.row),
          c = Number(el.dataset.col),
          own = r === 1 && c === 2,
          read = name === "ta" ? r === 1 : c === 2;
        inputs.set(
          el,
          phase >= 2 && read
            ? tint(name === "ta" ? blue : pink, 50)
            : own && phase < 2
              ? tint(blue, 75)
              : idle
        );
        el.textContent = phase < 2 && own ? "W" : phase >= 2 && read ? "R" : "";
      });
      buffer.setAttribute(
        "aria-label",
        `${name}: ${phase >= 2 ? "all cells readable; no writes" : "only thread 1, 2 may write its cell"}`
      );
      focus[name] =
        phase >= 2
          ? Array.from({ length: 4 }, (_, h) =>
              name === "ta" ? cell(name, 1, h) : cell(name, h, 2)
            )
          : [cell(name, 1, 2)];
    });
    focus.acc = [target];
    // Keep acc across the second barrier and the next tile's fill.
    outputs.set(target, {
      before: this.retained,
      after: phase === 2 ? this.tileResult() : this.retained,
    });
    if (phase === 2)
      for (let h = 0; h < 4; h++)
        products.push({
          sources: [cell("ta", 1, h), cell("tb", h, 2)],
          target,
          colour: this.tile === 0 ? blue : pink,
          delay: h * 260,
        });
    const descriptions = [
      "Each thread fills its own cells. This thread writes ta[1, 2] and tb[1, 2]; it cannot read its neighbours yet.",
      "First barrier: wait until every thread has finished filling its cells. Then both buffers become shared for reading.",
      "Read row 1 of ta and column 2 of tb. Four pairs stream into this thread’s private acc. No thread may write the buffers during this phase.",
      "Second barrier: wait until every thread has finished reading. Only then may the buffers be overwritten for the next tile; acc is retained.",
    ];
    this.querySelector("[data-description]").textContent =
      `Input tile ${this.tile + 1} of 2. ${descriptions[phase]}`;
    if (phase === 2 && this.motion.reduced) this.retained = this.tileResult();
    this.previous.disabled = phase === 0;
    this.next.disabled = phase === 2 && !this.motion.reduced;
    this.next.textContent =
      phase === 3
        ? this.tile === 0
          ? "Next tile →"
          : "Restart multiplication ↻"
        : "Next phase →";
    this.motion.transition(
      {
        inputs,
        outputs,
        focus,
        products,
        followSources: phase === 2,
        duration: MOVE + FLIGHT + LAND + (phase === 2 ? 780 : 0) + 200,
      },
      immediate
    );
  }
}
if (!customElements.get("cachet-tile-parts"))
  customElements.define("cachet-tile-parts", TileParts);
if (!customElements.get("cachet-phases"))
  customElements.define("cachet-phases", Phases);
