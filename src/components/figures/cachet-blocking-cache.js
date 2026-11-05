import {
  trace,
  frame,
  lineFor,
  parseLine,
  advance,
  reuseExcerpt,
} from "./cachet-memory-model.mjs";
import {
  figurePlayback,
  smooth,
  mix,
  curvePoint,
  curvePath,
} from "./figure-playback.js";

const runs = { rows: trace("rows"), blocked: trace("blocked") };
const origins = { A: [22, 192], B: [184, 35], C: [184, 192] };
const colour = array => `var(--hw-${array.toLowerCase()})`;
const name = line => {
  if (!line) return "empty";
  const { array, row, col } = parseLine(line);
  return `${array}[${row}, ${col}–${col + 3}]`;
};
const at = (array, step) => {
  const row = array === "B" ? step.k : step.i;
  const col = array === "A" ? step.k : step.j;
  return [
    origins[array][0] + col * 14 + 5.5,
    origins[array][1] + row * 14 + 5.5,
  ];
};

class BlockingCache extends HTMLElement {
  connectedCallback() {
    if (this.player) return;
    let mode = "blocked",
      compact = false;
    let excerpt = reuseExcerpt(mode);
    const svg = this.querySelector("svg");
    const cells = [...this.querySelectorAll("[data-cell]")];
    const lines = [...this.querySelectorAll("[data-line]")];
    const slots = [...this.querySelectorAll("[data-slot]")];
    const blocks = [...this.querySelectorAll("[data-block]")];
    const focuses = [...this.querySelectorAll("[data-focus]")];
    const buttons = [...this.querySelectorAll("[data-mode]")];
    const route = this.querySelector("[data-route]");
    const packet = this.querySelector("[data-value-packet]");
    const fill = this.querySelector("[data-fill-packet]");
    const paint = position => {
      const finished = position >= excerpt.end;
      const f = frame(runs[mode], Math.min(position, excerpt.end - 1e-7));
      const { step, access, stage, local, cache } = f;
      const previous = runs[mode].steps[Math.max(excerpt.start, f.index - 1)];
      cells.forEach(el => {
        const { array, row, col } = el.dataset;
        const cached =
          array !== "C" &&
          cache.includes(lineFor(array, Number(row), Number(col)));
        el.setAttribute(
          "opacity",
          array === "C"
            ? 0.07 + (f.output[Number(row) * 8 + Number(col)] / 8) * 0.8
            : cached
              ? 0.5
              : 0.09
        );
      });
      lines.forEach(el => {
        const { array, row, col } = el.dataset;
        el.setAttribute(
          "stroke-opacity",
          cache.includes(lineFor(array, Number(row), Number(col))) ? 0.4 : 0.06
        );
      });
      blocks.forEach(el => {
        const array = el.dataset.array;
        const r = Math.floor((array === "B" ? step.k : step.i) / 4) * 4;
        const c = Math.floor((array === "A" ? step.k : step.j) / 4) * 4;
        el.setAttribute("x", origins[array][0] + c * 14 - 4);
        el.setAttribute("y", origins[array][1] + r * 14 - 4);
        el.setAttribute(
          "visibility",
          mode === "blocked" ? "visible" : "hidden"
        );
      });
      focuses.forEach(el => {
        const array = el.dataset.array,
          old = at(array, previous),
          next = at(array, step);
        const t = smooth((position - Math.floor(position)) / 0.15);
        el.setAttribute("x", mix(old[0], next[0], finished ? 1 : t) - 6.5);
        el.setAttribute("y", mix(old[1], next[1], finished ? 1 : t) - 6.5);
        el.setAttribute(
          "opacity",
          array === "C" || array === access.array ? 1 : 0.3
        );
      });
      slots.forEach((el, s) => {
        const line = cache[s],
          ink = line ? colour(parseLine(line).array) : "var(--hw-muted)";
        el.style.color = ink;
        el.querySelector("[data-slot-label]").textContent = name(line);
        el.querySelector("[data-slot-frame]").setAttribute(
          "stroke",
          s === access.slot && stage < 2 ? ink : "var(--border)"
        );
        el.querySelectorAll("[data-slot-word]").forEach(word =>
          word.setAttribute("opacity", line ? 0.65 : 0.12)
        );
      });
      this.querySelector("[data-memory-line]").textContent = name(access.line);
      this.querySelector("[data-memory-line]").style.fill = colour(
        access.array
      );
      this.querySelectorAll("[data-memory-word]").forEach(el => {
        el.style.fill = colour(access.array);
        el.setAttribute("opacity", !access.hit && stage < 2 ? 0.75 : 0.18);
      });
      this.querySelector("[data-transfer]").textContent = finished
        ? "These partial totals stay in C."
        : stage === 2
          ? "Both inputs have reached the core."
          : access.hit
            ? "Cache hit: use the copy already here."
            : access.repeated
              ? "This line was evicted. Fetch it again."
              : "Cache miss: fetch all four neighbours.";
      this.querySelector("[data-operation]").textContent =
        `C[${step.i},${step.j}] += A[${step.i},${step.k}] × B[${step.k},${step.j}]`;
      this.querySelector("[data-progress]").textContent =
        `${Math.max(0, f.completed - excerpt.start)} of 16 products`;
      const hwX = compact ? 0 : 360,
        hwY = compact ? 375 : 0;
      const slotX = hwX + 20 + (access.slot % 2) * 156,
        slotY = hwY + 148 + Math.floor(access.slot / 2) * 29;
      let curve,
        t,
        isLine = false,
        ink = colour(access.array);
      if (stage < 2 && !access.hit && local < 0.58) {
        curve = [
          [hwX + 170, hwY + 75],
          [hwX + 170, hwY + 112],
          [slotX + 72, slotY - 30],
          [slotX + 72, slotY + 11.5],
        ];
        t = smooth(local / 0.58);
        isLine = true;
      } else if (stage < 2) {
        curve = [
          [slotX + 72, slotY + 23],
          [slotX + 72, hwY + 278],
          [hwX + 170, hwY + 270],
          [hwX + 170, hwY + 297],
        ];
        t = smooth(access.hit ? local : (local - 0.58) / 0.42);
      } else {
        const destination = at("C", step),
          source = [hwX + 20, hwY + 320];
        curve = compact
          ? [source, [2, source[1]], [2, destination[1]], destination]
          : [
              source,
              [hwX - 40, source[1]],
              [destination[0] + 55, destination[1]],
              destination,
            ];
        t = smooth(local / 0.93);
        ink = colour("C");
      }
      route.setAttribute("d", curvePath(curve));
      route.style.stroke = ink;
      route.setAttribute("visibility", finished ? "hidden" : "visible");
      for (const [el, show] of [
        [fill, isLine],
        [packet, !isLine],
      ]) {
        el.setAttribute(
          "visibility",
          show && !finished && (stage < 2 || local < 0.93)
            ? "visible"
            : "hidden"
        );
        el.setAttribute(
          "transform",
          `translate(${curvePoint(curve, t).join(" ")})`
        );
        el.style.fill = ink;
      }
    };
    this.player = figurePlayback(this, {
      ...excerpt,
      advance: (position, ms) => advance(runs[mode], position, ms),
      step: p => Math.floor(p) + 1,
      paint,
    });
    buttons.forEach(button => {
      button.disabled = false;
      button.onclick = () => {
        if (mode === button.dataset.mode) return;
        mode = button.dataset.mode;
        excerpt = reuseExcerpt(mode);
        buttons.forEach(b =>
          b.setAttribute("aria-pressed", String(b === button))
        );
        this.player.reset(excerpt.start, excerpt.end);
        this.querySelector("[data-explanation]").textContent =
          mode === "blocked"
            ? "The first row has received its contributions from these blocks. The four lines of B are still in cache: each can now help compute the next row."
            : "The first row of C is complete, but the walk through all of B has evicted its earlier lines. As we begin the next row, we have to fetch them again.";
      };
    });
    this.resize = new ResizeObserver(([entry]) => {
      if (!entry.contentRect.width) return;
      compact = entry.contentRect.width < 580;
      this.dataset.compact = String(compact);
      svg.setAttribute("viewBox", compact ? "0 0 340 725" : "0 0 700 365");
      this.querySelector("[data-memory]").setAttribute(
        "transform",
        compact ? "translate(0 375)" : "translate(360 0)"
      );
      this.player.redraw();
    });
    this.resize.observe(this);
  }
  disconnectedCallback() {
    this.player?.destroy();
    this.player = null;
    this.resize?.disconnect();
  }
}
if (!customElements.get("cachet-blocking-cache"))
  customElements.define("cachet-blocking-cache", BlockingCache);
