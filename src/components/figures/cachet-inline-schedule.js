const stepMotion = { duration: 1200, easing: "ease-in-out", fill: "both" };

const comparable = line => {
  const text = line.textContent.trim();
  return text && text !== "..." ? text : null;
};

const pairsFor = (before, after) => {
  const available = new Map();
  for (const line of before) {
    const key = comparable(line);
    if (key) available.set(key, [...(available.get(key) ?? []), line]);
  }
  return new Map(
    after.map(line => [line, available.get(comparable(line))?.shift()])
  );
};

// Measure the first token, rather than the line box, to animate indentation
// as well as vertical movement. This also works with highlighted token spans.
const textLeft = line => {
  const walker = document.createTreeWalker(line, NodeFilter.SHOW_TEXT);
  let text;
  while ((text = walker.nextNode())) {
    const offset = text.textContent.search(/\S/);
    if (offset < 0) continue;
    const range = document.createRange();
    range.setStart(text, offset);
    range.setEnd(text, offset + 1);
    return range.getBoundingClientRect().left;
  }
  return line.getBoundingClientRect().left;
};

const inStages = (list, stage) => list.split(" ").includes(String(stage));

// A run of stages shares one source listing, each line listing the stages
// that show it, whose rewrite it is and where it is yet to come.
const showSource = (source, stage) => {
  for (const line of source.querySelectorAll(".line[data-shown]")) {
    const future = inStages(line.dataset.futureIn, stage);
    line.hidden = !inStages(line.dataset.shown, stage);
    line.toggleAttribute("data-future", future);
    line.classList.toggle(
      "is-changed",
      inStages(line.dataset.currentIn, stage) && !future
    );
  }
};

class CachetInlineSchedule extends HTMLElement {
  connectedCallback() {
    this.output = this.querySelector("[data-output]");
    this.stages = [...this.querySelectorAll("[data-stage]")];
    this.sources = [...this.querySelectorAll("[data-source-stages]")];
    this.previous = this.querySelector('[data-step="-1"]');
    this.next = this.querySelector('[data-step="1"]');
    this.position = this.querySelector("[data-position]");
    this.count = this.output.children.length;
    this.current = Number(this.dataset.initialStage);
    this.motion = matchMedia("(prefers-reduced-motion: reduce)");
    this.events = new AbortController();
    for (const button of [this.previous, this.next]) {
      button.addEventListener(
        "click",
        () => this.go(Number(button.dataset.step)),
        {
          signal: this.events.signal,
        }
      );
    }
    this.motion.addEventListener("change", () => this.finish(), {
      signal: this.events.signal,
    });
    this.querySelector("nav").hidden = false;
    this.controls();
    // All excerpts share one grid cell, reserving the largest height. A
    // viewport or font change cancels motion whose old coordinates are stale.
    this.observer = new ResizeObserver(() => this.finish());
    this.observer.observe(this.output);
  }

  disconnectedCallback() {
    this.events?.abort();
    this.observer?.disconnect();
    this.finish();
  }

  controls() {
    if (!this.previous) return;
    this.previous.disabled = !!this.run || this.current === 0;
    this.next.disabled = !!this.run || this.current === this.count - 1;
    this.position.textContent =
      this.current === 0 ? "Start" : `${this.current} / ${this.count - 1}`;
  }

  finish() {
    this.run = null;
    for (const animation of this.animations ?? []) animation.cancel();
    this.animations = [];
    this.overlay?.remove();
    this.overlay = null;
    if (this.target) this.target.style.opacity = "";
    this.controls();
  }

  async go(delta) {
    if (this.run) return;
    const next = Math.max(0, Math.min(this.count - 1, this.current + delta));
    if (next === this.current) return;
    this.finish();
    const before = this.output.querySelector(".is-current pre");
    const beforeBar = this.querySelector(".is-current [data-timing-fill]");
    this.current = next;
    for (const stage of this.stages) {
      const active = Number(stage.dataset.stage) === next;
      stage.classList.toggle("is-current", active);
      stage.setAttribute("aria-hidden", String(!active));
      stage.inert = !active;
    }
    for (const source of this.sources) {
      const active = inStages(source.dataset.sourceStages, next);
      source.classList.toggle("is-current", active);
      source.setAttribute("aria-hidden", String(!active));
      source.inert = !active;
      if (active) showSource(source, next);
    }
    this.controls();
    if (this.motion.matches || !Element.prototype.animate) return;

    const after = this.output.querySelector(".is-current pre");
    this.target = after;
    const run = {};
    this.run = run;
    this.controls();
    try {
      this.animateRewrite(before, after);
      this.animateBar(
        beforeBar,
        this.querySelector(".is-current [data-timing-fill]")
      );
      await Promise.allSettled(
        this.animations.map(animation => animation.finished)
      );
    } finally {
      if (this.run === run) this.finish();
    }
  }

  animateBar(before, after) {
    if (
      !before ||
      !after ||
      before.dataset.target !== after.dataset.target ||
      before.dataset.scale !== after.dataset.scale
    )
      return;
    const from = Number(before.dataset.width);
    const to = Number(after.dataset.width);
    if (!Number.isFinite(from) || !Number.isFinite(to) || from === to) return;
    // Each stage retains its final CSS width. Animate the newly visible bar
    // from the preceding stage, then cancel back to that final width on finish.
    this.animations.push(
      after.animate([{ width: `${from}%` }, { width: `${to}%` }], stepMotion)
    );
  }

  animateRewrite(before, after) {
    const oldLines = [...before.querySelectorAll(".line")];
    const newLines = [...after.querySelectorAll(".line")];
    const pairs = pairsFor(oldLines, newLines);
    const start = before.getBoundingClientRect();
    const end = after.getBoundingClientRect();
    const overlay = after.cloneNode(false);
    overlay.classList.add("rewrite-overlay");
    overlay.setAttribute("aria-hidden", "true");
    overlay.tabIndex = -1;
    overlay.inert = true;
    const code = document.createElement("code");
    overlay.append(code);
    this.output.append(overlay);
    this.overlay = overlay;

    const add = (line, base, keyframes, kind) => {
      const rect = line.getBoundingClientRect();
      const clone = line.cloneNode(true);
      clone.classList.add(kind);
      Object.assign(clone.style, {
        position: "absolute",
        top: `${rect.top - base.top}px`,
        left: `${rect.left - base.left}px`,
        width: `${rect.width}px`,
        boxSizing: "border-box",
      });
      code.append(clone);
      this.animations.push(clone.animate(keyframes, stepMotion));
    };

    const matched = new Set([...pairs.values()].filter(Boolean));
    for (const line of oldLines) {
      if (matched.has(line)) continue;
      add(
        line,
        start,
        [
          { opacity: 1, offset: 0 },
          { opacity: 0, offset: 0.3 },
          { opacity: 0, offset: 1 },
        ],
        "rewrite-line-removed"
      );
    }
    for (const line of newLines) {
      const source = pairs.get(line);
      if (source) {
        const dx = textLeft(source) - start.left - (textLeft(line) - end.left);
        const dy =
          source.getBoundingClientRect().top -
          start.top -
          (line.getBoundingClientRect().top - end.top);
        const from = `translate(${dx}px, ${dy}px)`;
        add(
          line,
          end,
          [
            { transform: from, offset: 0 },
            { transform: from, offset: 0.1 },
            { transform: "translate(0, 0)", offset: 0.9 },
            { transform: "translate(0, 0)", offset: 1 },
          ],
          dx || dy ? "rewrite-line-moved" : "rewrite-line-unchanged"
        );
      } else {
        add(
          line,
          end,
          [
            { opacity: 0, offset: 0 },
            { opacity: 0, offset: 0.4 },
            { opacity: 1, offset: 0.9 },
            { opacity: 1, offset: 1 },
          ],
          "rewrite-line-added"
        );
      }
    }
    after.style.opacity = "0";
  }
}

if (!customElements.get("cachet-inline-schedule"))
  customElements.define("cachet-inline-schedule", CachetInlineSchedule);
