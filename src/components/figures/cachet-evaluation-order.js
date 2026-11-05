const TICK = 1900;
const steps = {
  0: "Six exactly representable values. Their sum over the reals is 1. Each + will round to f32.",
  1: "The grouped tree adds −2²⁵ and 1. This rounds back to −2²⁵, losing the 1. Its three pairs can run independently.",
  2: "The chain cancels 2²⁵ with −2²⁵ first. Its running sum is now 0, and it hasn't added the 1 yet.",
  3: "The chain adds 1 to 0. The grouped tree finishes at 0: the 1 was lost in its first addition.",
  4: "Only zeroes remain. The chain keeps its 1.",
  5: "The chain gives 1; the grouped sum gives 0. Same inputs, five additions each, different rounding. These are dependency steps, not measured timings.",
};
const LAST = 5;

class EvaluationOrder extends HTMLElement {
  connectedCallback() {
    this.step = 0;
    this.timer = 0;
    this.results = [...this.querySelectorAll("[data-result]")];
    this.parts = [...this.querySelectorAll("[data-step]")];
    this.caption = this.querySelector("[data-caption]");
    this.button = this.querySelector("[data-play]");
    this.querySelector("[data-controls]").hidden = false;
    this.button.onclick = () => {
      this.held = !!this.timer;
      if (this.timer) this.pause();
      else {
        if (this.step === LAST) this.step = 0;
        this.play();
      }
    };
    this.querySelectorAll("[data-move]").forEach(
      button =>
        (button.onclick = () => {
          this.held = true;
          this.pause();
          this.move(Number(button.dataset.move));
        })
    );
    const reduced = matchMedia("(prefers-reduced-motion: reduce)").matches;
    new IntersectionObserver(
      ([entry]) => {
        if (!entry.isIntersecting) this.pause();
        else if (!reduced && !this.held) this.play();
      },
      { threshold: 0.3 }
    ).observe(this);
    this.render();
  }
  move(delta) {
    const next = this.step + delta;
    this.step = (next + LAST + 1) % (LAST + 1);
    this.render();
  }
  play() {
    if (this.timer) return;
    this.timer = setInterval(() => {
      this.move(1);
      if (this.step === LAST) {
        this.held = true;
        this.pause();
      }
    }, TICK);
    this.render();
  }
  pause() {
    clearInterval(this.timer);
    this.timer = 0;
    this.render();
  }
  render() {
    this.button.textContent = this.timer
      ? "Pause"
      : this.step === LAST
        ? "Replay"
        : "Play";
    this.results.forEach(result => {
      result.toggleAttribute(
        "data-pending",
        this.step < Number(result.dataset.completeAt)
      );
    });
    this.parts.forEach(part => {
      const step = Number(part.dataset.step);
      part.dataset.state =
        step === this.step ? "current" : step < this.step ? "done" : "idle";
    });
    this.caption.textContent = steps[this.step];
  }
}
if (!customElements.get("cachet-evaluation-order"))
  customElements.define("cachet-evaluation-order", EvaluationOrder);
