import {
  cacheModel as model,
  lineOf,
  readsOf,
  totalTime,
} from "./cache-model.mjs";

const TICK = 70;

class CacheLines extends HTMLElement {
  connectedCallback() {
    this.walks = [...this.querySelectorAll("[data-walk]")].map(element => {
      const reads = readsOf(model, element.dataset.walk);
      return {
        reads,
        total: totalTime(reads),
        cells: [...element.querySelectorAll("[data-cell]")],
        status: element.querySelector("[data-status]"),
        focus: element.querySelector(".cache-focus"),
      };
    });
    this.end = Math.max(...this.walks.map(walk => walk.total));
    this.time = 0;
    this.timer = 0;
    this.held = true;
    this.button = this.querySelector("[data-play]");
    this.clock = this.querySelector("[data-clock]");
    this.button.disabled = false;
    this.querySelector("[data-controls]").hidden = false;
    this.button.onclick = () => {
      this.held = !!this.timer;
      if (this.timer) this.pause();
      else this.play();
    };
    // Playback is explicit; leaving the figure pauses the shared clock.
    this.observer = new IntersectionObserver(([entry]) => {
      if (!entry.isIntersecting) this.pause();
      else if (!this.held && this.time < this.end) this.play();
    });
    this.observer.observe(this);
    this.resize = new ResizeObserver(() => this.render());
    this.resize.observe(this);
    this.render();
  }
  disconnectedCallback() {
    clearInterval(this.timer);
    this.observer.disconnect();
    this.resize.disconnect();
  }
  play() {
    if (this.timer) return;
    if (this.time >= this.end) this.time = 0;
    this.timer = setInterval(() => {
      this.time += 1;
      if (this.time >= this.end) this.pause();
      this.render();
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
      : this.time >= this.end
        ? "Replay"
        : "Play";
    this.button.setAttribute("aria-pressed", String(!!this.timer));
    this.clock.textContent = `Time ${this.time} · ${this.timer ? "both walks share this clock" : this.time >= this.end ? "both walks complete" : "paused"}`;
    this.walks.forEach(walk => this.renderWalk(walk));
  }
  renderWalk({ reads, total, cells, status, focus }) {
    // The read in progress is the one whose cost the clock hasn't passed yet.
    let index = 0;
    let started = 0;
    while (index < reads.length && started + reads[index].cost <= this.time)
      started += reads[index++].cost;
    const read = reads[index];
    const done = reads.slice(0, index);
    const cached = done.at(-1)?.cached ?? [];
    const misses = done.filter(({ miss }) => miss).length;
    const fetching = read?.miss && this.time > 0;
    const missed = `${misses} ${misses === 1 ? "miss" : "misses"}`;
    const selected = read && cells[read.row * model.cols + read.col];
    focus.hidden = !selected;
    if (selected) {
      focus.style.transform = `translate(${selected.offsetLeft}px, ${selected.offsetTop}px)`;
      focus.style.width = `${selected.offsetWidth}px`;
      focus.style.height = `${selected.offsetHeight}px`;
    }
    cells.forEach((cell, n) => {
      const row = Math.floor(n / model.cols);
      const line = lineOf(model, row, n % model.cols);
      const current = read && read.row === row && read.col === n % model.cols;
      cell.dataset.state =
        fetching && line === read.line
          ? current
            ? "stalled"
            : "fetching"
          : current && this.time > 0
            ? "current"
            : line === read?.line || cached.includes(line)
              ? "cached"
              : "idle";
    });
    status.textContent = read
      ? `${index} of ${reads.length} read, ${missed}${fetching ? " · waiting for memory" : ""}`
      : `All ${reads.length} read at time ${total}, ${missed}`;
  }
}
customElements.define("cachet-cache-lines", CacheLines);
