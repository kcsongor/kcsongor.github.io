// All stages remain readable without scripts. Navigation and linked code
// inspection are enhancements; neither changes the captured compiler output.
class CachetSchedule extends HTMLElement {
  connectedCallback() {
    if (this.dataset.ready !== undefined) return;
    this.stages = [...this.querySelectorAll("[data-stage]")];
    this.tabs = [...this.querySelectorAll("[data-goto]")];
    this.picker = this.querySelector("[data-stage-picker]");
    this.previous = this.querySelector('[data-step="-1"]');
    this.next = this.querySelector('[data-step="1"]');
    this.position = this.querySelector("[data-position]");
    this.selection = this.querySelector("[data-selection]");
    this.clear = this.querySelector("[data-clear]");
    this.pinned = null;
    this.current = 0;

    this.dataset.ready = "";
    this.querySelector("[data-controls]").hidden = false;
    this.querySelector("[data-inspection]").hidden = false;
    this.tabs.forEach(tab => {
      tab.onclick = () => this.show(Number(tab.dataset.goto), true);
    });
    if (this.picker)
      this.picker.onchange = () => this.show(Number(this.picker.value), true);
    [this.previous, this.next].forEach(button => {
      button.onclick = () =>
        this.show(this.current + Number(button.dataset.step), true);
    });
    this.querySelector("[data-wrap-control]").onchange = event => {
      this.dataset.wrap = String(event.target.checked);
    };
    const expand = this.querySelector("[data-expand]");
    expand.onclick = () => {
      const expanded = this.dataset.expanded !== "true";
      this.dataset.expanded = String(expanded);
      expand.setAttribute("aria-pressed", String(expanded));
      expand.textContent = expanded ? "Compact code" : "Expand code";
    };
    this.querySelectorAll("[data-copy]").forEach(button => {
      button.hidden = !navigator.clipboard;
      button.disabled = false;
      button.onclick = async () => {
        const code = button.closest(".schedule-pane").querySelector("pre code");
        try {
          await navigator.clipboard.writeText(code.textContent);
          button.textContent = "Copied";
        } catch {
          button.textContent = "Couldn't copy";
        }
        setTimeout(() => {
          button.textContent = "Copy";
        }, 1500);
      };
    });

    const stepOf = target => target?.closest?.("[data-link]") ?? null;
    const focused = () => stepOf(document.activeElement);
    this.addEventListener("pointerover", event => {
      const step = stepOf(event.target);
      this.link(step || this.pinned || focused());
    });
    this.addEventListener("pointerout", event => {
      this.link(stepOf(event.relatedTarget) || this.pinned || focused());
    });
    this.addEventListener("focusin", event => {
      const step = stepOf(event.target);
      this.link(step || this.pinned, !!step);
    });
    this.addEventListener("focusout", event => {
      this.link(stepOf(event.relatedTarget) || this.pinned);
    });
    this.addEventListener("click", event => {
      const step = stepOf(event.target);
      if (step) this.pin(step);
    });
    this.addEventListener("keydown", event => {
      const step = stepOf(event.target);
      if (step && (event.key === "Enter" || event.key === " ")) {
        event.preventDefault();
        this.pin(step);
      } else if (event.key === "Escape") {
        this.pin(null);
      }
    });
    this.clear.onclick = () => {
      const step = this.pinned;
      // Keep keyboard focus in the listing when the Clear button disappears.
      step?.focus({ preventScroll: true });
      this.pin(null);
    };
    this.show(Number(this.dataset.initialStage) || 0);
  }
  pin(step) {
    this.pinned = step && step !== this.pinned ? step : null;
    this.querySelectorAll("[data-link]").forEach(line => {
      line.setAttribute("aria-pressed", String(line === this.pinned));
    });
    this.clear.hidden = !this.pinned;
    this.selection.textContent = this.pinned
      ? `Showing code linked to ${this.pinned.textContent.trim()}. Select it again to clear.`
      : "Select a schedule step to keep its generated lines highlighted.";
    this.link(this.pinned, !!this.pinned);
  }
  link(step, reveal = false) {
    this.querySelectorAll(".is-linked").forEach(line =>
      line.classList.remove("is-linked")
    );
    this.querySelectorAll(".has-link").forEach(panes =>
      panes.classList.remove("has-link")
    );
    const stage = this.stages[this.current];
    if (!step || step.closest("[data-stage]") !== stage) return;
    step.classList.add("is-linked");
    step.closest(".schedule-panes").classList.add("has-link");
    const lines = stage.querySelectorAll(
      `[data-links~="${step.dataset.link}"]`
    );
    lines.forEach(line => line.classList.add("is-linked"));
    // Hover previews never move the code. Selection and keyboard focus may
    // reveal linked lines, but only inside the code pane, never on the page.
    if (reveal) this.reveal(lines[0]);
  }
  reveal(line) {
    const pane = line?.closest("pre");
    if (!pane) return;
    const box = pane.getBoundingClientRect();
    const target = line.getBoundingClientRect();
    if (target.top < box.top || target.bottom > box.bottom)
      pane.scrollTop += target.top - box.top - 24;
  }
  show(index, reveal = false) {
    this.pin(null);
    this.current = Math.max(
      0,
      Math.min(this.stages.length - 1, Number(index) || 0)
    );
    this.stages.forEach((stage, at) => {
      stage.classList.toggle("is-current", at === this.current);
    });
    this.tabs.forEach((tab, at) => {
      tab.setAttribute("aria-pressed", String(at === this.current));
    });
    if (this.picker) this.picker.value = String(this.current);
    this.position.textContent =
      this.current === 0
        ? `Start · ${this.stages.length - 1} steps`
        : `Step ${this.current} of ${this.stages.length - 1}`;
    this.previous.disabled = this.current === 0;
    this.next.disabled = this.current === this.stages.length - 1;
    if (reveal)
      this.stages[this.current]
        .querySelectorAll(".schedule-pane")
        .forEach(pane => {
          this.reveal(pane.querySelector(".is-changed"));
        });
  }
}
if (!customElements.get("cachet-schedule"))
  customElements.define("cachet-schedule", CachetSchedule);
