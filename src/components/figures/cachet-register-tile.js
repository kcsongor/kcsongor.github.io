import {
  configuration,
  issuedGroup,
  inFlight,
  arrival,
} from "./cachet-register-model.mjs";
import { figurePlayback, smooth, mix } from "./figure-playback.js";

class RegisterTile extends HTMLElement {
  connectedCallback() {
    if (this.player) return;
    let columns = 8,
      visual = 0,
      drag = null,
      tween = 0;
    const svg = this.querySelector("svg");
    const edge = this.querySelector("[data-edge]");
    const vectors = [...this.querySelectorAll("[data-vector]")];
    const packets = [...this.querySelectorAll("[data-packet]")];
    const cycles = [...this.querySelectorAll("[data-cycle]")];
    const counts = [...this.querySelectorAll("[data-cycle-count]")];
    const pulses = [...this.querySelectorAll("[data-arrival]")];
    const buttons = [...this.querySelectorAll("[data-width]")];
    const motion = matchMedia("(prefers-reduced-motion: reduce)");
    const paint = time => {
      const incoming = arrival(columns, time);
      const work = inFlight(columns, time);
      vectors.forEach(el => {
        const group = Number(el.dataset.group);
        el.setAttribute("aria-hidden", String(group >= columns / 4));
        const glow = incoming.group === group ? incoming.glow : 0;
        el.setAttribute("opacity", group < 2 ? 1 : 0.12 + 0.88 * visual);
        el.querySelector("[data-vector-frame]").setAttribute(
          "stroke-opacity",
          0.3 + glow * 0.7
        );
        el.querySelectorAll("[data-lane]").forEach(lane =>
          lane.setAttribute("opacity", 0.3 + glow * 0.6)
        );
      });
      packets.forEach(el => {
        const packet = work.find(
          p =>
            p.row === Number(el.dataset.row) &&
            p.group === Number(el.dataset.group)
        );
        el.setAttribute("visibility", packet ? "visible" : "hidden");
        if (packet)
          el.setAttribute(
            "transform",
            `translate(${20 + 300 * packet.progress} ${76.5 + packet.row * 39})`
          );
      });
      pulses.forEach(el => {
        el.setAttribute("opacity", incoming.glow * 0.7);
        el.setAttribute("r", 3 + incoming.glow * 5);
      });
      cycles.forEach((el, i) => {
        el.setAttribute(
          "stroke",
          Math.floor(time) % 4 === i ? "currentColor" : "none"
        );
        counts[i].textContent = issuedGroup(columns, i) === null ? "–" : "4";
      });
      this.querySelector("[data-issue]").textContent =
        issuedGroup(columns, Math.floor(time)) === null
          ? "Waiting for the earlier totals…"
          : "Four updates can start.";
      edge.setAttribute("transform", `translate(${174 + 156 * visual} 55)`);
    };
    this.player = figurePlayback(this, {
      start: 0.35,
      end: 8.35,
      advance: (p, ms) => p + ms / 1150,
      step: p => Math.floor(p) + 1,
      paint,
    });
    const settle = value => {
      cancelAnimationFrame(tween);
      const from = visual,
        started = performance.now();
      const tick = now => {
        const t = motion.matches ? 1 : Math.min(1, (now - started) / 280);
        visual = mix(from, value, smooth(t));
        this.player.redraw();
        if (t < 1) tween = requestAnimationFrame(tick);
      };
      tween = requestAnimationFrame(tick);
    };
    const select = width => {
      if (columns !== width) {
        columns = width;
        this.player.reset(0.35, 8.35);
        const config = configuration(columns);
        this.querySelector("[data-size]").textContent = `4 × ${columns}`;
        this.querySelector("[data-vectors]").textContent =
          `${config.vectors} independent vectors`;
        this.querySelector("[data-explanation]").textContent =
          columns === 8
            ? "Eight vectors give us two cycles of work, then two cycles waiting for the first results. Widening the tile gives us more independent totals to update in the meantime."
            : "By the time we have started all sixteen updates, the first four results are ready. We can return to those totals without leaving a gap.";
        edge.setAttribute("aria-valuenow", columns);
        edge.setAttribute(
          "aria-valuetext",
          `${columns} columns, ${config.vectors} independent vectors`
        );
        buttons.forEach(button =>
          button.setAttribute(
            "aria-pressed",
            String(Number(button.dataset.width) === width)
          )
        );
      }
      settle(columns === 16 ? 1 : 0);
    };
    buttons.forEach(button => {
      button.disabled = false;
      button.onclick = () => select(Number(button.dataset.width));
    });
    edge.onkeydown = event => {
      if (
        [
          "ArrowLeft",
          "ArrowRight",
          "ArrowUp",
          "ArrowDown",
          "Home",
          "End",
        ].includes(event.key)
      ) {
        event.preventDefault();
        select(["ArrowRight", "ArrowUp", "End"].includes(event.key) ? 16 : 8);
      } else if (event.key === "Escape" && drag) cancel();
    };
    edge.onpointerdown = event => {
      if (event.button !== 0) return;
      event.preventDefault();
      this.player.stop();
      cancelAnimationFrame(tween);
      edge.focus({ preventScroll: true });
      drag = {
        id: event.pointerId,
        x: event.clientX,
        initial: visual,
        scale: svg.getBoundingClientRect().width / svg.viewBox.baseVal.width,
      };
      edge.setPointerCapture(event.pointerId);
    };
    edge.onpointermove = event => {
      if (!drag || drag.id !== event.pointerId) return;
      visual = Math.max(
        0,
        Math.min(
          1,
          drag.initial + (event.clientX - drag.x) / (156 * drag.scale)
        )
      );
      this.player.redraw();
    };
    edge.onpointerup = event => {
      if (!drag || drag.id !== event.pointerId) return;
      drag = null;
      select(visual >= 0.5 ? 16 : 8);
      edge.releasePointerCapture(event.pointerId);
    };
    const cancel = () => {
      drag = null;
      settle(columns === 16 ? 1 : 0);
    };
    edge.onpointercancel = cancel;
    edge.onlostpointercapture = () => {
      if (drag) cancel();
    };
    this.resize = new ResizeObserver(([entry]) => {
      const compact = entry.contentRect.width < 580;
      this.dataset.compact = String(compact);
      svg.setAttribute("viewBox", compact ? "0 0 355 610" : "0 0 690 320");
      this.querySelector("[data-pipelines]").setAttribute(
        "transform",
        compact ? "translate(0 295)" : "translate(350 0)"
      );
    });
    this.resize.observe(this);
    this.cancelTween = () => cancelAnimationFrame(tween);
  }
  disconnectedCallback() {
    this.player?.destroy();
    this.player = null;
    this.resize?.disconnect();
    this.cancelTween?.();
  }
}
if (!customElements.get("cachet-register-tile"))
  customElements.define("cachet-register-tile", RegisterTile);
