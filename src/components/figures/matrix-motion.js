// Shared screen-space animation for the flat matrix figures. A product owns
// two operand streams but contributes to its output only once, on arrival.
export const MOVE = 320;
export const FLIGHT = 650;
export const LAND = 140;
export const ease = x => {
  const p = Math.max(0, Math.min(1, x));
  return p * p * (3 - 2 * p);
};
export const landed = (time, delay = 0) =>
  ease((time - MOVE - delay - FLIGHT) / LAND);
// Rings settle with a slight overshoot, so a move reads as deliberate.
const settle = x => {
  const p = Math.max(0, Math.min(1, x)) - 1;
  return 1 + p * p * (2.2 * p + 1.2);
};
// A landing ripple outlives the flight by this long.
const RIPPLE = 420;
// A point a fraction p along a cubic Bézier route.
const along = (route, end, p) => {
  const q = 1 - p,
    a = route.start,
    c = route.c1,
    d = route.c2;
  return {
    x: q * q * q * a.x + 3 * q * q * p * c.x + 3 * q * p * p * d.x + p * p * p * end.x,
    y: q * q * q * a.y + 3 * q * q * p * c.y + 3 * q * p * p * d.y + p * p * p * end.y,
  };
};
const NS = "http://www.w3.org/2000/svg";
const svg = (name, attrs = {}) => {
  const el = document.createElementNS(NS, name);
  for (const [key, value] of Object.entries(attrs))
    el.setAttribute(key, String(value));
  return el;
};
export class MatrixMotion {
  constructor(host, complete = () => {}) {
    this.host = host;
    this.complete = complete;
    this.time = 0;
    this.running = false;
    this.reduced = matchMedia("(prefers-reduced-motion: reduce)").matches;
    this.layer = svg("svg", { class: "matrix-motion", "aria-hidden": "true" });
    host.append(this.layer);
    this.rings = new Map();
    this.fills = new Map();
    this.observer = new IntersectionObserver(([entry]) => {
      this.visible = entry.isIntersecting;
      this.previous = 0;
      this.wake();
    });
    this.observer.observe(host);
    this.resize = new ResizeObserver(() => {
      this.measure();
      this.draw();
    });
    this.resize.observe(host);
    this.visibility = () => {
      this.previous = 0;
      this.wake();
    };
    document.addEventListener("visibilitychange", this.visibility);
  }
  rect(elements) {
    const base = this.host.getBoundingClientRect(),
      boxes = elements.map(el => el.getBoundingClientRect());
    const x = Math.min(...boxes.map(r => r.left)) - base.left - 2;
    const y = Math.min(...boxes.map(r => r.top)) - base.top - 2;
    return {
      x,
      y,
      width: Math.max(...boxes.map(r => r.right)) - base.left - x + 2,
      height: Math.max(...boxes.map(r => r.bottom)) - base.top - y + 2,
    };
  }
  center(el) {
    const r = this.rect([el]);
    return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
  }
  measure() {
    this.layer.setAttribute(
      "viewBox",
      `0 0 ${this.host.clientWidth} ${this.host.clientHeight}`
    );
    if (!this.frame) return;
    for (const [key, elements] of Object.entries(this.frame.focus)) {
      const ring = this.rings.get(key);
      if (ring) ring.to = this.rect(elements);
    }
    for (const event of this.events) {
      event.end = this.center(event.target);
      event.paths.forEach(route => {
        route.start = this.center(route.source);
        const a = route.start,
          b = event.end;
        const lift = Math.min(36, Math.hypot(b.x - a.x, b.y - a.y) * 0.18);
        route.c1 = { x: a.x + (b.x - a.x) * 0.3, y: a.y - lift };
        route.c2 = { x: b.x - (b.x - a.x) * 0.2, y: b.y - lift };
        const d = `M${a.x},${a.y} C${route.c1.x},${route.c1.y} ${route.c2.x},${route.c2.y} ${b.x},${b.y}`;
        for (const path of [route.path, route.glow, route.core])
          path.setAttribute("d", d);
        route.length = route.path.getTotalLength();
      });
      event.box = this.rect([event.target]);
    }
  }
  transition(frame, immediate = false) {
    this.frame = frame;
    this.time = immediate ? frame.duration : 0;
    this.previous = 0;
    this.fills.forEach((fill, cell) => {
      fill.remove();
      cell.style.background = "";
    });
    this.fills.clear();
    for (const [cell, colour] of frame.inputs) cell.style.background = colour;
    for (const [cell, paint] of frame.outputs) {
      cell.style.background = paint.before;
      const fill = document.createElement("span");
      fill.className = "matrix-arrival";
      fill.setAttribute("aria-hidden", "true");
      fill.style.background = paint.after;
      cell.append(fill);
      this.fills.set(cell, fill);
    }
    this.layer.querySelectorAll(".matrix-route").forEach(el => el.remove());
    this.events = frame.products.map(product => ({
      ...product,
      // Each operand travels as a comet: a faint guide along the whole route,
      // a soft glow and a bright core behind a round head.
      paths: product.sources.map(source => {
        const group = svg("g", {
          class: "matrix-route",
          fill: product.colour,
          stroke: product.colour,
          "stroke-linecap": "round",
        });
        const path = svg("path", {
          fill: "none",
          "stroke-width": 1,
          opacity: 0.1,
        });
        const glow = svg("path", {
          fill: "none",
          "stroke-width": 7,
          opacity: 0.16,
        });
        const core = svg("path", {
          fill: "none",
          "stroke-width": 2.2,
          opacity: 0.85,
        });
        const halo = svg("circle", { r: 7, stroke: "none", opacity: 0.2 });
        const head = svg("circle", { r: 3, stroke: "none" });
        group.append(path, glow, core, halo, head);
        this.layer.append(group);
        return { source, group, path, glow, core, halo, head };
      }),
      ripple: (() => {
        const ripple = svg("rect", {
          class: "matrix-route",
          fill: "none",
          stroke: product.colour,
          "stroke-width": 2,
          rx: 5,
          opacity: 0,
        });
        this.layer.append(ripple);
        return ripple;
      })(),
    }));
    this.scans = [];
    if (frame.followSources && frame.products.length) {
      const delays = [...new Set(frame.products.map(p => p.delay))].sort(
        (a, b) => a - b
      );
      for (let source = 0; source < 2; source++) {
        const steps = delays
          .map(delay => ({
            delay,
            elements: frame.products
              .filter(p => p.delay === delay)
              .map(p => p.sources[source])
              .filter(Boolean),
          }))
          .filter(step => step.elements.length);
        if (steps.length) {
          const key = `scan${source}`;
          frame.focus[key] = steps[0].elements;
          this.scans.push({ key, steps });
        }
      }
    }
    for (const [key, ring] of this.rings) {
      ring.el.style.opacity = key in frame.focus ? "1" : "0";
    }
    for (const [key, elements] of Object.entries(frame.focus)) {
      const to = this.rect(elements);
      let ring = this.rings.get(key);
      if (!ring) {
        const el = svg("rect", {
          class: "matrix-focus",
          fill: "none",
          stroke: "var(--foreground)",
          "stroke-width": 1.5,
          "stroke-opacity": 0.7,
          rx: 4,
        });
        this.layer.append(el);
        ring = { el, current: to };
        this.rings.set(key, ring);
      }
      ring.from = { ...ring.current };
      ring.to = to;
      ring.el.style.opacity = "1";
    }
    this.measure();
    this.running = !immediate;
    this.draw();
    this.wake();
  }
  draw() {
    if (!this.frame) return;
    const motion = this.reduced ? 1 : settle(this.time / MOVE);
    for (const ring of this.rings.values()) {
      if (!ring.to) continue;
      for (const key of ["x", "y", "width", "height"]) {
        ring.current[key] =
          ring.from[key] + (ring.to[key] - ring.from[key]) * motion;
        ring.el.setAttribute(key, ring.current[key]);
      }
    }
    for (const scan of this.scans || []) {
      const ring = this.rings.get(scan.key),
        clock = this.time - MOVE + 160;
      let index = scan.steps.findLastIndex(step => clock >= step.delay);
      if (index <= 0) continue;
      const from = this.rect(scan.steps[index - 1].elements),
        to = this.rect(scan.steps[index].elements);
      const blend = this.reduced
        ? 1
        : ease((clock - scan.steps[index].delay) / 160);
      for (const key of ["x", "y", "width", "height"]) {
        ring.current[key] = from[key] + (to[key] - from[key]) * blend;
        ring.el.setAttribute(key, ring.current[key]);
      }
    }
    for (const [cell, fill] of this.fills) {
      const events = this.events.filter(event => event.target === cell);
      const progress = events.length
        ? events.reduce(
            (sum, event) => sum + landed(this.time, event.delay),
            0
          ) / events.length
        : 1;
      fill.style.opacity = String(this.reduced ? 1 : progress);
      cell.dataset.arrived = String(progress);
    }
    for (const event of this.events) {
      const age = this.time - MOVE - event.delay;
      for (const route of event.paths) {
        route.group.style.opacity =
          this.reduced || age < 0 || age > FLIGHT + LAND
            ? "0"
            : String(Math.min(1, age / 80, (FLIGHT + LAND - age) / LAND));
        if (!route.length) continue;
        const p = ease(age / FLIGHT),
          head = along(route, event.end, p);
        for (const dot of [route.halo, route.head]) {
          dot.setAttribute("cx", head.x);
          dot.setAttribute("cy", head.y);
        }
        // The tail grows as the head leaves its source, so it never pokes
        // out behind it; the route fades out once the head lands.
        const reach = p * route.length;
        for (const [path, share] of [
          [route.glow, 0.5],
          [route.core, 0.3],
        ]) {
          const tail = Math.min(reach, route.length * share, 48);
          path.setAttribute(
            "stroke-dasharray",
            `${tail} ${route.length + tail}`
          );
          path.setAttribute("stroke-dashoffset", tail - reach);
        }
      }
      // A ripple spreads from the output as the product lands in it.
      const since = age - FLIGHT;
      if (this.reduced || since < 0 || since > RIPPLE || !event.box) {
        event.ripple.setAttribute("opacity", 0);
        continue;
      }
      const spread = ease(since / RIPPLE),
        grow = 1 + spread * 6;
      event.ripple.setAttribute("x", event.box.x - grow);
      event.ripple.setAttribute("y", event.box.y - grow);
      event.ripple.setAttribute("width", event.box.width + 2 * grow);
      event.ripple.setAttribute("height", event.box.height + 2 * grow);
      event.ripple.setAttribute("opacity", 0.7 * (1 - spread));
    }
  }
  play(value) {
    this.running = value;
    this.previous = 0;
    this.wake();
  }
  wake() {
    if (this.raf || !this.running || !this.visible || document.hidden) return;
    this.raf = requestAnimationFrame(now => {
      this.raf = 0;
      if (!this.host.isConnected) {
        this.destroy();
        return;
      }
      if (!this.running || !this.visible || document.hidden) {
        this.previous = 0;
        return;
      }
      if (this.previous) this.time += Math.min(now - this.previous, 60);
      this.previous = now;
      this.draw();
      if (this.time >= this.frame.duration) {
        this.running = false;
        this.complete();
      }
      this.wake();
    });
  }
  destroy() {
    cancelAnimationFrame(this.raf);
    this.observer.disconnect();
    this.resize.disconnect();
    document.removeEventListener("visibilitychange", this.visibility);
    this.layer.remove();
  }
}
