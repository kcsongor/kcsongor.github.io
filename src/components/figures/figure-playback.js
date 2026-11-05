// A finite, reader-started demonstration. Nothing advances in a hidden view,
// off screen, or in a background tab; a completed demonstration stays put.
export function figurePlayback(root, { start, end, advance, step, paint }) {
  let position = start,
    running = false,
    visible = false,
    request = 0,
    previous = 0;
  const play = root.querySelector("[data-hw-play]");
  const next = root.querySelector("[data-hw-step]");
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  const controller = new AbortController();
  const draw = () => {
    paint(position);
    play.textContent = running ? "Pause" : position >= end ? "Replay" : "Play";
    play.setAttribute("aria-pressed", String(running));
    next.disabled = position >= end;
  };
  const eligible = () =>
    running && visible && !document.hidden && !root.closest("[hidden]");
  const schedule = () => {
    if (!request && eligible()) request = requestAnimationFrame(tick);
  };
  const tick = now => {
    request = 0;
    if (!root.isConnected || !eligible()) {
      previous = 0;
      return;
    }
    if (previous)
      position = Math.min(end, advance(position, Math.min(now - previous, 80)));
    previous = now;
    if (position >= end) running = false;
    draw();
    schedule();
  };
  const stop = () => {
    running = false;
    previous = 0;
    cancelAnimationFrame(request);
    request = 0;
    draw();
  };
  play.addEventListener(
    "click",
    () => {
      if (position >= end) position = start;
      running = !running;
      previous = 0;
      draw();
      schedule();
    },
    { signal: controller.signal }
  );
  next.addEventListener(
    "click",
    () => {
      running = false;
      position = Math.min(end, step(position));
      previous = 0;
      draw();
    },
    { signal: controller.signal }
  );
  const visibility = () => {
    previous = 0;
    schedule();
  };
  document.addEventListener("visibilitychange", visibility, {
    signal: controller.signal,
  });
  motion.addEventListener(
    "change",
    () => {
      if (motion.matches) stop();
    },
    { signal: controller.signal }
  );
  const observer = new IntersectionObserver(([entry]) => {
    visible = entry.isIntersecting;
    visibility();
  });
  observer.observe(root);
  play.disabled = next.disabled = false;
  draw();
  return {
    stop,
    reset(from, to) {
      running = false;
      start = position = from;
      end = to;
      previous = 0;
      draw();
    },
    redraw: draw,
    destroy() {
      cancelAnimationFrame(request);
      observer.disconnect();
      controller.abort();
    },
  };
}

export const smooth = x => {
  const t = Math.max(0, Math.min(1, x));
  return t * t * (3 - 2 * t);
};
export const mix = (a, b, t) => a + (b - a) * t;
export const curvePoint = ([a, b, c, d], t) =>
  [0, 1].map(
    n =>
      (1 - t) ** 3 * a[n] +
      3 * (1 - t) ** 2 * t * b[n] +
      3 * (1 - t) * t * t * c[n] +
      t ** 3 * d[n]
  );
export const curvePath = ([a, b, c, d]) => `M${a} C${b} ${c} ${d}`;
