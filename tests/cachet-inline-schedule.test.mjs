import test, { after } from "node:test";
import assert from "node:assert/strict";

// Exercise navigation and animation lifetimes without starting a browser.
const registry = new Map();
const saved = Object.fromEntries(
  ["HTMLElement", "Element", "customElements"].map((key) => [key, globalThis[key]]),
);
globalThis.HTMLElement = class {};
globalThis.Element = class {
  animate() {}
};
globalThis.customElements = {
  get: (name) => registry.get(name),
  define: (name, element) => registry.set(name, element),
};
await import("../src/components/figures/cachet-inline-schedule.js");
after(() => {
  for (const [key, value] of Object.entries(saved)) {
    if (value === undefined) delete globalThis[key];
    else globalThis[key] = value;
  }
});
const Schedule = registry.get("cachet-inline-schedule");

function animation() {
  const { promise, resolve, reject } = Promise.withResolvers();
  return {
    finished: promise,
    complete: resolve,
    cancelled: false,
    cancel() {
      this.cancelled = true;
      reject(new Error("cancelled"));
    },
  };
}

function fixture() {
  const calls = [];
  const viewer = new Schedule();
  const stages = [20, 60].map((width, index) => ({
    dataset: { stage: String(index) },
    classList: { toggle() {} },
    attributes: {},
    setAttribute(key, value) { this.attributes[key] = value; },
    pre: { style: {} },
    bar: {
      dataset: { width: String(width), target: "one_core", scale: "110" },
      animate(frames, options) {
        const motion = animation();
        calls.push({ frames, options, motion });
        return motion;
      },
    },
  }));
  Object.assign(viewer, {
    current: 0,
    count: stages.length,
    stages,
    sources: [],
    previous: {},
    next: {},
    position: {},
    motion: { matches: false },
    output: {
      querySelector: () => stages[viewer.current].pre,
    },
    querySelector: () => stages[viewer.current].bar,
    // The rewrite's DOM measurements are independent of the timing bars.
    animateRewrite() {},
  });
  return { viewer, stages, calls };
}

test("bars grow from the previous value and shrink when navigating backwards", async () => {
  const { viewer, stages, calls } = fixture();
  const forward = viewer.go(1);
  assert.deepEqual(calls[0].frames, [{ width: "20%" }, { width: "60%" }]);
  assert.equal(calls[0].options.duration, 1200);
  assert.equal(viewer.current, 1);
  assert.equal(stages[0].inert, true);
  assert.equal(stages[1].attributes["aria-hidden"], "false");
  assert.equal(viewer.previous.disabled, true);
  assert.equal(viewer.next.disabled, true);
  await viewer.go(-1);
  assert.equal(viewer.current, 1, "navigation waits for the current transition");

  calls[0].motion.complete();
  await forward;
  assert.equal(viewer.previous.disabled, false);
  assert.equal(viewer.next.disabled, true);
  assert.equal(calls[0].motion.cancelled, true, "completion restores the final CSS width");

  const backward = viewer.go(-1);
  assert.deepEqual(calls[1].frames, [{ width: "60%" }, { width: "20%" }]);
  calls[1].motion.complete();
  await backward;
  assert.equal(viewer.current, 0);
  assert.equal(viewer.previous.disabled, true);
  assert.equal(viewer.next.disabled, false);
});

test("a change of target or scale does not interpolate unrelated percentages", async () => {
  for (const [key, value] of [["target", "gpu"], ["scale", "200"], ["width", "20"]]) {
    const { viewer, stages, calls } = fixture();
    stages[1].bar.dataset[key] = value;
    await viewer.go(1);
    assert.equal(calls.length, 0, key);
    assert.equal(viewer.current, 1);
    assert.equal(viewer.run, null);
  }
});

test("reduced motion changes the selected step immediately", async () => {
  const { viewer, calls } = fixture();
  viewer.motion.matches = true;
  viewer.animateRewrite = () => assert.fail("reduced motion must skip rewrites too");
  await viewer.go(1);
  assert.equal(calls.length, 0);
  assert.equal(viewer.current, 1);
  assert.equal(viewer.previous.disabled, false);
});

test("cancelling a transition cannot cancel a newer transition", async () => {
  const { viewer, calls } = fixture();
  const forward = viewer.go(1);
  // Resize, a motion-preference change, and disconnect all call finish().
  viewer.finish();
  assert.equal(viewer.current, 1);
  assert.equal(calls[0].motion.cancelled, true);
  const backward = viewer.go(-1);
  const currentRun = viewer.run;
  await forward;
  assert.equal(viewer.run, currentRun);
  assert.equal(calls[1].motion.cancelled, false);
  calls[1].motion.complete();
  await backward;
  assert.equal(viewer.current, 0);
  assert.equal(viewer.run, null);
});

test("bar and code animations finish together, including on disconnect", async () => {
  const { viewer, calls } = fixture();
  const rewrite = animation();
  viewer.animateRewrite = () => viewer.animations.push(rewrite);
  const forward = viewer.go(1);
  calls[0].motion.complete();
  await Promise.resolve();
  assert.ok(viewer.run, "a finished bar must not end a rewrite still in progress");
  viewer.disconnectedCallback();
  await forward;
  assert.equal(rewrite.cancelled, true);
  assert.equal(calls[0].motion.cancelled, true);
  assert.equal(viewer.run, null);
  assert.equal(viewer.current, 1);
});

test("a run of stages sharing one source listing shows each stage's lines", async () => {
  const { viewer } = fixture();
  viewer.motion = { matches: true };
  const line = (shown, current, future) => ({
    dataset: { shown, currentIn: current, futureIn: future },
    hidden: false,
    attributes: new Set(),
    classes: new Set(),
    toggleAttribute(name, on) { on ? this.attributes.add(name) : this.attributes.delete(name); },
    classList: { toggle(name, on) { on ? line.owner.classes.add(name) : line.owner.classes.delete(name); } },
  });
  const lines = [line("0 1", "", ""), line("0 1", "1", "0"), line("0", "", "")];
  for (const each of lines) each.classList.toggle = (name, on) => (on ? each.classes.add(name) : each.classes.delete(name));
  const source = {
    dataset: { sourceStages: "0 1" },
    classList: { toggle() {} },
    setAttribute() {},
    querySelectorAll: () => lines,
  };
  viewer.sources = [source];
  await viewer.go(1);
  assert.deepEqual(lines.map(each => each.hidden), [false, false, true]);
  assert.deepEqual(lines.map(each => each.classes.has("is-changed")), [false, true, false]);
  assert.deepEqual(lines.map(each => each.attributes.has("data-future")), [false, false, false]);
  await viewer.go(-1);
  assert.deepEqual(lines.map(each => each.hidden), [false, false, false]);
  assert.deepEqual(lines.map(each => each.classes.has("is-changed")), [false, false, false]);
  assert.deepEqual(lines.map(each => each.attributes.has("data-future")), [false, true, false]);
});
