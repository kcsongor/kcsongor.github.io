import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";

// Controller checks use small DOM stand-ins; no browser or site build runs.
const registry = new Map();
const saved = {
  HTMLElement: globalThis.HTMLElement,
  customElements: globalThis.customElements,
};
globalThis.HTMLElement = class {};
globalThis.customElements = {
  get: name => registry.get(name),
  define: (name, element) => registry.set(name, element),
};
try {
  await import("../src/components/figures/cachet-schedule.js");
} finally {
  for (const [name, value] of Object.entries(saved)) {
    if (value === undefined) delete globalThis[name];
    else globalThis[name] = value;
  }
}
const Schedule = registry.get("cachet-schedule");
const element = () => {
  const classes = new Set();
  return {
    attributes: {},
    classList: {
      add: value => classes.add(value),
      remove: value => classes.delete(value),
      contains: value => classes.has(value),
      toggle: (value, on) => on ? classes.add(value) : classes.delete(value),
    },
    setAttribute(name, value) { this.attributes[name] = value; },
  };
};
function fixture() {
  const viewer = new Schedule();
  const panes = element();
  const pre = { scrollTop: 0, getBoundingClientRect: () => ({ top: 100, bottom: 300 }) };
  const output = {
    ...element(),
    closest: () => pre,
    getBoundingClientRect: () => ({ top: 450, bottom: 475 }),
  };
  const stage = { ...element(), querySelectorAll: () => [output] };
  const otherStage = { ...element(), querySelectorAll: () => [] };
  const source = {
    ...element(), dataset: { link: "0" }, textContent: "  hoist (j, k) c",
    closest: selector => selector === "[data-stage]" ? stage : panes,
  };
  Object.assign(viewer, {
    stages: [stage, otherStage], tabs: [element(), element()],
    current: 0, pinned: null, picker: {}, position: {}, selection: {}, clear: {},
    previous: {}, next: {},
    querySelectorAll(selector) {
      if (selector === "[data-link]") return [source];
      if (selector === ".is-linked") return [source, output].filter(e => e.classList.contains("is-linked"));
      if (selector === ".has-link") return [panes].filter(e => e.classList.contains("has-link"));
      return [];
    },
  });
  return { viewer, source, output, pre };
}
test("hover previews do not scroll the code; selecting a step reveals its lines", () => {
  const { viewer, source, output, pre } = fixture();
  viewer.link(source);
  assert.equal(pre.scrollTop, 0);
  assert.equal(output.classList.contains("is-linked"), true);
  viewer.pin(source);
  assert.equal(pre.scrollTop, 326);
  assert.equal(viewer.pinned, source);
  assert.equal(source.attributes["aria-pressed"], "true");
  assert.equal(viewer.clear.hidden, false);
});
test("a pinned step survives a hover preview and can be cleared", () => {
  const { viewer, source, output } = fixture();
  viewer.pin(source);
  viewer.link(null);
  assert.equal(viewer.pinned, source);
  viewer.link(viewer.pinned);
  assert.equal(output.classList.contains("is-linked"), true);
  viewer.pin(source);
  assert.equal(viewer.pinned, null);
  assert.equal(source.attributes["aria-pressed"], "false");
  assert.equal(output.classList.contains("is-linked"), false);
  assert.equal(viewer.clear.hidden, true);
});
test("changing stages clears old links and keeps navigation within bounds", () => {
  const { viewer, source, output } = fixture();
  viewer.pin(source);
  viewer.show(99);
  assert.equal(viewer.current, 1);
  assert.equal(viewer.picker.value, "1");
  assert.equal(viewer.position.textContent, "Step 1 of 1");
  assert.equal(viewer.next.disabled, true);
  assert.equal(viewer.pinned, null);
  assert.equal(output.classList.contains("is-linked"), false);
  viewer.link(source);
  assert.equal(source.classList.contains("is-linked"), false, "old-stage links cannot light up the current stage");
  viewer.show(-1);
  assert.equal(viewer.current, 0);
  assert.equal(viewer.previous.disabled, true);
});

test("each run's shared listing shows every stage as its own listing would", async () => {
  const { inlineSource, sharedSource } = await import("../src/components/figures/cachet-inline-source.mjs");
  const schedules = JSON.parse(readFileSync(new URL("../src/components/figures/cachet-schedule-stages.json", import.meta.url), "utf8"));
  for (const [name, stages] of Object.entries(schedules)) {
    const runs = sharedSource(stages);
    assert.deepEqual(runs.flatMap(run => run.stages), stages.map((_, index) => index), name);
    for (const run of runs) {
      for (const index of run.stages) {
        const own = inlineSource(stages[index], stages[index - 1]);
        const shown = run.lines.filter(line => line.shown.includes(index));
        const mark = (line, current, future) => `${current ? "C" : "-"}${future ? "F" : "-"}${line}`;
        assert.deepEqual(
          shown.map(line => mark(line.text, line.current.includes(index), line.future.includes(index))),
          own.code.split("\n").map((line, i) => mark(line, own.current.includes(i), own.future.includes(i))),
          `${name} stage ${index}`,
        );
      }
    }
  }
});
