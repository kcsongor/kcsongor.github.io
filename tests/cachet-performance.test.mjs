import test from "node:test";
import assert from "node:assert/strict";
import stages from "../src/components/figures/cachet-schedule-stages.json" with { type: "json" };
import snapshot from "../src/components/figures/cachet-benchmarks.json" with { type: "json" };
import {
  timing,
  measurement,
  target,
  comparison,
  referenceTiming,
  hasTimings,
} from "../src/components/figures/cachet-performance.mjs";

// Attention's small teaching example measures launch latency rather than the
// optimisation. All other viewers must retain measurements for every step.
const untimedSchedules = new Set(["flash"]);
const timedSchedules = Object.entries(stages).filter(([name]) => !untimedSchedules.has(name));
const timedNames = timedSchedules.map(([name]) => name).sort();

test("every timed schedule step has a measurement for the correct program and workload", () => {
  assert.deepEqual(Object.keys(snapshot.stages).sort(), timedNames);
  for (const [name, steps] of timedSchedules) {
    assert.equal(hasTimings(name), true);
    assert.equal(snapshot.stages[name].length, steps.length, name);
    steps.forEach((step, index) => {
      const entry = snapshot.stages[name][index];
      const result = timing(name, index, step.title);
      assert.equal(entry.step, step.title);
      assert.match(result.duration, /[\d,.]+ ms$/);
      assert.ok(result.context.startsWith(entry.target));
      if (entry.measurement) {
        assert.equal(entry.measurement.split(".")[0], entry.workload);
        assert.equal(result.source, measurement(entry.measurement).source ?? snapshot.runs[entry.workload].source);
        assert.ok(measurement(entry.measurement).seconds > 0);
      }
    });
  }
});

test("the final article uses only measured timings and library targets", () => {
  for (const [name, steps] of Object.entries(snapshot.stages)) {
    steps.forEach((entry, index) => {
      const result = timing(name, index);
      assert.equal(entry.placeholderSeconds, undefined);
      assert.ok(entry.measurement);
      assert.equal(result.placeholder, false);
      assert.equal(result.comparison.target.placeholder, false);
    });
  }
  for (const run of Object.values(snapshot.runs)) {
    assert.ok(run.source, "recorded measurements retain their source");
    for (const value of Object.values(run.measurements)) {
      assert.equal(value.placeholderSeconds, undefined);
    }
  }
});

test("attention deliberately has no timing bars or library target", () => {
  for (const name of untimedSchedules) {
    assert.ok(stages[name].length > 0, "the teaching example is still present");
    assert.equal(hasTimings(name), false);
    assert.equal(snapshot.stages[name], undefined);
    assert.equal(snapshot.scheduleTargets[name], undefined);
    assert.throws(() => timing(name, 0), /Update timing/);
  }
  assert.equal(snapshot.targets.attention, undefined);
});

test("a reordered schedule or missing measurement fails rather than showing a stale number", () => {
  assert.throws(() => timing("swap", 1, "a different rewrite"), /Update timing/);
  assert.throws(() => timing("swap", 99), /Update timing/);
  assert.throws(() => measurement("cpu.missing"), /Unknown benchmark/);
});

test("the targets are recorded library results, and every timed schedule has a matching target", () => {
  for (const key of Object.keys(snapshot.targets)) {
    const goal = target(key);
    assert.equal(goal.placeholder, false);
    assert.ok(goal.gflops > 0);
  }
  assert.deepEqual(Object.keys(snapshot.scheduleTargets).sort(), timedNames);
  for (const [name, steps] of timedSchedules) {
    const comparisons = steps.map((step, index) => timing(name, index, step.title).comparison);
    assert.equal(new Set(comparisons.map(c => c.targetPosition)).size, 1, `${name}: reference must not jump between steps`);
    comparisons.forEach((c, index) => {
      assert.equal(c.target.workloadKey, snapshot.stages[name][index].workload);
      assert.ok(c.width > 0 && c.width <= 100);
      assert.ok(c.targetPosition > 0 && c.targetPosition <= 100);
    });
  }
});

test("the bars use the appropriate one-core, all-core, tuning and GPU baselines", () => {
  const naive = referenceTiming("cpu.matmul", "one_core").comparison;
  assert.equal(naive.target.measurement, "cpu.openblas_neon_1");
  const bands = referenceTiming("cpu.bands", "all_cores").comparison;
  assert.equal(bands.target.measurement, "cpu.openblas_neon");
  const fast = timing("fast", 3).comparison;
  assert.equal(fast.target.measurement, "climb.openblas_neon_1");
  const gpu = referenceTiming("gpu.big_tiles", "gpu").comparison;
  assert.equal(gpu.target.measurement, "gpu.mlx");
  assert.throws(() => referenceTiming("cpu.matmul", "gpu"), /workloads differ/);
  assert.throws(() => target("missing"), /Unknown library target/);
});

test("unmeasured comparisons remain explicit even when recorded results are final", () => {
  const result = comparison({workload: "cpu", placeholderSeconds: 1}, "one_core");
  assert.equal(result.placeholder, true);
  assert.equal(result.comparison.placeholder, true);
  assert.equal(result.comparison.target.placeholder, false);
});

test("the CPU loop nest before the GPU grid keeps its device label", () => {
  const beforeGrid = timing("rows", 0);
  assert.match(beforeGrid.context, /^CPU/);
  assert.equal(beforeGrid.comparison.target.key, "gpu");
});

test("large gains extend the scale without clipping the result or hiding 100%", () => {
  const goal = target("one_core");
  const result = comparison({workload: "cpu", target: "CPU", placeholderSeconds: goal.seconds / 2}, "one_core");
  assert.equal(result.comparison.percent, 200);
  assert.equal(result.comparison.width, 100);
  assert.equal(result.comparison.targetPosition, 50);
  const slow = comparison({workload: "cpu", placeholderSeconds: goal.seconds * 2000}, "one_core");
  assert.equal(slow.comparison.percentLabel, "<0.1%");
  assert.throws(() => comparison({workload: "cpu", placeholderSeconds: 0}, "one_core"), /Invalid duration/);
});
