import test from "node:test";
import assert from "node:assert/strict";
import { aggregateModels, averageMetrics, plotModelAverages } from "../src/components/loom/averages.mjs";
import { apiCost } from "../src/components/loom/api-cost.mjs";
import { filterRuns } from "../src/components/loom/results.mjs";
import snapshot from "../src/data/loom/results.json" with { type: "json" };

const base = { model: "test", effective_effort: "high", prompt_condition: "baseline", status: "completed", reviewed: true, signer: "correct", dispatch: "correct", deferred_values: "correct", api_cost_usd: 2 };
test("model averages weight runs equally and pair cost with recognition on the same subset", () => {
  const rows = [
    { ...base, prompt_condition: "baseline", effective_effort: "high" },
    { ...base, prompt_condition: "assumptions", effective_effort: "medium", signer: "unstated", dispatch: "unstated", deferred_values: "unstated", api_cost_usd: 4 },
    { ...base, prompt_condition: "assumptions", effective_effort: "medium", api_cost_usd: 0 },
    { ...base, api_cost_usd: null },
    { ...base, api_cost_usd: 100, status: "timeout" },
  ];
  const groups = aggregateModels(rows);
  assert.equal(groups.length, 2);
  const medium = groups.find(g => g.effort === "medium");
  assert.equal(medium.runs, 2);
  assert.equal(medium.api_cost_usd, 2);
  assert.equal(medium.recognition, 1.5);
  const high = groups.find(g => g.effort === "high");
  assert.equal(high.runs, 1);
  assert.equal(high.recognition, 3);
  assert.notEqual(high.run_id, medium.run_id);
  assert.equal(aggregateModels(filterRuns(rows, { prompt_condition: "baseline" }))[0].recognition, 3);
  assert.equal(aggregateModels([{ ...base, api_cost_usd: null }]).length, 0);
  assert.equal(plotModelAverages([{ ...base, api_cost_usd: 0 }]).points.length, 1);
});

test("overview means exclude unreviewed attempts and preserve missing costs", () => {
  const row = snapshot.rows.find(row => row.reviewed && apiCost(row));
  const summary = averageMetrics([{ ...row, duration_seconds: 60 }, { ...row, duration_seconds: 180 }, { ...row, duration_seconds: 900, reviewed: false }]);
  assert.equal(summary.time, 120);
  assert.equal(summary.timeCount, 2);
  assert.equal(summary.cost, apiCost(row).usd);
  assert.equal(summary.costCount, 2);
  const gemini = snapshot.rows.filter(row => row.model.startsWith("gemini"));
  assert.equal(averageMetrics(gemini).cost, null);
  assert.equal(averageMetrics(gemini).timeCount, 7);
  assert.equal(averageMetrics([]).time, null);
});

test("current chart has 22 cost symbols and 24 time symbols, paired by model and effort", () => {
  const rows = snapshot.rows.map(row => ({ ...row, api_cost_usd: apiCost(row)?.usd ?? null }));
  const costs = plotModelAverages(rows);
  assert.equal(costs.points.length, 22);
  assert.equal(costs.groups.reduce((n, g) => n + g.runs, 0), 68);
  const times = plotModelAverages(rows, "duration_seconds");
  assert.equal(times.points.length, 24);
  assert.equal(times.groups.reduce((n, g) => n + g.runs, 0), 75);
  assert.equal(costs.pairs.length, 11);
  assert.equal(times.pairs.length, 12);
  for (const pair of costs.pairs) {
    const from = costs.groups.find(g => g.run_id === pair.from.id);
    const to = costs.groups.find(g => g.run_id === pair.to.id);
    assert.equal(from.prompt, "baseline"); assert.equal(to.prompt, "assumptions");
    assert.equal(from.model, to.model); assert.equal(from.effort, to.effort);
  }
  assert.equal(costs.ticks[0].label, "0");
  assert(costs.points.every(p => Number.isFinite(p.x) && Number.isFinite(p.y)));
});


test("prompt means stay separate; a missing partner never produces a line", () => {
  const rows = [{ ...base, api_cost_usd: 2 }, { ...base, prompt_condition: "assumptions", api_cost_usd: 4, signer: "unstated" }];
  const plot = plotModelAverages(rows);
  assert.equal(plot.groups.length, 2);
  assert.equal(plot.pairs.length, 1);
  assert.equal(plot.groups.find(g => g.prompt === "baseline").recognition, 3);
  assert.equal(plot.groups.find(g => g.prompt === "assumptions").recognition, 2);
  assert.equal(plotModelAverages([rows[0], { ...rows[1], api_cost_usd: null }]).pairs.length, 0);
});


test("effort lines connect only the same model and prompt in effort order", () => {
  const rows = snapshot.rows.map(row => ({ ...row, api_cost_usd: apiCost(row)?.usd ?? null }));
  const plot = plotModelAverages(rows);
  assert.equal(plot.effortSeries.length, 8);
  for (const series of plot.effortSeries) {
    const groups = series.points.map(point => plot.groups.find(group => group.run_id === point.id));
    assert(groups.every(group => group.model === series.model && group.prompt === series.prompt));
    const order = groups.map(group => ["medium", "high", "xhigh", "max"].indexOf(group.effort));
    assert.deepEqual(order, [...order].sort((a, b) => a - b));
  }
  assert.equal(plotModelAverages(rows.filter(row => row.model === "gpt-6-astra")).effortSeries.length, 2);
  assert.equal(plotModelAverages([{ ...base }]).effortSeries.length, 0);
});
