import { apiCost } from "./api-cost.mjs";
import { recognitionCount, modelName, promptName, metricLabel, plotRuns } from "./results.mjs";

const mean = values => values.length ? values.reduce((a, b) => a + b, 0) / values.length : null;
const valid = value => Number.isFinite(value) && value >= 0;
export function reviewedMetrics(rows) {
  return rows.filter(row => recognitionCount(row) !== null).map(row => ({
    ...row, api_cost_usd: apiCost(row)?.usd ?? null,
  }));
}

export function averageMetrics(rows) {
  const eligible = reviewedMetrics(rows);
  const times = eligible.map(row => row.duration_seconds).filter(valid);
  const costs = eligible.map(row => row.api_cost_usd).filter(valid);
  return { runs: eligible.length, time: mean(times), timeCount: times.length, cost: mean(costs), costCount: costs.length };
}

// Both coordinates use the same reviewed runs, grouped by model, effort, and prompt.
// Every run has equal weight; baseline and assumptions are never pooled.
export const configurationId = row => JSON.stringify([row.model, row.effective_effort]);

export const averageId = row => JSON.stringify([row.model, row.effective_effort, row.prompt_condition]);

export function aggregateModels(rows, metric = "api_cost_usd") {
  const groups = new Map();
  for (const row of rows.filter(row => recognitionCount(row) !== null && valid(row[metric]))) {
    const id = averageId(row);
    if (!groups.has(id)) groups.set(id, []);
    groups.get(id).push(row);
  }
  return [...groups.entries()].map(([id, runs]) => ({
    run_id: id, model: runs[0].model, effort: runs[0].effective_effort, prompt: runs[0].prompt_condition, pairId: configurationId(runs[0]), runs: runs.length,
    recognition: mean(runs.map(recognitionCount)),
    [metric]: mean(runs.map(row => row[metric])),
  })).sort((a, b) => modelName(a.model).localeCompare(modelName(b.model)) || ["medium", "high", "xhigh", "max"].indexOf(a.effort) - ["medium", "high", "xhigh", "max"].indexOf(b.effort));
}

export function plotModelAverages(rows, metric = "api_cost_usd") {
  const groups = aggregateModels(rows, metric);
  const plot = plotRuns(groups, metric, "linear", row => row.recognition);
  const plotted = new Map(plot.points.map(point => [point.id, point]));
  const pairs = [...new Set(groups.map(group => group.pairId))].flatMap(id => {
    const baseline = groups.find(group => group.pairId === id && group.prompt === "baseline");
    const assumptions = groups.find(group => group.pairId === id && group.prompt === "assumptions");
    const from = baseline && plotted.get(baseline.run_id);
    const to = assumptions && plotted.get(assumptions.run_id);
    return from && to ? [{ id, model: baseline.model, from, to }] : [];
  });
  const effortSeries = [...new Set(groups.map(group => JSON.stringify([group.model, group.prompt])))].flatMap(id => {
    const members = groups.filter(group => JSON.stringify([group.model, group.prompt]) === id);
    const points = members.map(group => plotted.get(group.run_id)).filter(Boolean);
    return points.length > 1 ? [{ id, model: members[0].model, prompt: members[0].prompt, points,
      path: points.map((point, index) => `${index ? "L" : "M"}${point.x} ${point.y}`).join(" "),
    }] : [];
  });
  return { ...plot, groups, pairs, effortSeries };
}

export function describeAverage(group, metric) {
  return `${modelName(group.model)} · ${group.effort} · ${promptName(group.prompt)}: ${Math.round(group.recognition / 3 * 100)}% average runtime recognition, ${metricLabel(group[metric], metric)} average per run, ${group.runs} reviewed runs.`;
}
