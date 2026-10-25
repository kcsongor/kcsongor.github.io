export const languageHazards = ["signer", "dispatch", "deferred_values"];
export const matrixHazards = [...languageHazards, "rounding"];
export const roundingFixJudgments = {
  none: { symbol: "○", label: "No fix found" },
  proposed: { symbol: "P", label: "Correction proposed" },
  guard: { symbol: "G", label: "Partial guard implemented" },
  proposed_guard: { symbol: "P+G", label: "Proposed correction + partial guard" },
  implemented: { symbol: "I", label: "Correction implemented" },
  missing: { symbol: "—", label: "Not audited" },
};

export function recognitionCount(row) {
  if (row.status !== "completed" || row.reviewed !== true || languageHazards.some(key => row[key] == null)) return null;
  return languageHazards.filter(key => row[key] === "correct").length;
}

export function filterRuns(rows, filters = {}) {
  return rows.filter(row => Object.entries(filters).every(([key, value]) => value === "all" || row[key] === value));
}

export const plotFrame = { left: 76, right: 596, top: 30, bottom: 350 };

// Shape and colour both identify the model; fill identifies the prompt.
export const modelMarker = model => ({
  "gpt-6-astra": "M0,-8 2.4,-2.7 7.6,-2.5 3.8,1.2 4.7,6.5 0,4 -4.7,6.5 -3.8,1.2 -7.6,-2.5 -2.4,-2.7Z",
  "gpt-6.1-sol": "M6,0 A6,6 0 1,1 -6,0 A6,6 0 1,1 6,0Z M0,-8V-10 M0,8V10 M8,0H10 M-8,0H-10 M6,-6L7.5,-7.5 M-6,-6L-7.5,-7.5 M6,6L7.5,7.5 M-6,6L-7.5,7.5",
  "claude-opus-5-5": "M0,-8 7,0 0,8 -7,0Z",
  "claude-sonnet-5-5": "M-6,-6H6V6H-6Z",
  "gemini-4-argon-high": "M0,-8 7,6H-7Z",
}[model] ?? "M6,0 A6,6 0 1,1 -6,0 A6,6 0 1,1 6,0Z");

/** @returns {{maximum: number, ticks: Array<{x: number, label: string}>, points: Array<{id: string, x: number, y: number}>}} */
export function plotRuns(rows, metric = "duration_seconds", scale = "linear", scoreFor = recognitionCount) {
  const eligible = rows.filter(row => scoreFor(row) !== null && Number.isFinite(row[metric]) && (scale === "log" ? row[metric] > 0 : row[metric] >= 0));
  const unit = metric === "duration_seconds" ? 60 : 1;
  const values = eligible.map(row => row[metric] / unit);
  let minimum = 0;
  let maximum = Math.max(1, ...values);
  let ticks;
  if (scale === "log") {
    minimum = 10 ** Math.floor(Math.log10(Math.min(...values, maximum)));
    maximum = 10 ** Math.ceil(Math.log10(maximum));
    if (minimum === maximum) maximum *= 10;
    ticks = [];
    for (let exponent = Math.round(Math.log10(minimum)); exponent <= Math.round(Math.log10(maximum)); exponent++) ticks.push(10 ** exponent);
  } else {
    const magnitude = 10 ** Math.floor(Math.log10(maximum / 4));
    const step = [1, 2, 5, 10].find(value => value * magnitude >= maximum / 4) * magnitude;
    maximum = Math.ceil(maximum / step) * step;
    ticks = Array.from({ length: Math.round(maximum / step) + 1 }, (_, index) => index * step);
  }
  const position = value => plotFrame.left + (scale === "log" ? Math.log10(value / minimum) / Math.log10(maximum / minimum) : value / maximum) * (plotFrame.right - plotFrame.left);
  return {
    maximum: maximum * unit,
    ticks: ticks.map(value => ({ x: position(value), label: value.toLocaleString("en-GB", { maximumFractionDigits: 3 }) })),
    points: eligible.map(row => ({
      id: row.run_id,
      x: position(row[metric] / unit),
      y: plotFrame.bottom - scoreFor(row) / 3 * (plotFrame.bottom - plotFrame.top),
    })),
  };
}

export function metricLabel(value, metric) {
  if (value === null || !Number.isFinite(value)) return "Unknown";
  if (metric === "api_cost_usd") return `~$${value.toFixed(2)}`;
  return metric === "duration_seconds" ? `${(value / 60).toFixed(1)} min` : `${Math.round(value).toLocaleString("en-GB")} tokens`;
}

export const modelName = model => ({
  "gpt-6.1-sol": "GPT 6.1 Sol",
  "gpt-6-astra": "GPT 6 Astra",
  "claude-opus-5-5": "Opus 5.5",
  "claude-sonnet-5-5": "Sonnet 5.5",
  "gemini-4-argon-high": "Gemini 4 Argon",
}[model] ?? model);

// Equal weight per runtime hazard and complete review; rounding is a separate issue.
export function recognitionSummary(runs) {
  const eligible = runs.filter(run => run.status === "completed" && run.cells.slice(0, 3).length === 3 && run.cells.slice(0, 3).every(cell => cell !== "missing"));
  const cells = eligible.flatMap(run => run.cells.slice(0, 3));
  const correct = cells.filter(cell => cell === "correct").length;
  const partial = cells.filter(cell => cell === "partial").length;
  return {
    runs: eligible.length, total: cells.length, correct, partial,
    percent: cells.length ? 100 * correct / cells.length : null,
    partialPercent: cells.length ? 100 * partial / cells.length : 0,
  };
}

// Combine run dates by model and effective effort; retain provenance on each attempt.
/** @returns {Array<{model: string, effort: string, prompts: Array<{prompt: string, reviewed: number, runs: Array<{id: string, label: string, status: string, effortSource: string, cohort: string, durationSeconds: number | null, roundingFix: string, roundingFixNote: string, cells: Array<"correct" | "partial" | "unstated" | "incorrect" | "missing">}>}>}>} */
export function buildHazardMatrix(rows) {
  const groups = new Map();
  for (const row of rows) {
    const key = JSON.stringify([row.model, row.effective_effort]);
    if (!groups.has(key)) groups.set(key, { model: row.model, effort: row.effective_effort, rows: [] });
    groups.get(key).rows.push(row);
  }
  const efforts = { medium: 0, high: 1, xhigh: 2, max: 3 };
  return [...groups.values()].sort((a, b) => modelName(a.model).localeCompare(modelName(b.model)) || (efforts[a.effort] ?? 9) - (efforts[b.effort] ?? 9)).map(group => ({
    model: group.model,
    effort: group.effort,
    prompts: ["baseline", "assumptions"].map(prompt => {
      const attempts = group.rows.filter(row => row.prompt_condition === prompt);
      return {
        prompt,
        reviewed: attempts.filter(row => recognitionCount(row) !== null).length,
        runs: attempts.map(row => ({
          id: row.run_id,
          label: row.label,
          status: row.status,
          effortSource: row.effort_source,
          cohort: row.cohort,
          durationSeconds: row.duration_seconds ?? null,
          roundingFix: row.status === "completed" && row.reviewed === true ? row.rounding_fix ?? "missing" : "missing",
          roundingFixNote: row.rounding_fix_note ?? "No completed source audit for this attempt.",
          cells: matrixHazards.map(key => row.reviewed === true && row[key] != null ? row[key] : "missing"),
        })),
      };
    }),
  }));
}
