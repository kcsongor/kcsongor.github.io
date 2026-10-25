import test from "node:test";
import assert from "node:assert/strict";
import snapshot from "../src/data/loom/results.json" with { type: "json" };
import roundingAudit from "../src/data/loom/rounding-audit.json" with { type: "json" };
import { recognitionCount, recognitionSummary, filterRuns, plotRuns, buildHazardMatrix, plotFrame } from "../src/components/loom/results.mjs";
import { createHighlighter } from "shiki";
import loom from "../src/syntax/loom.tmLanguage.json" with { type: "json" };

test("recognition excludes failed, unreviewed and missing judgments instead of scoring zero", () => {
  const row = { status: "completed", reviewed: true, signer: "correct", dispatch: "partial", deferred_values: "unstated", rounding: "correct" };
  assert.equal(recognitionCount(row), 1);
  assert.equal(recognitionCount({ ...row, status: "timeout" }), null);
  assert.equal(recognitionCount({ ...row, reviewed: false }), null);
  assert.equal(recognitionCount({ ...row, signer: null }), null);
  assert.equal(recognitionCount({ ...row, signer: "incorrect" }), 0);
});

test("snapshot preserves attempts and historical cohorts separately", () => {
  assert.equal(snapshot.rows.length, new Set(snapshot.rows.map(row => row.run_id)).size);
  const sweep = filterRuns(snapshot.rows, { cohort: "effort-sweep", model: "all" });
  assert.equal(sweep.length, 24);
  assert.equal(sweep.filter(row => recognitionCount(row) !== null).length, 22);
  assert.equal(filterRuns(snapshot.rows, { cohort: "effort-sweep", model: "claude-opus-5-5" }).length, 0);
  for (const metric of ["duration_seconds", "output_tokens"]) {
    const plot = plotRuns(sweep, metric);
    assert(plot.points.every(point => Number.isFinite(point.x) && point.x >= plotFrame.left && point.x <= plotFrame.right && point.y >= plotFrame.top && point.y <= plotFrame.bottom));
  }
  assert.deepEqual(plotRuns([]).points, []);
});

test("missing metrics are excluded; a real zero remains a valid observation", () => {
  const row = snapshot.rows.find(row => recognitionCount(row) !== null);
  assert.equal(plotRuns([{ ...row, output_tokens: null }], "output_tokens").points.length, 0);
  assert.equal(plotRuns([{ ...row, output_tokens: 0 }], "output_tokens").points.length, 1);
});

test("Loom grammar highlights addresses, keywords and comments without changing code", async () => {
  const highlighter = await createHighlighter({ langs: [loom], themes: ["github-light"] });
  const code = 'tx signed_by [@alice] { call Coin::steal(signer(@alice)) } // retained';
  const tokens = highlighter.codeToTokens(code, { lang: "loom", theme: "github-light" }).tokens.flat();
  assert.equal(tokens.map(token => token.content).join(""), code);
  assert(new Set(tokens.map(token => token.color)).size >= 3);
  highlighter.dispose();
});

test("hazard matrix preserves judgments and distinguishes unavailable reviews from unstated", () => {
  const row = { run_id: "a", label: "attempt a", cohort: "effort-sweep", model: "gpt-6-astra", effective_effort: "high", effort_source: "explicit", prompt_condition: "baseline", status: "completed", reviewed: true, signer: "correct", dispatch: "partial", deferred_values: "unstated", rounding: "incorrect" };
  const groups = buildHazardMatrix([
    row,
    { ...row, run_id: "b", signer: "incorrect", dispatch: null },
    { ...row, run_id: "failed", status: "timeout" },
    { ...row, run_id: "unreviewed", reviewed: false },
  ]);
  const [baseline, assumptions] = groups[0].prompts;
  assert.equal(baseline.reviewed, 1);
  assert.deepEqual(baseline.runs.map(run => run.cells), [
    ["correct", "partial", "unstated", "incorrect"],
    ["incorrect", "missing", "unstated", "incorrect"],
    ["correct", "partial", "unstated", "incorrect"],
    ["missing", "missing", "missing", "missing"],
  ]);
  assert.deepEqual(assumptions.runs, []);
  assert.equal(assumptions.reviewed, 0);
});

test("matrix combines collection dates and effective effort while preserving run provenance", () => {
  const row = snapshot.rows[0];
  const combined = buildHazardMatrix([
    { ...row, run_id: "first", cohort: "historical", effort_source: "explicit" },
    { ...row, run_id: "second", cohort: "effort-sweep", effort_source: "explicit" },
    { ...row, run_id: "third", cohort: "historical", effort_source: "documented_cli_default" },
  ]);
  assert.equal(combined.length, 1);
  assert.deepEqual(combined[0].prompts.flatMap(prompt => prompt.runs).map(run => [run.id, run.cohort, run.effortSource]), [
    ["first", "historical", "explicit"],
    ["second", "effort-sweep", "explicit"],
    ["third", "historical", "documented_cli_default"],
  ]);
  assert.equal(buildHazardMatrix([row, { ...row, effective_effort: "different" }]).length, 2);
  assert.deepEqual(buildHazardMatrix([]), []);
  const groups = buildHazardMatrix(snapshot.rows);
  assert.equal(groups.length, 12);
  const prompts = groups.flatMap(group => group.prompts);
  const runs = prompts.flatMap(prompt => prompt.runs);
  assert.equal(runs.length, 81);
  assert.equal(new Set(runs.map(run => run.id)).size, 81);
  assert.equal(prompts.reduce((total, prompt) => total + prompt.reviewed, 0), 75);
  assert.equal(runs.filter(run => run.cells.every(cell => cell === "missing")).length, 4);
  const failedArgon = snapshot.rows.find(row => row.model.startsWith("gemini") && row.reviewed && row.status === "agent_error");
  assert.equal(recognitionCount(failedArgon), null);
  assert.deepEqual(runs.find(run => run.id === failedArgon.run_id).cells, ["partial", "unstated", "unstated", "unstated"]);
});

test("logarithmic plots space equal ratios equally and keep zeros off the log axis", () => {
  const row = snapshot.rows.find(row => recognitionCount(row) !== null);
  const rows = [0, 10, 100, 1000].map(value => ({ ...row, run_id: String(value), output_tokens: value }));
  const log = plotRuns(rows, "output_tokens", "log");
  assert.equal(log.points.length, 3);
  assert.equal(log.points[1].x - log.points[0].x, log.points[2].x - log.points[1].x);
  assert.deepEqual(log.ticks.map(tick => tick.label), ["10", "100", "1,000"]);
  assert.equal(plotRuns(rows, "output_tokens", "linear").points.length, 4);
  for (const input of [[], [rows[0]], [rows[1]]]) {
    const plot = plotRuns(input, "output_tokens", "log");
    assert(plot.ticks.every(tick => Number.isFinite(tick.x)));
    assert(plot.points.every(point => Number.isFinite(point.x)));
  }
});

test("rounding and adopted fixes stay separate from language recognition", () => {
  const row = snapshot.rows.find(row => row.rounding_fix === "proposed_guard");
  const altered = { ...row, rounding: "incorrect", rounding_fix: "implemented" };
  assert.equal(recognitionCount(altered), recognitionCount(row));
  const [group] = buildHazardMatrix([altered, { ...altered, run_id: "failed", status: "timeout" }]);
  const runs = group.prompts.flatMap(prompt => prompt.runs);
  assert.equal(runs[0].cells[3], "incorrect");
  assert.equal(runs[0].roundingFix, "implemented");
  assert.equal(runs[1].roundingFix, "missing");
  assert.deepEqual(runs[1].cells, runs[0].cells);
  assert.equal(group.prompts.reduce((n, prompt) => n + prompt.reviewed, 0), 1);
});

test("rounding audit covers reviewed submissions and retains evidence for corrections", () => {
  assert.equal(roundingAudit.rows.length, 75);
  assert.equal(new Set(roundingAudit.rows.map(row => row.run_id)).size, 75);
  for (const audit of roundingAudit.rows) {
    const row = snapshot.rows.find(row => row.run_id === audit.run_id);
    assert.equal(row.rounding_fix, audit.status);
    assert(audit.evidence.every(item => /^[a-f0-9]{64}$/.test(item.sha256) && item.lines[0] <= item.lines[1]));
    if (audit.recognition_correction) assert.equal(row.rounding, audit.recognition_correction.to);
  }
  assert.equal(roundingAudit.rows.filter(row => row.proposed).length, 11);
  assert.equal(roundingAudit.rows.filter(row => row.partial_guard).length, 22);
  assert.equal(roundingAudit.rows.filter(row => row.implemented).length, 0);
  assert(snapshot.rows.filter(row => !row.reviewed).every(row => !row.rounding_fix));
});


test("overview percentages weight runtime hazards equally without credit for partial or rounding", () => {
  const rows = [
    { cells: ["correct", "correct", "partial", "incorrect"] },
    { cells: ["correct", "unstated", "incorrect", "correct"] },
    { cells: ["missing", "missing", "missing", "missing"] },
    { cells: ["correct", "missing", "correct", "correct"] },
  ];
  const attempts = [...rows.map(row => ({ ...row, status: "completed" })), { status: "agent_error", cells: ["correct", "correct", "correct", "correct"] }];
  assert.deepEqual(recognitionSummary(attempts), { runs: 2, total: 6, correct: 3, partial: 1, percent: 50, partialPercent: 100 / 6 });
  assert.equal(recognitionSummary([]).percent, null);
  assert.equal(recognitionSummary([{ status: "completed", cells: ["unstated", "incorrect", "partial", "correct"] }]).percent, 0);
  const argon = buildHazardMatrix(snapshot.rows).find(group => group.model === "gemini-4-argon-high");
  const score = recognitionSummary(argon.prompts.find(prompt => prompt.prompt === "assumptions").runs);
  assert.equal(score.runs, 3);
  assert.equal(score.percent, 100 / 9);
});
