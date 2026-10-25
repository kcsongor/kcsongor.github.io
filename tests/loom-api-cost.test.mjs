import test from "node:test";
import assert from "node:assert/strict";
import { estimateApiCost } from "../scripts/loom-api-cost.mjs";
import { apiCost } from "../src/components/loom/api-cost.mjs";
import snapshot from "../src/data/loom/results.json" with { type: "json" };
import costs from "../src/data/loom/api-costs.json" with { type: "json" };

test("OpenAI cost separates cached input and never adds reasoning twice", () => {
  const usage = { input_tokens: 1000, cached_input_tokens: 600, cache_write_input_tokens: 0, output_tokens: 100, reasoning_output_tokens: 80 };
  assert.equal(estimateApiCost("gpt-6.1-sol", { usage }).usd, 0.00186);
  assert.equal(estimateApiCost("gpt-6-astra", { usage }).usd, 0.0096);
  assert.equal(estimateApiCost("gpt-6.1-sol", { usage: { ...usage, reasoning_output_tokens: 0 } }).usd, 0.00186);
  for (const altered of [{ cached_input_tokens: 1001 }, { output_tokens: null }, { cache_write_input_tokens: 1 }]) {
    assert.equal(estimateApiCost("gpt-6.1-sol", { usage: { ...usage, ...altered } }).usd, null);
  }
});

test("Claude uses the run-level estimate; Gemini and missing data stay blank", () => {
  assert.equal(estimateApiCost("claude-sonnet-5-5", { estimated_cost_usd: 5.4476672, usage: { output_tokens: 60391 } }).usd, 5.4476672);
  assert.equal(estimateApiCost("claude-sonnet-5-5", { estimated_cost_usd: 0 }).usd, 0);
  assert.equal(estimateApiCost("claude-sonnet-5-5", {}).usd, null);
  assert.equal(estimateApiCost("gpt-6-astra", {}).usd, null);
  assert.equal(estimateApiCost("gemini-4-argon-high", { estimated_cost_usd: 1 }).usd, null);
});

test("cost snapshot covers current runs, exposes no raw events, and leaves unknown costs empty", () => {
  assert.deepEqual(costs.rows.map(r => r.run_id), snapshot.rows.map(r => r.run_id));
  assert.equal(costs.rows.filter(r => r.usd !== null).length, 68);
  for (const row of snapshot.rows) {
    const cost = costs.rows.find(c => c.run_id === row.run_id);
    assert.deepEqual(Object.keys(cost).sort(), ["basis", "model", "run_id", "source_sha256", "usd"]);
    assert.equal(cost.model, row.model);
    if (row.agent === "antigravity" || !row.reviewed || row.status !== "completed") assert.equal(apiCost(row), null);
    else assert.match(apiCost(row).label, /^~\$\d+\.\d{2}$/);
  }
});
