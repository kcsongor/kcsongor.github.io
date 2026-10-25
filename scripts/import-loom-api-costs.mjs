// Read saved run metadata only. Never export terminal events, transcripts, or credentials.
// Run after refreshing results.json: node scripts/import-loom-api-costs.mjs /path/to/eval/build/runs
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { createHash } from "node:crypto";
import assert from "node:assert/strict";
import { estimateApiCost, openAIRates } from "./loom-api-cost.mjs";

const root = process.argv[2];
assert(root, "Pass the directory containing saved Loom runs.");
const snapshot = JSON.parse(readFileSync(new URL("../src/data/loom/results.json", import.meta.url)));
const rows = snapshot.rows.map(row => {
  assert(!row.run_id.includes("/") && !row.run_id.includes(".."));
  const bytes = readFileSync(join(root, row.run_id, "run-status.json"));
  const report = JSON.parse(bytes).agent_report;
  if (report?.reported_model) assert.equal(report.reported_model, row.model);
  return {
    run_id: row.run_id, model: row.model, ...estimateApiCost(row.model, report),
    source_sha256: createHash("sha256").update(bytes).digest("hex"),
  };
});
const output = {
  schema_version: 1, pricing_checked: "2026-10-25", currency: "USD",
  openai_rates_per_million: openAIRates,
  sources: [
    "https://developers.openai.com/api/docs/models/gpt-6.1-sol",
    "https://developers.openai.com/api/docs/models/gpt-6-astra",
    "https://platform.claude.com/docs/en/about-claude/pricing",
  ],
  rows,
};
writeFileSync(new URL("../src/data/loom/api-costs.json", import.meta.url), JSON.stringify(output, null, 2) + "\n");
console.log(`Estimated ${rows.filter(row => row.usd !== null).length} of ${rows.length} attempts; missing estimates remain null.`);
