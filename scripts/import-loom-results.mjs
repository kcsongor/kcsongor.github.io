// Import only a public, reviewed-data export; never read agent streams or credentials.
// Usage: node scripts/import-loom-results.mjs export.json [--reviews reviews-dir] [--excluded-run run-dir]
import { readFileSync, writeFileSync, mkdirSync, readdirSync } from "node:fs";
import { createHash } from "node:crypto";
import assert from "node:assert/strict";
import { dirname, resolve, relative, join } from "node:path";

const source = process.argv[2];
if (!source) throw new Error("Pass the path to Loom's token-usage.json export.");
const bytes = readFileSync(source);
const input = JSON.parse(bytes);
assert.equal(input.schema_version, 1);
assert(Array.isArray(input.rows));
const sources = [...input.sources];
const evalRoot = resolve(dirname(source), "../..");
const readSource = path => {
  const data = readFileSync(path);
  sources.push({ path: relative(evalRoot, path), sha256: createHash("sha256").update(data).digest("hex") });
  return JSON.parse(data);
};
let outputPath;
let roundingAudit;
let presentation;
const extraRows = [];
const addedIds = new Set();
const originalIds = new Set(input.rows.map(row => row.run_id));
const add = row => {
  // Fail on overlap so refreshed upstream exports cannot silently double-count.
  assert(!originalIds.has(row.run_id) && !addedIds.has(row.run_id), `Duplicate imported run: ${row.run_id}`);
  addedIds.add(row.run_id);
  extraRows.push(row);
};
for (let index = 3; index < process.argv.length; index += 2) {
  const flag = process.argv[index];
  const path = process.argv[index + 1];
  assert(path, `Missing path for ${flag}`);
  if (flag === "--reviews") {
    for (const name of readdirSync(path).filter(name => name.endsWith(".json")).sort()) {
      const review = readSource(join(path, name));
      assert.equal(review.schema_version, 1);
      assert(["completed", "agent_error", "timeout"].includes(review.status));
      assert(review.review_method, `Incomplete review: ${name}`);
      // New public reviews omit provenance; verify against saved run metadata.
      const runPath = join(evalRoot, "build/runs", review.run_id);
      const manifest = review.provenance ? null : readSource(join(runPath, "manifest.json"));
      const status = review.provenance ? null : readSource(join(runPath, "run-status.json"));
      assert.equal(review.provenance?.reported_model ?? manifest.model, review.model);
      if (status) {
        assert.equal(status.status, review.status);
        assert.equal(manifest.reasoning_effort, review.requested_effort);
        assert.equal(manifest.document_assumptions, review.document_assumptions);
        assert.equal(status.agent_report?.reported_model, review.model);
      }
      assert.equal(typeof review.document_assumptions, "boolean");
      assert.equal(review.prompt_condition, review.document_assumptions ? "document-assumptions-v1" : "baseline");
      add({
        run_id: review.run_id, label: review.label, model: review.model,
        agent: manifest?.agent ?? "claude-code", agent_version: review.provenance?.agent_version ?? manifest.agent_version,
        effective_effort: review.requested_effort, effort_source: "explicit",
        prompt_condition: review.document_assumptions ? "assumptions" : "baseline", cohort: relative(evalRoot, resolve(path, "..")).replace(/^results\//, ""),
        repeat: review.repeat, status: review.status, reviewed: true,
        duration_seconds: review.duration_seconds, output_tokens: review.usage?.output_tokens ?? status?.agent_report?.usage?.output_tokens ?? null,
        ...Object.fromEntries(["signer", "dispatch", "deferred_values", "rounding"].map(key => [key, review[key]?.recognition])),
        deferred_authorization_witness: review.deferred_values?.capability_gate_recognition ?? null,
      });
    }
  } else if (flag === "--excluded-run") {
    const manifest = readSource(join(path, "manifest.json"));
    const status = readSource(join(path, "run-status.json"));
    assert(["interrupted", "timeout", "process_error", "agent_error"].includes(status.status), "Excluded run must have a recorded unsuccessful status");
    const runId = resolve(path).split("/").at(-1);
    add({
      run_id: runId, label: `${manifest.model}-${manifest.reasoning_effort}-${manifest.document_assumptions ? "assumptions" : "baseline"}-${status.status}`,
      model: manifest.model, agent: manifest.agent, agent_version: manifest.agent_version,
      effective_effort: manifest.reasoning_effort, effort_source: "explicit",
      prompt_condition: manifest.document_assumptions ? "assumptions" : "baseline",
      cohort: "claude-effort-2026-10-24", repeat: null, status: status.status, reviewed: false,
      duration_seconds: status.seconds ?? null, output_tokens: status.agent_report?.usage?.output_tokens ?? null,
    });
  } else if (flag === "--rounding-audit") {
    roundingAudit = readSource(path);
    assert.equal(roundingAudit.schema_version, 1);
  } else if (flag === "--presentation") {
    presentation = readSource(path);
    assert.equal(presentation.schema_version, 1);
  } else if (flag === "--output") {
    outputPath = resolve(path);
  } else {
    throw new Error(`Unknown import option: ${flag}`);
  }
}
const fields = ["run_id", "label", "model", "agent", "agent_version", "effective_effort", "effort_source", "prompt_condition", "cohort", "repeat", "status", "reviewed", "duration_seconds", "output_tokens", "signer", "dispatch", "deferred_values", "rounding", "deferred_authorization_witness"];
const rows = [...input.rows, ...extraRows].sort((a, b) => a.run_id.localeCompare(b.run_id)).map(row => {
  const clean = Object.fromEntries(fields.map(key => [key, row[key] ?? null]));
  assert(typeof clean.run_id === "string" && typeof clean.model === "string");
  assert.equal(typeof clean.reviewed, "boolean");
  assert(["baseline", "assumptions"].includes(clean.prompt_condition));
  assert(typeof clean.effective_effort === "string");
  for (const key of ["duration_seconds", "output_tokens"]) {
    assert(clean[key] === null || (Number.isFinite(clean[key]) && clean[key] >= 0));
  }
  for (const key of ["signer", "dispatch", "deferred_values", "rounding", "deferred_authorization_witness"]) {
    assert([null, "correct", "partial", "unstated", "incorrect"].includes(clean[key]), `${key}: ${clean[key]}`);
  }
  if (clean.reviewed && clean.status === "completed") {
    assert(["signer", "dispatch", "deferred_values"].every(key => clean[key] !== null));
  }
  return clean;
});
assert.equal(new Set(rows.map(row => row.run_id)).size, rows.length);
if (roundingAudit) {
  const seen = new Set();
  for (const audit of roundingAudit.rows) {
    assert(!seen.has(audit.run_id), `Duplicate rounding audit: ${audit.run_id}`);
    seen.add(audit.run_id);
    const row = rows.find(row => row.run_id === audit.run_id);
    assert(row?.reviewed && row.status === "completed", `Rounding audit requires a reviewed completion: ${audit.run_id}`);
    const status = audit.implemented ? "implemented" : audit.proposed ? (audit.partial_guard ? "proposed_guard" : "proposed") : audit.partial_guard ? "guard" : "none";
    assert.equal(audit.status, status);
    assert(audit.evidence.length > 0 && audit.note);
    row.rounding_fix = status;
    row.rounding_fix_note = audit.note;
    if (audit.recognition_correction) {
      const correction = audit.recognition_correction;
      assert([correction.from, correction.to].includes(row.rounding), `Rounding review changed since audit: ${audit.run_id}`);
      assert(["correct", "partial", "unstated", "incorrect"].includes(correction.to));
      row.rounding = correction.to;
    }
  }
}
const omitted = presentation?.omitted_attempts ?? [];
for (const omission of omitted) {
  const row = rows.find(row => row.run_id === omission.run_id);
  assert(row && !row.reviewed && omission.reason, `Omission requires an unreviewed attempt and reason: ${omission.run_id}`);
}
for (const annotation of presentation?.run_notes ?? []) {
  const row = rows.find(row => row.run_id === annotation.run_id);
  assert(row && annotation.note, `Unknown annotated run: ${annotation.run_id}`);
  row.run_note = annotation.note;
}
const includedRows = rows.filter(row => !omitted.some(omission => omission.run_id === row.run_id));
const output = {
  schema_version: 1,
  generated_at: extraRows.length ? new Date().toISOString() : input.generated_at,
  base_generated_at: input.generated_at,
  scope: extraRows.length ? `${input.scope} Includes supplemental public reviews and retained unsuccessful attempts. Only reviewed completions contribute to scores and averages.` : input.scope,
  source_sha256: createHash("sha256").update(bytes).digest("hex"),
  sources,
  omitted_attempts: omitted,
  rows: includedRows,
};
const destination = outputPath ?? new URL("../src/data/loom/results.json", import.meta.url);
if (outputPath) mkdirSync(dirname(outputPath), { recursive: true });
else mkdirSync(new URL(".", destination), { recursive: true });
writeFileSync(destination, JSON.stringify(output, null, 2) + "\n");
console.log(`Imported ${includedRows.length} attempts (${omitted.length} omitted) from snapshot ${output.generated_at}.`);
