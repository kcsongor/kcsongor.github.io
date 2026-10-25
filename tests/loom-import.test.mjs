import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const importer = fileURLToPath(new URL("../scripts/import-loom-results.mjs", import.meta.url));
test("Argon reviews verify saved provenance and retain reviewed API failures", () => {
  const root = mkdtempSync(join(tmpdir(), "loom-argon-import-"));
  try {
    const sourceDir = join(root, "results/base");
    const reviews = join(root, "results/2026-10-25-argon/reviews");
    const runDir = join(root, "build/runs/argon-run");
    for (const dir of [sourceDir, reviews, runDir]) mkdirSync(dir, { recursive: true });
    const save = (path, data) => writeFileSync(path, JSON.stringify(data));
    const source = join(sourceDir, "token-usage.json");
    const output = join(root, "output.json");
    save(source, { schema_version: 1, generated_at: "2026-10-21", scope: "Test", sources: [], rows: [] });
    save(join(reviews, "argon.json"), {
      schema_version: 1, run_id: "argon-run", label: "argon-assumptions", model: "gemini-4-argon-high",
      requested_effort: "high", document_assumptions: true, prompt_condition: "document-assumptions-v1",
      status: "agent_error", repeat: 2, duration_seconds: 1038.7, review_method: { reviewer: "test" },
      signer: { recognition: "partial" }, dispatch: { recognition: "unstated" }, deferred_values: { recognition: "unstated" }, rounding: { recognition: "unstated" },
    });
    save(join(runDir, "manifest.json"), { model: "gemini-4-argon-high", agent: "antigravity", agent_version: "1.4.0", reasoning_effort: "high", document_assumptions: true });
    const status = { status: "agent_error", agent_report: { reported_model: "gemini-4-argon-high", usage: { output_tokens: 60000 }, terminal_event: "must not export" } };
    save(join(runDir, "run-status.json"), status);
    const args = [importer, source, "--reviews", reviews, "--output", output];
    execFileSync(process.execPath, args, { stdio: "pipe" });
    const data = JSON.parse(readFileSync(output));
    assert.equal(data.rows[0].agent, "antigravity");
    assert.equal(data.rows[0].cohort, "2026-10-25-argon");
    assert.equal(data.rows[0].output_tokens, 60000);
    assert.equal(data.rows[0].status, "agent_error");
    assert.equal(data.rows[0].reviewed, true);
    assert.equal(data.rows[0].signer, "partial");
    assert.equal(data.sources.length, 3);
    assert(!JSON.stringify(data).includes("must not export"));
    save(join(runDir, "run-status.json"), { ...status, status: "completed" });
    assert.throws(() => execFileSync(process.execPath, args, { stdio: "pipe" }));
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test("review import normalizes prompt names, retains exclusions and hashes, and rejects duplicates", () => {
  const root = mkdtempSync(join(tmpdir(), "loom-import-"));
  try {
    const reviews = join(root, "reviews");
    const interrupted = join(root, "interrupted-run");
    mkdirSync(reviews); mkdirSync(interrupted);
    const save = (path, data) => writeFileSync(path, JSON.stringify(data));
    const base = join(root, "base.json");
    const output = join(root, "combined.json");
    save(base, { schema_version: 1, generated_at: "2026-10-21T00:00:00Z", scope: "Test", sources: [], rows: [] });
    const review = {
      schema_version: 1, run_id: "reviewed-run", label: "test", model: "claude-opus-5-5",
      requested_effort: "high", document_assumptions: true, prompt_condition: "document-assumptions-v1",
      status: "completed", repeat: 1, reviewed_at: "2026-10-24T12:00:00Z", review_method: { reviewer: "test" },
      provenance: { reported_model: "claude-opus-5-5", agent_version: "test" },
      duration_seconds: 60, usage: { output_tokens: 123, unrelated_field: "do not export" },
      signer: { recognition: "correct", explanation: "do not copy prose" },
      dispatch: { recognition: "partial" }, deferred_values: { recognition: "unstated", capability_gate_recognition: "unstated" },
      rounding: { recognition: "incorrect" },
    };
    const reviewPath = join(reviews, "review.json");
    save(reviewPath, review);
    save(join(interrupted, "manifest.json"), { model: "claude-opus-5-5", reasoning_effort: "high", document_assumptions: true, agent: "claude-code", agent_version: "test" });
    save(join(interrupted, "run-status.json"), { status: "interrupted", seconds: 10 });
    const args = [importer, base, "--reviews", reviews, "--excluded-run", interrupted, "--output", output];
    execFileSync(process.execPath, args, { stdio: "pipe" });
    const imported = JSON.parse(readFileSync(output));
    assert.equal(imported.rows.length, 2);
    const scored = imported.rows.find(row => row.reviewed);
    const excluded = imported.rows.find(row => !row.reviewed);
    assert.equal(scored.prompt_condition, "assumptions");
    assert.equal(scored.output_tokens, 123);
    assert.equal(scored.dispatch, "partial");
    assert.equal(excluded.status, "interrupted");
    assert.equal(excluded.signer, null);
    assert.equal(excluded.output_tokens, null);
    assert.equal(imported.base_generated_at, "2026-10-21T00:00:00Z");
    assert(imported.sources.some(source => source.sha256 === createHash("sha256").update(readFileSync(reviewPath)).digest("hex")));
    assert(!JSON.stringify(imported).includes("do not"));
    save(join(interrupted, "run-status.json"), { status: "timeout", seconds: 2700.3 });
    execFileSync(process.execPath, args, { stdio: "pipe" });
    const timedOut = JSON.parse(readFileSync(output)).rows.find(row => !row.reviewed);
    assert.equal(timedOut.status, "timeout");
    assert(timedOut.label.endsWith("-timeout"));
    assert.equal(timedOut.signer, null);
    assert.equal(timedOut.duration_seconds, 2700.3);
    const audit = join(root, "audit.json");
    save(audit, { schema_version: 1, rows: [{ run_id: review.run_id, status: "proposed_guard", proposed: true, partial_guard: true, implemented: false, note: "Proposed correction; original formula retained.", evidence: [{ file: "work/exercise/exchange.loom" }], recognition_correction: { from: "incorrect", to: "correct" } }] });
    execFileSync(process.execPath, [...args, "--rounding-audit", audit], { stdio: "pipe" });
    const audited = JSON.parse(readFileSync(output)).rows.find(row => row.reviewed);
    assert.equal(audited.rounding, "correct");
    assert.equal(audited.rounding_fix, "proposed_guard");
    assert.equal(audited.dispatch, "partial");
    save(audit, { schema_version: 1, rows: [{ run_id: "missing", status: "none" }] });
    assert.throws(() => execFileSync(process.execPath, [...args, "--rounding-audit", audit], { stdio: "pipe" }), /requires a reviewed completion/);
    const presentation = join(root, "presentation.json");
    save(presentation, { schema_version: 1, omitted_attempts: [{ run_id: "interrupted-run", reason: "Author requested omission" }], run_notes: [{ run_id: review.run_id, note: "Author clarification" }] });
    execFileSync(process.execPath, [...args, "--presentation", presentation], { stdio: "pipe" });
    const displayed = JSON.parse(readFileSync(output));
    assert.deepEqual(displayed.rows.map(row => row.run_id), [review.run_id]);
    assert.equal(displayed.rows[0].run_note, "Author clarification");
    assert.equal(displayed.omitted_attempts[0].run_id, "interrupted-run");
    save(presentation, { schema_version: 1, omitted_attempts: [{ run_id: review.run_id, reason: "Should not omit reviewed work" }] });
    assert.throws(() => execFileSync(process.execPath, [...args, "--presentation", presentation], { stdio: "pipe" }), /requires an unreviewed attempt/);
    save(join(reviews, "duplicate.json"), review);
    assert.throws(() => execFileSync(process.execPath, args, { stdio: "pipe" }), /Duplicate imported run/);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});
