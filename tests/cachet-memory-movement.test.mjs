import test from 'node:test';
import assert from 'node:assert/strict';
import { products, trace, frame, MODES, lineFor, PLAYBACK_MS, timeAtPosition, positionAtTime, advance, reuseExcerpt } from '../src/components/figures/cachet-memory-model.mjs';

test('every traversal visits all products once and preserves each output reduction order', () => {
  for (const mode of MODES) {
    const operations = products(mode);
    assert.equal(operations.length, 512);
    assert.equal(new Set(operations.map(({ i, j, k }) => `${i},${j},${k}`)).size, 512);
    for (let i = 0; i < 8; i++) for (let j = 0; j < 8; j++) {
      assert.deepEqual(operations.filter(p => p.i === i && p.j === j).map(p => p.k), [0, 1, 2, 3, 4, 5, 6, 7]);
    }
  }
});

test('cache hits never transfer a line, and misses replace the least recently used resident', () => {
  for (const mode of MODES) {
    const recent = [];
    for (const step of trace(mode).steps) for (const read of [step.a, step.b]) {
      assert.equal(read.hit, recent.includes(read.line));
      if (read.hit) assert.deepEqual(read.before, read.after);
      else if (recent.length === 8) assert.equal(read.evicted, recent.shift());
      if (recent.includes(read.line)) recent.splice(recent.indexOf(read.line), 1);
      recent.push(read.line);
      assert.deepEqual(read.after.filter(Boolean).sort(), [...recent].sort());
      assert.equal(read.after[read.slot], read.line);
    }
  }
});

test('counts distinguish logical reads, line fills, and repeated transfers', () => {
  const runs = MODES.map(trace);
  for (const run of runs) {
    assert.equal(run.reads, 1024);
    assert.equal(run.distinct, 32);
    assert.equal(run.hits + run.fills, run.reads);
    assert.equal(run.fills - run.reloads, 32);
  }
  assert.deepEqual(runs.map(r => r.fills), [528, 144, 64]);
  // Four adjacent B reads use one fetched line in row order.
  assert.deepEqual(runs[1].steps.slice(0, 4).map(s => s.b.hit), [false, true, true, true]);
  // A second output row in the first block reuses all four B lines.
  assert.ok(runs[2].steps.slice(16, 32).every(s => s.b.hit));
});

test('cache and output change on packet arrival, and completed outputs persist', () => {
  const run = trace('blocked');
  assert.ok(frame(run, 0.20).cache.every(x => x === null));
  assert.equal(frame(run, 0.22).cache[0], lineFor('A', 0, 0));
  assert.equal(frame(run, 0.97).output[0], 0);
  assert.equal(frame(run, 0.99).output[0], 1);
  assert.equal(frame(run, 1).output[0], 1);
  // Moving to the next depth block keeps partial totals; moving to the next
  // output block keeps all sixteen completed outputs.
  for (const [position, count] of [[64, 4], [128, 8]]) {
    const output = frame(run, position).output;
    for (let i = 0; i < 8; i++) for (let j = 0; j < 8; j++) {
      assert.equal(output[i * 8 + j], i < 4 && j < 4 ? count : 0);
    }
  }
  const last = frame(run, 512);
  assert.equal(last.completed, 512);
  assert.equal(last.fills, run.fills);
  assert.ok(last.output.every(x => x === 8));
});

test('misses add a slow memory leg while cached reads and accumulation keep their speed', () => {
  const run = trace('rows');
  const hitProduct = PLAYBACK_MS.cache * 2 + PLAYBACK_MS.accumulate;
  assert.equal(timeAtPosition(run, 1), hitProduct + 2 * PLAYBACK_MS.memory);
  assert.equal(timeAtPosition(run, 2) - timeAtPosition(run, 1), hitProduct);
  const before = frame(run, positionAtTime(run, PLAYBACK_MS.memory - 1));
  const after = frame(run, positionAtTime(run, PLAYBACK_MS.memory + 1));
  assert.ok(before.cache.every(x => x === null));
  assert.equal(after.cache[0], lineFor('A', 0, 0));
  const outputArrival = timeAtPosition(run, 0.72 + 0.28 * 0.93);
  assert.equal(frame(run, positionAtTime(run, outputArrival - 1)).output[0], 0);
  assert.equal(frame(run, positionAtTime(run, outputArrival + 1)).output[0], 1);
});

test('variable-speed playback survives seeks and crosses boundaries without skipping work', () => {
  const run = trace('blocked');
  for (const position of [0, 0.2088, 0.36, 0.72, 1, 4, 52, 64, 128, 511.99, 512]) {
    assert.ok(Math.abs(positionAtTime(run, timeAtPosition(run, position)) - position) < 1e-9);
    const whole = advance(run, position, 1800);
    let incremental = position;
    for (let n = 0; n < 120; n++) incremental = advance(run, incremental, 15);
    assert.ok(Math.abs(incremental - whole) < 1e-8);
  }
  assert.equal(advance(run, 511, run.duration), 512);
});

test('short article excerpts retain prior output and show B reuse between rows', () => {
  for (const mode of ['rows', 'blocked']) {
    const run = trace(mode), { start, end } = reuseExcerpt(mode);
    const before = frame(run, start), after = frame(run, end);
    assert.equal(end - start, 16);
    assert.equal(before.step.i, 1);
    assert.ok(before.output.some(n => n > 0));
    assert.ok(after.output.every((n, index) => n >= before.output[index]));
    const b = run.steps.slice(start, end).map(step => step.b);
    if (mode === 'blocked') {
      assert.ok(b.every(read => read.hit));
      for (let row = 0; row < 4; row++) assert.ok(before.cache.includes(lineFor('B', row, 0)));
    } else assert.ok(b.some(read => !read.hit && read.repeated));
    const duration = timeAtPosition(run, end) - timeAtPosition(run, start);
    assert.ok(duration < 10000, 'a complete excerpt should take less than ten seconds');
  }
});
