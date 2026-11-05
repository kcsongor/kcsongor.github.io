import test from 'node:test';
import assert from 'node:assert/strict';
import { configuration, issuedGroup, inFlight, arrival } from '../src/components/figures/cachet-register-model.mjs';

test('eight chains issue for two cycles, sixteen for all four', () => {
  assert.equal(configuration(8).vectors, 8);
  assert.equal(configuration(16).vectors, 16);
  assert.deepEqual([0, 1, 2, 3].map(c => issuedGroup(8, c)), [0, 1, null, null]);
  assert.deepEqual([0, 1, 2, 3].map(c => issuedGroup(16, c)), [0, 1, 2, 3]);
});

test('every accumulator waits four cycles before it is reused', () => {
  for (const width of [8, 16]) {
    const last = new Map();
    for (let cycle = 0; cycle < 24; cycle++) {
      const group = issuedGroup(width, cycle);
      if (group === null) continue;
      if (last.has(group)) assert.equal(cycle - last.get(group), 4);
      last.set(group, cycle);
    }
  }
});

test('packets leave exactly when the matching output vector lights up', () => {
  for (const width of [8, 16]) {
    for (let cycle = 4; cycle < 12; cycle++) {
      const group = issuedGroup(width, cycle - 4);
      const before = inFlight(width, cycle - 0.001).filter(p => p.cycle === cycle - 4);
      const after = inFlight(width, cycle).filter(p => p.cycle === cycle - 4);
      assert.equal(before.length, group === null ? 0 : 4);
      assert.equal(after.length, 0);
      assert.equal(arrival(width, cycle).group, group);
      assert.equal(arrival(width, cycle).glow, group === null ? 0 : 1);
    }
  }
});

test('steady-state packets have distinct accumulators and valid progress', () => {
  for (const width of [8, 16]) {
    for (const time of [0, 0.2, 1.99, 4, 10.5]) {
      const packets = inFlight(width, time);
      assert.equal(packets.length, configuration(width).vectors);
      assert.equal(new Set(packets.map(p => p.accumulator)).size, packets.length);
      assert.ok(packets.every(p => p.progress >= 0 && p.progress < 1));
    }
  }
  assert.throws(() => configuration(12), RangeError);
});
