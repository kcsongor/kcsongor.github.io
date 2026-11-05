import test from 'node:test';
import assert from 'node:assert/strict';
import { cacheModel, readsOf, totalTime } from '../src/components/figures/cache-model.mjs';

test('walking along the rows misses once per line',()=>{
  const reads=readsOf(cacheModel,'rows');
  assert.equal(reads.length,64);
  assert.equal(reads.filter(r=>r.miss).length,16);
  assert.equal(totalTime(reads),16*8+48);
});

test('walking down the columns misses on every read',()=>{
  const reads=readsOf(cacheModel,'columns');
  assert.equal(reads.length,64);
  assert.equal(reads.filter(r=>r.miss).length,64);
  assert.equal(totalTime(reads),64*8);
});

test('both walks read every element once',()=>{
  for(const walk of ['rows','columns']){
    const cells=new Set(readsOf(cacheModel,walk).map(r=>r.row*8+r.col));
    assert.equal(cells.size,64);
  }
});

test('the cache never holds more than its capacity',()=>{
  for(const read of readsOf(cacheModel,'columns'))
    assert.ok(read.cached.length<=cacheModel.capacity);
});
