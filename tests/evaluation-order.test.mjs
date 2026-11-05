import test from 'node:test';
import assert from 'node:assert/strict';
import { additions, chain, grouped, exampleValues, roundedSum } from '../src/components/figures/evaluation-order.mjs';
const leaves=tree=>'k' in tree?[tree.k]:[...leaves(tree.left),...leaves(tree.right)];
test('both trees add the same values, left to right',()=>{
  for(const tree of [chain(6),grouped(6,2),grouped(6,3)])
    assert.deepEqual(leaves(tree),[0,1,2,3,4,5]);
});
test('regrouping the displayed f32 values loses the one',()=>{
  assert.ok(exampleValues.every(value=>Math.fround(value)===value));
  assert.equal(exampleValues.reduce((a,b)=>a+b),1);
  assert.equal(roundedSum(chain(6)),1);
  assert.equal(roundedSum(grouped(6,2)),0);
  assert.equal(Math.fround(-(2**25)+1),-(2**25));
});
test('a chain takes one step per addition',()=>{
  assert.equal(additions(chain(6)).length,5);
  assert.equal(chain(6).step,5);
});
test('grouping does the same additions in fewer steps',()=>{
  assert.equal(additions(grouped(6,2)).length,5);
  assert.equal(grouped(6,2).step,3);
  assert.equal(additions(grouped(6,2)).filter(a=>a.step===1).length,3);
});
