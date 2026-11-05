import test from 'node:test';
import assert from 'node:assert/strict';
import { productsFor, stepCount } from '../src/components/figures/matmul-model.mjs';
import { landed, MOVE, FLIGHT, LAND } from '../src/components/figures/matrix-motion.js';
const dims={m:4,k:6,n:5};
const a=(i,k)=>(i+1)*3-k*.5;
const b=(k,j)=>k*.25-j+2;
const reference=(i,j)=>Array.from({length:dims.k},(_,k)=>a(i,k)*b(k,j)).reduce((x,y)=>x+y,0);
const run=(mode,output=0)=>{
  const values=Array.from({length:dims.m},()=>Array(dims.n).fill(0));
  const seen=new Set();
  for(let step=0;step<stepCount(mode,dims.k);step++)for(const p of productsFor(mode,step,dims,output)){
    const key=[p.i,p.j,p.k].join(',');assert.ok(!seen.has(key),'one contribution per operand pair');seen.add(key);
    values[p.i][p.j]+=a(p.i,p.k)*b(p.k,p.j);
  }
  return {values,seen};
};
test('independent and blocked schedules compute the same complete product',()=>{
  for(const mode of ['outputs','blocks']){
    const {values,seen}=run(mode);assert.equal(seen.size,120);
    values.forEach((row,i)=>row.forEach((value,j)=>assert.equal(value,reference(i,j))));
  }
});
test('selecting any scalar output streams every term exactly once',()=>{
  for(let output=0;output<20;output++){
    const {values,seen}=run('scalar',output);assert.equal(seen.size,6);
    assert.equal(values[Math.floor(output/5)][output%5],reference(Math.floor(output/5),output%5));
  }
});
test('split sums partition six terms and the merge does not reread inputs',()=>{
  const {values,seen}=run('split');assert.equal(seen.size,6);assert.equal(values[1][3],reference(1,3));
  assert.deepEqual(productsFor('split',2,dims),[]);
});
test('each delayed stream leaves output unchanged until arrival and settles before the next step',()=>{
  for(const delay of [0,220,440,780]){
    const arrival=MOVE+FLIGHT+delay;
    assert.equal(landed(arrival-1,delay),0);
    assert.equal(landed(arrival,delay),0);
    assert.equal(landed(arrival+LAND/2,delay),.5);
    assert.equal(landed(arrival+LAND,delay),1);
  }
});

test('moving to another output preserves completed and partial dot products',async()=>{
  const {scalarHistory}=await import('../src/components/figures/matmul-model.mjs');
  const progress=new Map([[0,6],[1,2]]);
  const history=scalarHistory(progress,dims,2,0);
  assert.equal(history.filter(p=>p.i===0&&p.j===0).length,6);
  assert.equal(history.filter(p=>p.i===0&&p.j===1).length,2);
  assert.equal(history.filter(p=>p.i===0&&p.j===2).length,0);
  assert.equal(history.filter(p=>p.j===0).reduce((sum,p)=>sum+a(p.i,p.k)*b(p.k,p.j),0),reference(0,0));
  const resumed=scalarHistory(progress,dims,1,2);
  assert.equal(resumed.length,8,'resuming does not count old products twice');
  const finished=scalarHistory(progress,dims,0,5);
  assert.equal(finished.filter(p=>p.j===0).length,6,'refocusing a completed cell does not erase its final term');
  assert.equal(scalarHistory(new Map(),dims,0,0).length,0,'a new multiplication starts empty');
});

test('choosing an output before playback does not invent a completed product', async () => {
  // Exercise controller state without a browser or a rendered figure.
  const saved = { HTMLElement: globalThis.HTMLElement, customElements: globalThis.customElements };
  const elements = new Map();
  globalThis.HTMLElement = class {};
  globalThis.customElements = {
    get: name => elements.get(name),
    define: (name, element) => elements.set(name, element),
  };
  try {
    await import('../src/components/figures/cachet-matmul.js');
  } finally {
    for (const [name, value] of Object.entries(saved)) {
      if (value === undefined) delete globalThis[name];
      else globalThis[name] = value;
    }
  }
  const Figure = elements.get('cachet-matmul');
  const figure = new Figure();
  const playback = [];
  Object.assign(figure, {
    scalar: true, output: 0, index: 0, progress: new Map(),
    motion: { reduced: false, time: 0, frame: { duration: 1000 }, play: value => playback.push(value) },
    controls() {},
  });
  let selections = 0;
  const select = () => selections++;
  figure.request(select);
  assert.equal(selections, 1, 'an unstarted product does not delay selection');
  assert.equal(figure.progress.size, 0, 'nothing arrived before playback');
  assert.deepEqual(playback, []);

  figure.motion.time = 500;
  figure.request(select);
  assert.equal(selections, 1, 'a product already in flight finishes before selection');
  assert.equal(figure.pending, select);
  assert.deepEqual(playback, [true]);
  figure.commit();
  assert.equal(figure.progress.size, 0, 'an unfinished product is not committed');
  figure.motion.time = 1000;
  figure.commit();
  assert.equal(figure.progress.get(0), 1, 'completion records the product exactly once');
  figure.commit();
  assert.equal(figure.progress.get(0), 1);
});
