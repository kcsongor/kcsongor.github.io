export const SIZE = 8;
export const LINE_SIZE = 4;
export const CAPACITY = 8;
export const MODES = ["columns", "rows", "blocked"];
// Milliseconds of explanatory animation, not measured hardware latencies.
export const PLAYBACK_MS = { memory: 800, cache: 100, accumulate: 140 };

export function products(mode) {
  if (!MODES.includes(mode)) throw new RangeError("Unknown traversal");
  const result = [];
  if (mode === "columns") {
    for (let i = 0; i < SIZE; i++)
      for (let j = 0; j < SIZE; j++)
        for (let k = 0; k < SIZE; k++) result.push({ i, j, k });
  } else if (mode === "rows") {
    for (let i = 0; i < SIZE; i++)
      for (let k = 0; k < SIZE; k++)
        for (let j = 0; j < SIZE; j++) result.push({ i, j, k });
  } else {
    // A small illustrative cache-blocked loop nest, not the full tower schedule.
    for (let ii = 0; ii < SIZE; ii += 4)
      for (let jj = 0; jj < SIZE; jj += 4)
        for (let kk = 0; kk < SIZE; kk += 4)
          for (let i = ii; i < ii + 4; i++)
            for (let k = kk; k < kk + 4; k++)
              for (let j = jj; j < jj + 4; j++) result.push({ i, j, k });
  }
  return result;
}

export const lineFor = (array, row, col) =>
  `${array}:${row}:${Math.floor(col / LINE_SIZE) * LINE_SIZE}`;
export const parseLine = line => {
  const [array, row, col] = line.split(":");
  return { array, row: Number(row), col: Number(col) };
};

/** Input traffic only: a cold, fully associative, eight-line LRU cache.
 * Cache slots stay in place; hits update replacement order without moving data.
 * Output accumulation is tracked separately and does not compete for these slots.
 */
export function trace(mode) {
  const cache = Array(CAPACITY).fill(null),
    used = Array(CAPACITY).fill(-1);
  const seen = new Set(),
    output = Array(SIZE * SIZE).fill(0);
  let reads = 0,
    fills = 0,
    reloads = 0;
  const read = (array, row, col) => {
    const line = lineFor(array, row, col),
      before = cache.slice();
    let slot = cache.indexOf(line);
    const hit = slot !== -1;
    const repeated = !hit && seen.has(line);
    if (!hit) {
      slot = cache.indexOf(null);
      if (slot === -1) slot = used.indexOf(Math.min(...used));
      cache[slot] = line;
      fills++;
      if (repeated) reloads++;
    }
    used[slot] = reads++;
    seen.add(line);
    return {
      array,
      row,
      col,
      line,
      hit,
      repeated,
      slot,
      evicted: hit ? null : before[slot],
      before,
      after: cache.slice(),
      fills,
      reloads,
      reads,
    };
  };
  const steps = products(mode).map(({ i, j, k }) => {
    const before = output.slice();
    const a = read("A", i, k),
      b = read("B", k, j);
    output[i * SIZE + j]++;
    return { i, j, k, a, b, before, after: output.slice() };
  });
  const playback = [];
  let elapsed = 0;
  const segment = (from, to, duration) => {
    playback.push({ from, to, start: elapsed, end: elapsed + duration });
    elapsed += duration;
  };
  steps.forEach((step, index) => {
    for (const [stage, access] of [step.a, step.b].entries()) {
      const from = index + stage * 0.36,
        to = index + (stage + 1) * 0.36;
      const arrival = from + 0.36 * 0.58;
      if (!access.hit) segment(from, arrival, PLAYBACK_MS.memory);
      segment(access.hit ? from : arrival, to, PLAYBACK_MS.cache);
    }
    segment(index + 0.72, index + 1, PLAYBACK_MS.accumulate);
  });
  return {
    mode,
    steps,
    fills,
    reloads,
    hits: reads - fills,
    reads,
    distinct: seen.size,
    playback,
    duration: elapsed,
  };
}

// Both scrubbing and playback use the same product positions. Only the clock
// changes: a cache miss adds a slow memory-to-cache leg before the usual read.
function segmentAt(segments, value, key) {
  let low = 0,
    high = segments.length - 1;
  while (low < high) {
    const mid = (low + high) >> 1;
    if (value >= segments[mid][key]) low = mid + 1;
    else high = mid;
  }
  return segments[low];
}

export function timeAtPosition(run, position) {
  const value = Math.max(0, Math.min(run.steps.length, position));
  const s = segmentAt(run.playback, value, "to");
  return s.start + ((value - s.from) / (s.to - s.from)) * (s.end - s.start);
}

export function positionAtTime(run, time) {
  const value = Math.max(0, Math.min(run.duration, time));
  const s = segmentAt(run.playback, value, "end");
  return s.from + ((value - s.start) / (s.end - s.start)) * (s.to - s.from);
}

export const advance = (run, position, elapsedMs) =>
  positionAtTime(run, timeAtPosition(run, position) + elapsedMs);

// Begin just after the first output row in the current traversal. A short
// excerpt is enough to show whether B survived in cache between rows.
export function reuseExcerpt(mode) {
  if (!["rows", "blocked"].includes(mode))
    throw new RangeError("Unknown reuse excerpt");
  const start = mode === "blocked" ? 16 : 64;
  return { start, end: start + 16 };
}

// Each product has two input reads and one accumulation. Time is an explanatory
// playback clock, not a latency or performance prediction.
export function frame(trace, position) {
  const complete = position >= trace.steps.length;
  const index = Math.max(
    0,
    Math.min(trace.steps.length - 1, Math.floor(position))
  );
  const step = trace.steps[index];
  const fraction = complete ? 1 : position - index;
  const stage = fraction < 0.36 ? 0 : fraction < 0.72 ? 1 : 2;
  const local =
    stage === 0
      ? fraction / 0.36
      : stage === 1
        ? (fraction - 0.36) / 0.36
        : (fraction - 0.72) / 0.28;
  const access = stage === 0 ? step.a : step.b;
  const arrived = stage === 2 || local >= 0.58;
  const outputArrived = complete || (stage === 2 && local >= 0.93);
  return {
    index,
    step,
    stage,
    local,
    access,
    arrived,
    complete,
    cache: arrived ? access.after : access.before,
    fills: access.fills - (!access.hit && !arrived ? 1 : 0),
    output: outputArrived ? step.after : step.before,
    completed: index + (outputArrived ? 1 : 0),
  };
}
