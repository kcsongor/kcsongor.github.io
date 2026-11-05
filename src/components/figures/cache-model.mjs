/** A matrix stored row by row, read through a small cache that evicts the
    least recently used line. A hit takes one tick; a miss stalls for `missCost`. */
export const cacheModel = { rows: 8, cols: 8, lineSize: 4, capacity: 4, missCost: 8 };

export const lineOf = (model, row, col) =>
  Math.floor((row * model.cols + col) / model.lineSize);

const walks = {
  rows: ({ rows, cols }) =>
    Array.from({ length: rows * cols }, (_, n) => [Math.floor(n / cols), n % cols]),
  columns: ({ rows, cols }) =>
    Array.from({ length: rows * cols }, (_, n) => [n % rows, Math.floor(n / rows)]),
};

/** Every read of a walk, with the lines cached after it, oldest first. */
export function readsOf(model, walk) {
  let cached = [];
  return walks[walk](model).map(([row, col]) => {
    const line = lineOf(model, row, col);
    const miss = !cached.includes(line);
    cached = [...cached.filter(other => other !== line), line].slice(-model.capacity);
    return { row, col, line, miss, cached, cost: miss ? model.missCost : 1 };
  });
}

export const totalTime = reads => reads.reduce((time, read) => time + read.cost, 0);
