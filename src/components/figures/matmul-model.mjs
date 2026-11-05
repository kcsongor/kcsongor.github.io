// Each entry is one scalar multiply-add, with two operand streams.
export function productsFor(mode, step, { m, k, n }, output = 0) {
  if (mode === "scalar")
    return [
      {
        i: Math.floor(output / n),
        j: output % n,
        k: step,
        group: step < k / 2 ? 0 : 1,
        delay: 0,
      },
    ];
  if (mode === "split")
    return step < 2
      ? Array.from({ length: 3 }, (_, group) => ({
          i: 1,
          j: 3,
          k: group * 2 + step,
          group,
          delay: 0,
        }))
      : [];
  const ks =
    mode === "blocks" ? [step * 3, step * 3 + 1, step * 3 + 2] : [step];
  return ks.flatMap((kk, q) =>
    Array.from({ length: m * n }, (_, out) => ({
      i: Math.floor(out / n),
      j: out % n,
      k: kk,
      group: kk < k / 2 ? 0 : 1,
      delay: q * 220,
    }))
  );
}
export const stepCount = (mode, k) =>
  mode === "split" ? 3 : mode === "blocks" ? 2 : k;

// Reconstruct the whole output matrix, not just the cell under the cursor.
// Revisiting a finished cell keeps all its terms; rewinding is explicit.
export function scalarHistory(progress, dims, output, step) {
  const counts = new Map(progress);
  counts.set(output, Math.max(counts.get(output) || 0, step));
  return [...counts].flatMap(([cell, count]) =>
    Array.from(
      { length: Math.min(count, dims.k) },
      (_, k) => productsFor("scalar", k, dims, cell)[0]
    )
  );
}
