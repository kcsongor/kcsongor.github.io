/** Cold, aligned arrays of four-byte fields, reading field zero of every record. */
export function cacheLines(records, fields) {
  const touched = new Set();
  for (let i = 0; i < records; i++) touched.add(Math.floor((i * fields * 4) / 64));
  return { aos: touched.size, soa: Math.ceil((records * 4) / 64) };
}

export function plotPath(fields, layout) {
  return Array.from({ length: 257 }, (_, records) => {
    const x = 48 + (records / 256) * 560;
    const y = 250 - (cacheLines(records, fields)[layout] / 256) * 224;
    return `${records === 0 ? "M" : "L"}${x.toFixed(2)},${y.toFixed(2)}`;
  }).join(" ");
}
