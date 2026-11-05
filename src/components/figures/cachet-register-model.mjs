export const ROWS = 4;
export const LANES = 4;
export const PIPES = 4;
export const LATENCY = 4;
export const WIDTHS = [8, 16];

export function configuration(columns) {
  if (!WIDTHS.includes(columns)) throw new RangeError("Choose 8 or 16 columns");
  const vectors = (ROWS * columns) / LANES;
  const issueCycles = vectors / PIPES;
  return { columns, vectors, issueCycles, utilisation: issueCycles / LATENCY };
}

// Four independent rows issue together. A column group is revisited only
// after four cycles, when its previous fused multiply-add has completed.
// Negative cycles represent the previous repetition in steady state.
export function issuedGroup(columns, cycle) {
  const phase = ((cycle % LATENCY) + LATENCY) % LATENCY;
  return phase < configuration(columns).issueCycles ? phase : null;
}

export function inFlight(columns, time) {
  const packets = [];
  for (
    let cycle = Math.floor(time) - LATENCY + 1;
    cycle <= Math.floor(time);
    cycle++
  ) {
    const group = issuedGroup(columns, cycle);
    if (group === null) continue;
    for (let row = 0; row < ROWS; row++) {
      packets.push({
        id: `${cycle}-${row}`,
        cycle,
        row,
        group,
        accumulator: group * ROWS + row,
        progress: (time - cycle) / LATENCY,
      });
    }
  }
  return packets;
}

export function arrival(columns, time) {
  const cycle = Math.floor(time);
  const group = issuedGroup(columns, cycle - LATENCY);
  const fraction = time - cycle;
  return { group, glow: group === null ? 0 : Math.max(0, 1 - fraction / 0.6) };
}
