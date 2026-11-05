import snapshot from "./cachet-benchmarks.json" with { type: "json" };

export const number = (value, digits = 0) =>
  value.toLocaleString("en-GB", { maximumFractionDigits: digits });

export function measurement(reference) {
  const [run, kernel] = reference.split(".");
  const result = snapshot.runs[run]?.measurements[kernel];
  if (!result) throw new Error(`Unknown benchmark: ${reference}`);
  return result;
}

export function metric(run, kernel, kind = "rate", other, digits = 0) {
  const value = measurement(`${run}.${kernel}`);
  const baseline = other && measurement(`${run}.${other}`);
  let result;
  if (kind === "rate") result = value.gflops;
  else if (kind === "speedup" && baseline)
    result = baseline.seconds / value.seconds;
  else if (kind === "percent" && baseline)
    result = (100 * value.gflops) / baseline.gflops;
  else throw new Error(`Unknown benchmark comparison: ${kind}`);
  return number(result, digits);
}

export const rate = value => `${number(value, value < 10 ? 2 : 0)} GFLOP/s`;
export const duration = seconds => `${number(seconds * 1000, 2)} ms`;

function workloadDescription(key) {
  const run = snapshot.runs[key];
  const description = run
    ? `${run.size} × ${run.size} · ${run.precision}`
    : snapshot.workloads?.[key]?.description;
  if (!description) throw new Error(`Unknown workload: ${key}`);
  return description;
}

function resolveTiming(entry, label) {
  const placeholder = entry.placeholderSeconds !== undefined;
  if (placeholder === Boolean(entry.measurement))
    throw new Error(
      `Timing needs either a measurement or a placeholder: ${label}`
    );
  const recorded = placeholder ? null : measurement(entry.measurement);
  const seconds = placeholder ? entry.placeholderSeconds : recorded.seconds;
  if (!(Number.isFinite(seconds) && seconds > 0))
    throw new Error(`Invalid duration: ${label}`);
  const run = snapshot.runs[entry.workload];
  const workload = workloadDescription(entry.workload);
  if (!placeholder && entry.measurement.split(".")[0] !== entry.workload)
    throw new Error(`Measurement and workload differ: ${label}`);
  const gflops =
    recorded?.gflops ?? (run ? (2 * run.size ** 3) / seconds / 1e9 : null);
  return {
    seconds,
    gflops,
    duration: duration(seconds),
    rate: gflops === null ? null : rate(gflops),
    workload,
    placeholder,
    source: placeholder ? null : (recorded.source ?? run.source),
  };
}

export function target(key) {
  const entry = snapshot.targets[key];
  if (!entry) throw new Error(`Unknown library target: ${key}`);
  return {
    ...entry,
    ...resolveTiming(entry, `target ${key}`),
    workloadKey: entry.workload,
    key,
  };
}

export function comparison(entry, targetKey, minimumScale = 110) {
  const value = resolveTiming(entry, entry.measurement ?? "placeholder");
  const goal = target(targetKey);
  if (entry.workload !== goal.workloadKey)
    throw new Error(
      `Timing and target workloads differ: ${entry.workload}/${targetKey}`
    );
  // Equal workloads: throughput is inversely proportional to elapsed time.
  // Keep percentages above 100: a library result is a target we can beat.
  const percent = (100 * goal.seconds) / value.seconds;
  const scale = Math.max(minimumScale, Math.ceil(percent / 10) * 10);
  return {
    ...value,
    context: `${entry.target ?? goal.hardware} · ${value.workload}`,
    comparison: {
      target: goal,
      percent,
      percentLabel:
        percent < 0.1 ? "<0.1%" : `${number(percent, percent < 10 ? 1 : 0)}%`,
      scale,
      width: (100 * percent) / scale,
      targetPosition: 10000 / scale,
      placeholder: value.placeholder || goal.placeholder,
    },
  };
}

// Whether a schedule's steps have timings: one whose measurement says
// nothing about its speed, such as attention at a size launch latency
// dominates, has none and shows no bars.
export const hasTimings = schedule => schedule in snapshot.stages;

export function timing(schedule, index, title) {
  const entries = snapshot.stages[schedule];
  const entry = entries?.[index];
  if (!entry || (title && entry.step !== title))
    throw new Error(`Update timing for ${schedule} step ${index}: ${title}`);
  const targetKey = snapshot.scheduleTargets[schedule];
  // A shared scale prevents the reference marker moving between steps.
  const scale = Math.max(
    ...entries.map(value => comparison(value, targetKey).comparison.scale)
  );
  return comparison(entry, targetKey, scale);
}

export function referenceTiming(reference, targetKey) {
  return comparison(
    {
      measurement: reference,
      workload: reference.split(".")[0],
    },
    targetKey
  );
}
