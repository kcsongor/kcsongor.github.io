// Show the declaration and the steps around the current rewrite. Keep source
// line identities so highlighting still follows the saved compiler stages.
/**
 * @param {{ source: string, current: number | null, future: number[] }} stage
 * @param {{ source: string }} [previous]
 * @returns {{ code: string, current: number[], future: number[] }}
 */
export function inlineSource(stage, previous) {
  const lines = stage.source.split("\n");
  const start = lines.findIndex((line) => line.startsWith("schedule "));
  const body =
    lines.findIndex((line, i) => i >= start && line.endsWith("=")) + 1;
  if (start < 0 || body <= start)
    throw new Error("Missing schedule declaration");
  const steps = lines
    .map((_, i) => i)
    .filter((i) => i >= body && lines[i].trim());
  const old = new Set(previous?.source.split("\n").map((line) => line.trim()));
  const current =
    stage.current === null
      ? previous
        ? steps.filter((i) => !old.has(lines[i].trim()))
        : []
      : [stage.current];
  const visible = new Set();
  for (const line of current.length ? current : [steps[0]]) {
    const at = steps.indexOf(line);
    for (const i of steps.slice(Math.max(0, at - 1), at + 2)) visible.add(i);
  }
  const kept = lines
    .slice(start, body)
    .map((text, i) => ({ text, original: start + i }));
  for (const i of steps) {
    if (steps.length <= 6 || visible.has(i))
      kept.push({ text: lines[i], original: i });
    else if (kept.at(-1).original !== null)
      kept.push({ text: "  ...", original: null, before: i });
  }
  return {
    code: kept.map((line) => line.text).join("\n"),
    current: kept.flatMap((line, i) =>
      current.includes(line.original) ? [i] : [],
    ),
    future: kept.flatMap((line, i) =>
      stage.future.includes(line.original) ? [i] : [],
    ),
    kept,
  };
}

/**
 * One listing for each run of stages that show the same schedule source, in
 * place of a listing per stage: every line some stage of the run shows, the
 * `...` that stands for hidden steps included, each with the stages that show
 * it, the stages whose rewrite it is, and the stages it is yet to come in.
 * `inlineSource` still decides what each stage shows; this only merges them.
 * @param {{ source: string, current: number | null, future: number[] }[]} stages
 * @returns {{ stages: number[], code: string, lines: { shown: number[], current: number[], future: number[] }[] }[]}
 */
export function sharedSource(stages) {
  const runs = [];
  stages.forEach((stage, index) => {
    const shown = inlineSource(stage, stages[index - 1]);
    const run = runs.at(-1);
    if (run && run.source === stage.source) run.shown.set(index, shown);
    else runs.push({ source: stage.source, shown: new Map([[index, shown]]) });
  });
  return runs.map(({ source, shown }) => {
    const text = source.split("\n");
    // A line of the source by its index, and a `...` by the step it stands
    // before, in source order with each `...` ahead of its step.
    const entries = new Map();
    for (const [index, { kept, current, future }] of shown)
      kept.forEach((line, i) => {
        const key = line.original ?? line.before - 0.5;
        const entry = entries.get(key) ?? {
          key,
          text: line.original === null ? line.text : text[line.original],
          shown: [],
          current: [],
          future: [],
        };
        entry.shown.push(index);
        if (current.includes(i)) entry.current.push(index);
        if (future.includes(i)) entry.future.push(index);
        entries.set(key, entry);
      });
    const lines = [...entries.values()].sort((a, b) => a.key - b.key);
    return {
      stages: [...shown.keys()],
      code: lines.map((line) => line.text).join("\n"),
      lines,
    };
  });
}
