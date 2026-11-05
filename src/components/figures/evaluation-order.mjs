// Addition-only trees illustrate rounding under regrouping. These are not
// the fused multiply-add operations used by a Cachet matrix contraction.
export const exampleValues = [2 ** 25, 0, -(2 ** 25), 1, 0, 0];
export const exampleLabels = ["2²⁵", "0", "−2²⁵", "1", "0", "0"];

export function roundedSum(tree, values = exampleValues) {
  return "k" in tree
    ? Math.fround(values[tree.k])
    : Math.fround(
        roundedSum(tree.left, values) + roundedSum(tree.right, values)
      );
}

// How a value is written in the figure: powers of two as powers, and the
// sign of a zero kept, since rounding can produce either.
export function label(value) {
  const power = Math.log2(Math.abs(value));
  if (value !== 0 && Number.isInteger(power) && power > 4)
    return `${value < 0 ? "−" : ""}2${[...String(power)].map(d => "⁰¹²³⁴⁵⁶⁷⁸⁹"[d]).join("")}`;
  if (Object.is(value, -0)) return "−0";
  return String(value).replace("-", "−");
}

// Each addition has two children and the step it runs at. Additions at the
// same step are independent, so they can run at the same time.
const leaf = k => ({ k, x: k, step: 0 });
const add = (left, right) => ({
  left,
  right,
  x: (left.x + right.x) / 2,
  step: Math.max(left.step, right.step) + 1,
});

export const chain = k =>
  Array.from({ length: k }, (_, t) => leaf(t)).reduce(add);

export const grouped = (k, block) => {
  const leaves = Array.from({ length: k }, (_, t) => leaf(t));
  const blocks = [];
  for (let t = 0; t < k; t += block)
    blocks.push(leaves.slice(t, t + block).reduce(add));
  return blocks.reduce(add);
};

export const additions = tree =>
  "k" in tree ? [] : [...additions(tree.left), ...additions(tree.right), tree];
