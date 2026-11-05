import { createHighlighter } from "shiki";
import cachet from "../../syntax/cachet.mjs";
import { monochromeLight, monochromeDark } from "../../syntax/monochrome";
import { transformerCachetTypography } from "../../utils/transformers/cachetTypography.mjs";
import { transformerTokenClasses } from "../../utils/transformers/tokenClasses.mjs";

let highlighter;
const shared = () =>
  (highlighter ??= createHighlighter({
    langs: [cachet],
    themes: [monochromeLight, monochromeDark],
  }));

// The lines of `code` that `previous` doesn't have, ignoring indentation, so
// that moving a loop inward doesn't mark everything inside it. A loop's own
// header keeps its indentation, so a loop that moves is marked.
const key = text => (/^\s*for /.test(text) ? text : text.trim());
const changedLines = (code, previous) => {
  if (previous === undefined) return new Set();
  const left = new Map();
  for (const line of previous.split("\n").map(key))
    left.set(line, (left.get(line) ?? 0) + 1);
  const changed = new Set();
  code.split("\n").forEach((text, index) => {
    if (text.trim() === "") return;
    const line = key(text);
    if (left.get(line)) left.set(line, left.get(line) - 1);
    else changed.add(index);
  });
  return changed;
};

// `marks` gives lines attributes by their index, to link steps to their code.
// `changed`, where given, says which lines changed instead of `previous`.
export async function listing(code, previous, marks = new Map(), changed) {
  const shiki = await shared();
  const lines = code.split("\n");
  changed ??= changedLines(code, previous);
  return shiki.codeToHtml(code, {
    lang: "cachet",
    themes: { light: monochromeLight.name, dark: monochromeDark.name },
    defaultColor: false,
    transformers: [
      transformerCachetTypography(),
      {
        pre(node) {
          this.addClassToHast(node, "astro-code");
          node.properties.tabindex = 0;
          node.properties["data-copy-managed"] = "";
        },
        line(node, number) {
          const mark = marks.get(number - 1);
          node.properties["data-line-number"] = number;
          const indent = lines[number - 1].match(/^\s*/)[0].length;
          node.properties.style = `--continuation-indent: ${indent + 2}ch`;
          // A step that isn't in effect yet hasn't changed, whatever was before.
          if (changed.has(number - 1) && !(mark && "data-future" in mark))
            this.addClassToHast(node, "is-changed");
          Object.assign(node.properties, mark);
        },
      },
      transformerTokenClasses(),
    ],
  });
}
