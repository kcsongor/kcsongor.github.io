import test from "node:test";
import assert from "node:assert/strict";
import { createHighlighter } from "shiki";
import grammar from "../src/syntax/cachet.tmLanguage.json" with { type: "json" };
import cachet from "../src/syntax/cachet.mjs";
import { monochromeLight, monochromeDark } from "../src/syntax/monochrome.ts";
import { rehypeInlineCode } from "../src/syntax/inline-code.mjs";
import { transformerCachetTypography } from "../src/utils/transformers/cachetTypography.mjs";

const themes = { light: monochromeLight, dark: monochromeDark };
const text = node =>
  node.type === "text" ? node.value : (node.children ?? []).map(text).join("");

test("a reloaded Cachet grammar updates out in an already-used highlighter", async () => {
  const old = structuredClone(grammar);
  old.patterns.find(pattern => pattern.name === "keyword.control.cachet").match =
    old.patterns
      .find(pattern => pattern.name === "keyword.control.cachet")
      .match.replace("|out|", "|");
  const highlighter = await createHighlighter({
    langs: [old],
    themes: Object.values(themes),
  });
  try {
    const styles = () => {
      const tokens = highlighter.codeToTokens("reads out output", {
        lang: "cachet", themes, defaultColor: false,
      }).tokens[0];
      return Object.fromEntries(tokens.map(token => [token.content.trim(), token.htmlStyle]));
    };
    const before = styles();
    assert.notDeepEqual(before.out, before.reads);
    await highlighter.loadLanguage(cachet);
    const after = styles();
    assert.deepEqual(after.out, after.reads);
    assert.equal(after.out["--shiki-light-font-weight"], "bold");
    assert.equal(after.out["--shiki-dark-font-weight"], "bold");
    assert.notDeepEqual(after.output, after.out);

    const source = "fn product (reads a: [f32; 2]) -> out c: [f32; 1]";
    const tree = highlighter.codeToHast(source, {
      lang: "cachet", themes, defaultColor: false,
      transformers: [transformerCachetTypography()],
    });
    assert.equal(text(tree), source);
    assert(JSON.stringify(tree).includes("cachet-arrow-source"));
  } finally {
    highlighter.dispose();
  }
});

test("inline arrows preserve the copied source and highlight out", async () => {
  const code = {
    type: "element", tagName: "code",
    properties: { dataInlineLanguage: "cachet" },
    children: [{ type: "text", value: "-> out c" }],
  };
  await rehypeInlineCode({ langs: [cachet], themes })({ type: "root", children: [code] });
  assert.equal(text(code), "-> out c");
  assert(JSON.stringify(code).includes("cachet-arrow-source"));
  const out = code.children.find(node => text(node).trim() === "out");
  // Bold in both themes, by the token class that carries those properties.
  assert([out.properties.class].flat().join(" ").split(" ").includes("tk-strong-bold"));
  assert.equal(out.properties.style, undefined);
});
