import test from "node:test";
import assert from "node:assert/strict";
import { createMarkdownProcessor } from "@astrojs/markdown-remark";
import { remarkInlineCode, rehypeInlineCode } from "../src/syntax/inline-code.mjs";
import { monochromeLight, monochromeDark } from "../src/syntax/monochrome.ts";
import cachet from "../src/syntax/cachet.tmLanguage.json" with { type: "json" };

const themes = { light: monochromeLight, dark: monochromeDark };
const processor = await createMarkdownProcessor({
  shikiConfig: { langs: [cachet], themes, defaultColor: false },
  remarkPlugins: [remarkInlineCode],
  rehypePlugins: [[rehypeInlineCode, { langs: [cachet], themes }]],
});
const render = async (source, frontmatter = {}) => (await processor.render(source, { frontmatter })).code;
const inlineLanguages = html => [...html.matchAll(/class="shiki-inline" data-language="([^"]+)"/g)].map(match => match[1]);

test("inline languages follow fences, including initial prose, but skip console output", async () => {
  const html = await render('`Just 1`\n\n```haskell\nJust 1\n```\n\n`Nothing`\n\n```rust\nlet x = 1;\n```\n\n`Some(1)`\n\n```txt\noutput\n```\n\n`None`');
  assert.deepEqual(inlineLanguages(html), ["haskell", "haskell", "rust", "rust"]);
  assert.equal((html.match(/<pre/g) ?? []).length, 3);
});

test("frontmatter fixes the language; snippet overrides and plaintext opt-out work", async () => {
  const html = await render('`Just 1`\n\n```rust\nlet x = 1;\n```\n\n`Nothing` `Some(1){:rust}` `path/file{:text}`', { inlineCodeLanguage: "haskell" });
  assert.deepEqual(inlineLanguages(html), ["haskell", "haskell", "rust"]);
  assert(html.includes('<code>path/file</code>'));
  assert(!html.includes("{:"));
});

test("custom Cachet grammar emits both themes and safely escapes inline HTML", async () => {
  const html = await render('`"hello" < 42{:cachet}` and `"<script>alert(1)</script>"{:rust}`');
  assert.deepEqual(inlineLanguages(html), ["cachet", "rust"]);
  assert(html.includes("--shiki-light:#B22222"));
  assert(html.includes("--shiki-dark:#00AF87"));
  assert(!html.includes("<script>"));
  assert(!html.includes("<pre"));
  assert(html.includes("&#x3C;script>"));
});

test("heading text and IDs exclude annotations; unknown languages fail clearly", async () => {
  const html = await render('## Using `Some{:rust}`');
  assert(html.includes('id="using-some"'));
  assert(!html.includes("{:rust}"));
  assert.equal(await render('`unmarked`'), '<p><code>unmarked</code></p>');
  await assert.rejects(render('`x{:not-a-language}`'), /Unknown inline code language/);
});
