import test from "node:test";
import assert from "node:assert/strict";
import { createMarkdownProcessor } from "@astrojs/markdown-remark";
import { footnoteSidenotes } from "../src/syntax/footnote-sidenotes.mjs";

const processor = await createMarkdownProcessor({ rehypePlugins: [footnoteSidenotes] });
const render = async source => (await processor.render(source)).code;

test("margin copies retain rich content without duplicating footnote IDs or backlinks", async () => {
  const html = await render('First[^one]. Again[^one].\n\n[^one]: A **bold** note with `code` and a [link](https://example.com).\n\n    Second paragraph.');
  const notes = [...html.matchAll(/<aside\b[\s\S]*?<\/aside>/g)].map(m => m[0]);
  assert.equal(notes.length, 1);
  assert(notes[0].includes('<strong>bold</strong>'));
  assert(notes[0].includes('<code>code</code>'));
  assert(notes[0].includes('href="https://example.com"'));
  assert(notes[0].includes('Second paragraph.'));
  assert(!notes[0].includes('data-footnote-backref'));
  assert.equal((notes[0].match(/ id=/g) ?? []).length, 1);
  assert(notes[0].includes('id="margin-user-content-fn-one"'));
  assert(notes[0].includes('data-footnote-id="user-content-fn-one"'));
  assert(notes[0].includes('href="#user-content-fnref-one"'));
  assert(notes[0].includes('aria-label="Back to reference 1">1</a>'));
  assert(html.indexOf('<aside') < html.indexOf('<p>First'));
  assert(html.includes('data-footnotes'));
  assert(html.includes('data-all-sidenotes'));
  assert(html.includes('data-sidenote="margin-user-content-fn-one"'));
  const ids = [...html.matchAll(/ id="([^"]+)"/g)].map(m => m[1]);
  assert.equal(new Set(ids).size, ids.length);
  assert.equal((html.match(/ data-footnote-backref=/g) ?? []).length, 2);
});

test("multiple notes follow their first references, including nested paragraphs", async () => {
  const html = await render('> Quoted[^two].\n\nNext[^one].\n\n[^one]: First definition.\n[^two]: Second definition.');
  assert.equal((html.match(/<aside/g) ?? []).length, 2);
  assert(html.indexOf('Second definition.') < html.indexOf('First definition.'));
  assert(html.includes('<blockquote>\n<aside'));
});

test("articles without footnotes are unchanged", async () => {
  assert.equal(await render('Just a paragraph.'), '<p>Just a paragraph.</p>');
});
