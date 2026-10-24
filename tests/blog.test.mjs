import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { cacheLines, plotPath } from "../src/components/figures/cache-lines.mjs";
import { legacyHeadingAliases } from "../src/syntax/legacy-headings.mjs";

const legacy = JSON.parse(readFileSync(new URL("./legacy-posts.json", import.meta.url), "utf8"));
test("all migrated posts retain their publication dates and exact code samples", () => {
  assert.equal(legacy.length, 12);
  for (const post of legacy) {
    const source = readFileSync(new URL(`../src/content/posts/${post.slug}.md`, import.meta.url), "utf8");
    assert(source.includes(`pubDatetime: ${post.published}T00:00:00Z`));
    assert(!/\{%|\{: \.notice/.test(source));
    const hashes = [...source.matchAll(/^```\w+\n(.*?)^```/gms)].map(([, code]) => createHash("sha256").update(code.replace(/\n+$/, "")).digest("hex"));
    assert.deepEqual(hashes, post.codeHashes, post.slug);
  }
});
test("cache-line model covers packed, sparse and empty arrays", () => {
  assert.deepEqual(cacheLines(0, 8), { aos: 0, soa: 0 });
  assert.deepEqual(cacheLines(16, 1), { aos: 1, soa: 1 });
  assert.deepEqual(cacheLines(17, 1), { aos: 2, soa: 2 });
  assert.deepEqual(cacheLines(256, 8), { aos: 128, soa: 16 });
  assert.deepEqual(cacheLines(256, 16), { aos: 256, soa: 16 });
  assert.deepEqual(cacheLines(256, 32), { aos: 256, soa: 16 });
  assert(plotPath(8, "aos").endsWith("L608.00,138.00"));
});
test("historical heading aliases preserve new anchors without duplicate IDs", () => {
  const tree = { type: "root", children: [{ type: "element", tagName: "h2", properties: { id: "how-new" }, children: [{ type: "text", value: "How?" }] }] };
  const transform = legacyHeadingAliases();
  transform(tree, { path: "/posts/time-travel-in-haskell-for-dummies.md" });
  assert.equal(tree.children[0].properties.id, "how-new");
  assert.equal(tree.children[0].children[0].properties.id, "how");
  transform(tree, { path: "/posts/time-travel-in-haskell-for-dummies.md" });
  assert.equal(tree.children[0].children.length, 2);
});

test("historical IDs are assigned before Astro generates its table of contents", () => {
  const heading = { type: "element", tagName: "h2", properties: {}, children: [{ type: "text", value: "How?" }] };
  legacyHeadingAliases()({ type: "root", children: [heading] }, { path: "/posts/time-travel-in-haskell-for-dummies.md" });
  assert.equal(heading.properties.id, "how");
  assert.equal(heading.children.length, 1);
});
