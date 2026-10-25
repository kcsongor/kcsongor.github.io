import assert from "node:assert/strict";
import { readFileSync, readdirSync, existsSync } from "node:fs";
import { join, relative } from "node:path";
import { createHash } from "node:crypto";

const dist = "dist";
const legacy = JSON.parse(readFileSync("tests/legacy-posts.json", "utf8"));
const decode = value => value.replace(/&(#x[0-9a-f]+|#\d+|amp|lt|gt|quot|apos|nbsp);/gi, (_, entity) => {
  if (entity.startsWith("#x")) return String.fromCodePoint(parseInt(entity.slice(2), 16));
  if (entity.startsWith("#")) return String.fromCodePoint(Number(entity.slice(1)));
  return { amp: "&", lt: "<", gt: ">", quot: '"', apos: "'", nbsp: "\u00a0" }[entity];
});
const hash = value => createHash("sha256").update(value.replace(/\n+$/, "")).digest("hex");
const idList = html => [...html.matchAll(/\sid="([^"]+)"/g)].map(match => decode(match[1]));
const ids = html => new Set(idList(html));
let blockCount = 0;
for (const post of legacy) {
  const html = readFileSync(join(dist, post.slug, "index.html"), "utf8");
  assert(html.includes(`href="https://blog.csongor.co.uk/${post.slug}/"`), `${post.slug}: canonical URL`);
  assert(decode(html).includes(post.title), `${post.slug}: title`);
  assert(html.includes(post.published), `${post.slug}: publication date`);
  const anchors = ids(html);
  assert.equal(anchors.size, idList(html).length, `${post.slug}: duplicate IDs`);
  for (const heading of post.headings) assert(anchors.has(heading), `${post.slug}: missing old #${heading}`);
  const codes = [...html.matchAll(/<pre\b[^>]*>[\s\S]*?<code\b[^>]*>([\s\S]*?)<\/code>[\s\S]*?<\/pre>/g)]
    .map(match => decode(match[1].replace(/<[^>]*>/g, "")));
  assert.deepEqual(codes.map(hash), post.codeHashes, `${post.slug}: code blocks changed`);
  blockCount += codes.length;
  assert(!/\{%|\{: \.notice/.test(html), `${post.slug}: unconverted Jekyll syntax`);
}
function* files(dir) {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) yield* files(path);
    else yield path;
  }
}
// Verify actual generated paths and fragments, including navigation and footnotes.
const siteOrigins = new Set(["https://blog.csongor.co.uk", "http://blog.csongor.co.uk", "https://kcsongor.github.io", "http://kcsongor.github.io"]);
for (const path of files(dist)) {
  if (!path.endsWith(".html")) continue;
  const html = readFileSync(path, "utf8");
  const pageUrl = new URL(relative(dist, path).replace(/index\.html$/, ""), "https://blog.csongor.co.uk/");
  for (const [, href] of html.matchAll(/<a\b[^>]*\bhref="([^"]*)"/g)) {
    const url = new URL(decode(href), pageUrl);
    if (!siteOrigins.has(url.origin)) continue;
    const pathname = decodeURIComponent(url.pathname);
    let target = join(dist, pathname);
    if (pathname.endsWith("/") || !/\.[^/]+$/.test(pathname)) target = join(target, "index.html");
    assert(existsSync(target), `${path}: broken link ${href}`);
    if (url.hash && target.endsWith(".html")) {
      assert(ids(readFileSync(target, "utf8")).has(decodeURIComponent(url.hash.slice(1))), `${path}: broken fragment ${href}`);
    }
  }
}
for (const path of ["index.html", "posts/index.html", "404.html", "feed.xml", "rss.xml", "sitemap-index.xml", "robots.txt", "pagefind/pagefind.js", "CNAME"]) {
  assert(existsSync(join(dist, path)), `Missing ${path}`);
}
assert.equal(readFileSync(join(dist, "CNAME"), "utf8").trim(), "blog.csongor.co.uk");
assert(!existsSync(join(dist, "interactive-example")), "Draft example leaked into production");
// The public RUNBOOK download shares this directory with the post's route.
assert(existsSync(join(dist, "loom/index.html")), "Missing Loom post");
assert(existsSync(join(dist, "loom/RUNBOOK.md")), "Missing Loom RUNBOOK");
const feed = readFileSync(join(dist, "feed.xml"), "utf8");
for (const post of legacy) assert(feed.includes(`/${post.slug}/`), `${post.slug}: missing from feed`);
assert(!feed.includes("interactive-example"), "Draft leaked into feed");
assert(feed.includes("/loom/"), "Loom post missing from feed");
console.log(`Verified ${legacy.length} legacy URLs, ${blockCount} code blocks, historical headings, internal links, feeds, search output and draft exclusion.`);
