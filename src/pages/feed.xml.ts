import { getCollection } from "astro:content";
import { getSortedPosts } from "@/utils/getSortedPosts";
import { getPostUrl } from "@/utils/getPostPaths";
import config from "@/config";

const xml = (value: string) => value.replace(/[<>&"']/g, char => ({ "<": "&lt;", ">": "&gt;", "&": "&amp;", '"': "&quot;", "'": "&apos;" })[char]!);

export async function GET() {
  const posts = getSortedPosts(await getCollection("posts")).filter(post => !post.data.draft);
  const updated = new Date(Math.max(...posts.map(post => (post.data.modDatetime ?? post.data.pubDatetime).getTime()), 0)).toISOString();
  const entries = posts.map(post => {
    const { title, description, pubDatetime, modDatetime, legacyId, author } = post.data;
    const url = new URL(getPostUrl(post.id, post.filePath), config.site.url).href;
    return `<entry>
      <title>${xml(title)}</title>
      <id>${xml(legacyId ?? url)}</id>
      <link href="${xml(url)}" />
      <published>${pubDatetime.toISOString()}</published>
      <updated>${(modDatetime ?? pubDatetime).toISOString()}</updated>
      <author><name>${xml(author)}</name></author>
      <summary>${xml(description)}</summary>
      <content type="html">${xml(post.rendered?.html ?? `<p>${xml(description)}</p><p><a href="${xml(url)}">Read the article</a></p>`)}</content>
    </entry>`;
  }).join("\n");
  return new Response(`<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
  <title>${xml(config.site.title)}</title>
  <id>https://kcsongor.github.io/</id>
  <link rel="self" href="${config.site.url}/feed.xml" />
  <link rel="alternate" href="${config.site.url}/" />
  <updated>${updated}</updated>
  <author><name>${xml(config.site.author)}</name></author>
  ${entries}
</feed>`, { headers: { "Content-Type": "application/atom+xml; charset=utf-8" } });
}
