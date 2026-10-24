import { basename } from "node:path";
import legacy from "./legacy-headings.json" with { type: "json" };

/** Preserve Kramdown section IDs before Astro collects headings for the TOC. */
export function legacyHeadingAliases() {
  return (tree, file) => {
    const slug = basename(file.path ?? "", ".md");
    const previous = legacy[slug];
    if (!previous) return;
    const normalize = value => value.replace(/[‘’]/g, "'").replace(/[“”]/g, '"').replace(/\s+/g, " ").trim();
    const text = node => node.type === "text" ? node.value : (node.children ?? []).map(text).join("");
    const ids = new Set();
    const walk = (node, fn) => { fn(node); for (const child of node.children ?? []) walk(child, fn); };
    walk(tree, node => { if (node.properties?.id) ids.add(node.properties.id); });
    const remaining = [...previous];
    walk(tree, node => {
      if (node.type !== "element" || !/^h[1-6]$/.test(node.tagName)) return;
      const index = remaining.findIndex(heading => normalize(heading.text) === normalize(text(node)));
      if (index < 0) return;
      const [{ id }] = remaining.splice(index, 1);
      if (ids.has(id)) return;
      ids.add(id);
      node.properties ??= {};
      if (!node.properties.id) {
        node.properties.id = id;
        return;
      }
      // Preserve an explicitly assigned new ID as well as its historical alias.
      node.children.unshift({ type: "element", tagName: "span", properties: { id, className: ["legacy-anchor"], ariaHidden: "true" }, children: [] });
    });
  };
}
