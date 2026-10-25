const element = (tagName, properties, children) => ({ type: "element", tagName, properties, children });
const walk = (node, visit) => {
  visit(node);
  for (const child of node.children ?? []) walk(child, visit);
};
const has = (node, property) => Object.hasOwn(node.properties ?? {}, property);

/** Add a linked margin copy; the browser chooses one presentation at a time. */
export function footnoteSidenotes() {
  return tree => {
    const definitions = new Map();
    walk(tree, node => {
      if (!has(node, "dataFootnotes")) return;
      walk(node, child => {
        if (child.tagName === "li" && child.properties?.id) {
          definitions.set(child.properties.id, child);
        }
      });
    });
    const used = new Set();
    const ids = new Set();
    walk(tree, node => { if (node.properties?.id) ids.add(node.properties.id); });
    const copyContent = node => {
      const copy = structuredClone(node);
      const clean = child => {
        if (child.properties) delete child.properties.id;
        if (child.children) {
          child.children = child.children.filter(item => !has(item, "dataFootnoteBackref"));
          child.children.forEach(clean);
        }
      };
      clean(copy);
      return copy.children;
    };
    const insert = parent => {
      if (!parent.children || has(parent, "dataFootnotes")) return;
      const children = [];
      for (const child of parent.children) {
        // Paragraphs also cover references inside lists, blockquotes and MDX wrappers.
        if (child.type === "element" && /^(p|h[1-6])$/.test(child.tagName)) {
          walk(child, ref => {
            if (!has(ref, "dataFootnoteRef")) return;
            const href = ref.properties.href;
            if (typeof href !== "string" || !href.startsWith("#")) return;
            let id;
            try { id = decodeURIComponent(href.slice(1)); } catch { return; }
            const definition = definitions.get(id);
            if (!definition || used.has(id)) return;
            used.add(id);
            let marginId = `margin-${id}`;
            while (ids.has(marginId)) marginId += "-note";
            ids.add(marginId);
            definition.properties.dataSidenote = marginId;
            children.push(element("aside", {
              id: marginId,
              dataFootnoteId: id,
              tabIndex: -1,
              className: ["callout", "footnote-sidenote"],
              "data-callout": "note",
              "data-pagefind-ignore": "all",
              role: "note",
            }, [
              element("p", { className: ["callout-title"] }, [
                element("a", { href: `#${ref.properties.id}`, ariaLabel: `Back to reference ${ref.children.map(n => n.value ?? "").join("")}` }, [
                  ...structuredClone(ref.children),
                ]),
              ]),
              element("div", { className: ["callout-content"] }, copyContent(definition)),
            ]));
          });
        } else {
          insert(child);
        }
        children.push(child);
      }
      parent.children = children;
    };
    insert(tree);
    walk(tree, node => {
      if (!has(node, "dataFootnotes")) return;
      const items = [];
      walk(node, child => { if (definitions.has(child.properties?.id)) items.push(child); });
      if (items.length && items.every(item => has(item, "dataSidenote"))) {
        node.properties.dataAllSidenotes = "";
      }
    });
  };
}
