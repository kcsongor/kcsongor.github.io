import { bundledLanguages, createHighlighter } from "shiki";

const plainLanguages = new Set(["text", "txt", "plain", "plaintext"]);
const supported = lang => lang === "cachet" || Object.hasOwn(bundledLanguages, lang) || plainLanguages.has(lang);
const normalize = lang => plainLanguages.has(lang) ? "text" : lang;

function visit(node, callback) {
  callback(node);
  for (const child of node.children ?? []) visit(child, callback);
}

/** Carry the surrounding fence language through Markdown/MDX into HTML. */
export function remarkInlineCode() {
  return (tree, file) => {
    const preferred = file.data.astro?.frontmatter?.inlineCodeLanguage;
    if (preferred && !supported(preferred)) file.fail(`Unknown inline code language: ${preferred}`);

    let firstLanguage;
    visit(tree, node => {
      if (!firstLanguage && node.type === "code" && supported(node.lang) && !plainLanguages.has(node.lang)) {
        firstLanguage = node.lang;
      }
    });
    let currentLanguage = preferred ?? firstLanguage;
    visit(tree, node => {
      // Console output should not change the surrounding programming language.
      if (!preferred && node.type === "code" && supported(node.lang) && !plainLanguages.has(node.lang)) {
        currentLanguage = node.lang;
      }
      if (node.type !== "inlineCode") return;
      const annotation = node.value.match(/\{:(\w[\w+-]*)\}$/);
      const language = annotation?.[1] ?? currentLanguage;
      if (annotation) {
        if (!supported(language)) file.fail(`Unknown inline code language: ${language}`, node);
        node.value = node.value.slice(0, -annotation[0].length);
      }
      if (!language || plainLanguages.has(language)) return;
      node.data ??= {};
      node.data.hProperties = { ...node.data.hProperties, "dataInlineLanguage": normalize(language) };
    });
  };
}

/** Build-time highlighting with the same themes and token styling as blocks. */
export function rehypeInlineCode({ langs = [], themes }) {
  let highlighterPromise;
  return async tree => {
    const nodes = [];
    visit(tree, node => {
      if (node.type === "element" && node.tagName === "code" && node.properties?.["dataInlineLanguage"]) nodes.push(node);
    });
    if (!nodes.length) return;
    const highlighter = await (highlighterPromise ??= createHighlighter({ langs, themes: Object.values(themes) }));
    const languages = [...new Set(nodes.map(node => node.properties["dataInlineLanguage"]))];
    const loaded = new Set(highlighter.getLoadedLanguages());
    await highlighter.loadLanguage(...languages.filter(lang => !loaded.has(lang)));

    for (const node of nodes) {
      const lang = node.properties["dataInlineLanguage"];
      const source = node.children.map(child => child.value ?? "").join("");
      const highlighted = highlighter.codeToHast(source, { lang, themes, defaultColor: false });
      const pre = highlighted.children.find(child => child.type === "element" && child.tagName === "pre");
      const code = pre.children.find(child => child.type === "element" && child.tagName === "code");
      node.children = code.children.flatMap(line => line.type === "element" ? line.children : [line]);
      node.properties.className = [...(node.properties.className ?? []), "shiki-inline"];
      node.properties["data-language"] = lang;
      delete node.properties["dataInlineLanguage"];
    }
  };
}
