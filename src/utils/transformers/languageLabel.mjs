/** Label fenced code at build time; keep the label outside the copied code. */
export const transformerLanguageLabel = () => ({
  name: "language-label",
  pre(node) {
    const language = this.options.lang;
    const override = this.options.meta?.__raw?.match(/(?:^|\s)label="([^"]*)"/)?.[1];
    const label = override ?? (/^(text|txt|plain|plaintext)$/.test(language) ? "" : language);
    if (!label) return;
    this.addClassToHast(node, "has-language-label");
    node.children.unshift({
      type: "element",
      tagName: "span",
      properties: { className: ["code-language-label"] },
      children: [{ type: "text", value: label }],
    });
  },
});
