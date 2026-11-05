/** Display arrows without changing the source text used for selection and copying. */
export const transformerCachetTypography = () => ({
  name: "cachet-typography",
  span(node) {
    if (this.options.lang !== "cachet") return;
    node.children = node.children.flatMap(child => {
      if (child.type !== "text" || !child.value.includes("->")) return [child];
      return child.value.split("->").flatMap((part, index) => [
        ...(index
          ? [
              {
                type: "element",
                tagName: "span",
                properties: { className: ["cachet-arrow"] },
                children: [
                  {
                    type: "element",
                    tagName: "span",
                    properties: { className: ["cachet-arrow-source"] },
                    children: [{ type: "text", value: "->" }],
                  },
                ],
              },
            ]
          : []),
        { type: "text", value: part },
      ]);
    });
  },
});
