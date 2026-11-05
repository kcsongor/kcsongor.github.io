/** Colour variables by how often they change: `heat="i j k"`, coldest first. */
export const transformerHeat = () => ({
  name: "heat",
  preprocess(code, options) {
    const names = options.meta?.__raw?.match(/(?:^|\s)heat="([^"]*)"/)?.[1].split(" ");
    if (!names) return;
    const level = name => ["cold", "warm", "hot"][names.indexOf(name)];
    options.decorations = [...(options.decorations ?? [])];
    for (const match of code.matchAll(/\b\w+\b/g)) {
      const heat = names.includes(match[0]) && `heat-${level(match[0])}`;
      // A literal stays plain, so that the only colours are the heat's.
      const name = heat || (/^\d+$/.test(match[0]) && "heat-none");
      if (name)
        options.decorations.push({
          start: match.index,
          end: match.index + match[0].length,
          properties: { class: name },
        });
    }
  },
});
