/**
 * Adapted from https://github.com/kcsongor/vim-monochrome
 * Neutral identifiers, bold statements/types, italic comments, accented literals.
 */
type Role = "foreground" | "strong" | "subtle" | "accent" | "invalid";
type FontStyle = "" | "italic" | "bold" | "underline";
type Palette = Record<Role, string> & { background: string };

// Each rule names its colour by role, so the light and dark themes stay
// alike rule for rule, and a token's styles in both can be named once
// (`tokenStyles`).
const rules: { scope?: string | string[]; color: Role; fontStyle?: FontStyle }[] = [
  { scope: ["comment", "punctuation.definition.comment"], color: "subtle", fontStyle: "italic" },
  { scope: ["keyword", "storage"], color: "strong", fontStyle: "bold" },
  {
    scope: ["entity.name.type", "entity.name.class", "entity.name.namespace", "support.type", "support.class"],
    color: "strong",
    fontStyle: "bold",
  },
  { scope: ["entity.name.function", "support.function", "variable"], color: "foreground", fontStyle: "" },
  { scope: ["constant", "string"], color: "accent", fontStyle: "" },
  // Move's grammar labels the module declaration name as a constant.
  { scope: "meta.module.move constant.other.move", color: "foreground", fontStyle: "" },
  { scope: "meta.module_scope.move constant.other.move", color: "accent", fontStyle: "" },
  {
    scope: ["constant.other.haskell", "constant.other.purescript", "constant.other.constructor"],
    color: "strong",
    fontStyle: "bold",
  },
  { scope: ["keyword.operator", "punctuation", "constant.character.escape"], color: "subtle", fontStyle: "" },
  { scope: ["punctuation.definition.string"], color: "accent", fontStyle: "" },
  { scope: ["invalid"], color: "invalid", fontStyle: "underline" },
];

function monochrome(type: "light" | "dark", palette: Palette) {
  return {
    name: `csongor-monochrome-${type}`,
    type,
    colors: {
      "editor.background": palette.background,
      "editor.foreground": palette.foreground,
    },
    tokenColors: [
      { settings: { foreground: palette.foreground, background: palette.background } },
      ...rules.map(({ scope, color, fontStyle }) => ({
        scope,
        settings: { foreground: palette[color], fontStyle },
      })),
    ],
  };
}

const light: Palette = {
  background: "#ffffff",
  foreground: "#0a0a0a",
  strong: "#0a0a0a",
  subtle: "#767676",
  accent: "#b22222",
  invalid: "#b22222",
};
const dark: Palette = {
  background: "#0a0a0a",
  foreground: "#eeeeee",
  strong: "#ffffff",
  subtle: "#808080",
  accent: "#00af87",
  invalid: "#df5f5f",
};

export const monochromeLight = monochrome("light", light);
export const monochromeDark = monochrome("dark", dark);

// The CSS custom properties Shiki writes for a token in one theme, as
// `defaultColor: false` spells them.
const properties = (theme: "light" | "dark", color: string, fontStyle: FontStyle) => ({
  [`--shiki-${theme}`]: color,
  ...(fontStyle === "italic" && { [`--shiki-${theme}-font-style`]: "italic" }),
  ...(fontStyle === "bold" && { [`--shiki-${theme}-font-weight`]: "bold" }),
  ...(fontStyle === "underline" && { [`--shiki-${theme}-text-decoration`]: "underline" }),
});

/**
 * Every token style the two themes give, by name: a role and its font style.
 * A highlighted token whose inline style is exactly one of these is given
 * the class instead (see `transformerTokenClasses`), which keeps a page of
 * listings from carrying the same few declarations on every token.
 */
export const tokenStyles: { name: string; properties: Record<string, string> }[] = Object.values(
  Object.fromEntries(
    [{ color: "foreground" as Role, fontStyle: "" as FontStyle }, ...rules].map(({ color, fontStyle = "" }) => {
      const name = `tk-${color}${fontStyle ? `-${fontStyle}` : ""}`;
      return [
        name,
        {
          name,
          properties: {
            ...properties("light", light[color], fontStyle),
            ...properties("dark", dark[color], fontStyle),
          },
        },
      ];
    })
  )
);

/** The stylesheet that gives each token class its properties. */
export const tokenStylesheet = tokenStyles
  .map(
    ({ name, properties }) =>
      `.${name}{${Object.entries(properties)
        .map(([property, value]) => `${property}:${value}`)
        .join(";")}}`
  )
  .join("");
