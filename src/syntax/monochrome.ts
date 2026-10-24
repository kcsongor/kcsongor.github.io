/**
 * Adapted from https://github.com/kcsongor/vim-monochrome
 * Neutral identifiers, bold statements/types, italic comments, accented literals.
 */
function monochrome(
  type: "light" | "dark",
  background: string,
  foreground: string,
  strong: string,
  subtle: string,
  accent: string
) {
  return {
    name: `csongor-monochrome-${type}`,
    type,
    colors: {
      "editor.background": background,
      "editor.foreground": foreground,
    },
    tokenColors: [
      { settings: { foreground, background } },
      {
        scope: ["comment", "punctuation.definition.comment"],
        settings: { foreground: subtle, fontStyle: "italic" },
      },
      {
        scope: ["keyword", "storage"],
        settings: { foreground: strong, fontStyle: "bold" },
      },
      {
        scope: ["entity.name.type", "entity.name.class", "entity.name.namespace", "support.type", "support.class"],
        settings: { foreground: strong, fontStyle: "bold" },
      },
      {
        scope: ["entity.name.function", "support.function", "variable"],
        settings: { foreground, fontStyle: "" },
      },
      {
        scope: ["constant", "string"],
        settings: { foreground: accent, fontStyle: "" },
      },
      {
        scope: ["constant.other.haskell", "constant.other.purescript", "constant.other.constructor"],
        settings: { foreground: strong, fontStyle: "bold" },
      },
      {
        scope: ["keyword.operator", "punctuation", "constant.character.escape"],
        settings: { foreground: subtle, fontStyle: "" },
      },
      {
        scope: ["punctuation.definition.string"],
        settings: { foreground: accent, fontStyle: "" },
      },
      {
        scope: ["invalid"],
        settings: { foreground: type === "dark" ? "#df5f5f" : "#b22222", fontStyle: "underline" },
      },
    ],
  };
}

export const monochromeLight = monochrome("light", "#ffffff", "#0a0a0a", "#0a0a0a", "#767676", "#b22222");
export const monochromeDark = monochrome("dark", "#0a0a0a", "#eeeeee", "#ffffff", "#808080", "#00af87");
