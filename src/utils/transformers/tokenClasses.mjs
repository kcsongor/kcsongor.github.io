import { tokenStyles } from "../../syntax/monochrome.ts";

// A style's declarations, keyed by property, with values lower-cased so that
// Shiki's `#0A0A0A` matches the theme's `#0a0a0a`.
const declarations = style =>
  Object.fromEntries(
    style
      .split(";")
      .map(part => part.split(":").map(text => text.trim()))
      .filter(([property, value]) => property && value)
      .map(([property, value]) => [property, value.toLowerCase()])
  );

const keyOf = properties =>
  Object.entries(properties)
    .map(([property, value]) => `${property}:${String(value).toLowerCase()}`)
    .sort()
    .join(";");

const classes = new Map(tokenStyles.map(({ name, properties }) => [keyOf(properties), name]));

/**
 * Gives a token whose inline style is exactly one of the themes' token
 * styles that style's class (`tk-…`, defined by `tokenStylesheet`) in place
 * of the inline style. The custom properties are the same, so every rule that
 * reads `--shiki-light` and `--shiki-dark` works as before; a page of
 * listings just stops repeating them on every token. Any other style is left
 * as it is.
 */
export function transformerTokenClasses() {
  return {
    name: "token-classes",
    span(node) {
      const style = node.properties?.style;
      if (typeof style !== "string") return;
      const name = classes.get(keyOf(declarations(style)));
      if (!name) return;
      delete node.properties.style;
      this.addClassToHast(node, name);
    },
  };
}
