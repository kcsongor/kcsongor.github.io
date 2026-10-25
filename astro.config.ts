import {
  defineConfig,
  envField,
  svgoOptimizer,
} from "astro/config";
import tailwindcss from "@tailwindcss/vite";
import mdx from "@astrojs/mdx";
import sitemap from "@astrojs/sitemap";
import { unified } from "@astrojs/markdown-remark";
import rehypeCallouts from "rehype-callouts";
import {
  transformerNotationDiff,
  transformerNotationHighlight,
  transformerNotationWordHighlight,
} from "@shikijs/transformers";
import { transformerFileName } from "./src/utils/transformers/fileName";
import { transformerLanguageLabel } from "./src/utils/transformers/languageLabel.mjs";
import config from "./astro-paper.config";
import cachet from "./src/syntax/cachet.tmLanguage.json";
import loom from "./src/syntax/loom.tmLanguage.json";
import { monochromeLight, monochromeDark } from "./src/syntax/monochrome";
import { legacyHeadingAliases } from "./src/syntax/legacy-headings.mjs";
import { footnoteSidenotes } from "./src/syntax/footnote-sidenotes.mjs";
import { remarkInlineCode, rehypeInlineCode } from "./src/syntax/inline-code.mjs";

const syntaxThemes = { light: monochromeLight, dark: monochromeDark };

export default defineConfig({
  site: config.site.url,
  output: "static",
  devToolbar: { enabled: false },
  trailingSlash: "always",
  integrations: [
    mdx(),
    sitemap({
      filter: page =>
        config.features?.showArchives !== false || !page.endsWith("/archives/"),
    }),
  ],
  i18n: {
    locales: ["en"],
    defaultLocale: "en",
    routing: {
      prefixDefaultLocale: false,
    },
  },
  markdown: {
    processor: unified({
      remarkPlugins: [remarkInlineCode],
      rehypePlugins: [
        rehypeCallouts,
        [rehypeInlineCode, { langs: [cachet, loom], themes: syntaxThemes }],
        legacyHeadingAliases,
        // Pair numbered margin notes with their original anchors for responsive navigation.
        footnoteSidenotes,
      ],
    }),
    shikiConfig: {
      langs: [cachet, loom],
      themes: syntaxThemes,
      defaultColor: false,
      wrap: false,
      transformers: [
        transformerFileName({ style: "v2", hideDot: false }),
        transformerLanguageLabel(),
        transformerNotationHighlight(),
        transformerNotationWordHighlight(),
        transformerNotationDiff({ matchAlgorithm: "v3" }),
      ],
    },
  },
  vite: {
    plugins: [tailwindcss()],
  },
  env: {
    schema: {
      PUBLIC_GOOGLE_SITE_VERIFICATION: envField.string({
        access: "public",
        context: "client",
        optional: true,
      }),
    },
  },
  experimental: {
    svgOptimizer: svgoOptimizer(),
  },
});

