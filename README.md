# Csongor's blog

A static Astro blog based on [AstroPaper](https://github.com/satnaing/astro-paper),
with Markdown/MDX posts, custom Cachet highlighting, and optional interactive
figures. Published at <https://blog.csongor.co.uk> on GitHub Pages.

## Run locally

Use Node.js 24 (`.nvmrc`) and the pinned pnpm version:

```sh
npx --yes pnpm@11.3.0 install --frozen-lockfile
npx pnpm@11.3.0 dev
```

Open <http://localhost:4321>. Changes reload automatically. Draft posts are
visible in development, including `/interactive-example/`.

```sh
npx pnpm@11.3.0 test     # Migration and figure-model checks
npx pnpm@11.3.0 build   # Typecheck, build, search index, and output verification
npx pnpm@11.3.0 preview # Serve the production output
```

Search uses a static Pagefind index. Run a build before testing search locally;
rebuild after changing content to refresh its index.

## Docker

No local Node or Ruby installation is required:

```sh
docker compose up --build
```

Open <http://localhost:4321>. Source files are mounted for live updates. Rebuild
the image after changing dependencies. Stop it with Ctrl-C, then optionally
`docker compose down`.

To build and verify the deployable static files using the same Dockerfile as CI:

```sh
docker buildx build --target export --output type=local,dest=dist .
```

The final `export` stage contains only the static site. GitHub Pages serves those
files; there is no container running in production.

## GitHub Pages

`.github/workflows/pages.yml` builds in Docker on pull requests and pushes to
`master`. Only pushes to `master` (or manual runs on that branch) deploy.

In the repository's **Settings → Pages**, set **Source → GitHub Actions** and
retain the custom domain `blog.csongor.co.uk`. `public/CNAME` is also copied into
the output. The workflow uploads `dist/` with `actions/upload-pages-artifact`
and publishes it with `actions/deploy-pages`.

The build uses the committed `pnpm-lock.yaml` with `--frozen-lockfile`. No
publishing credentials or personal access tokens are needed beyond the workflow's
scoped `GITHUB_TOKEN` permissions.

## Write a post

Add `src/content/posts/my-post.md`:

```yaml
---
title: "My post"
description: "A short description for the index and feeds."
pubDatetime: 2026-10-24T00:00:00Z
tags: [programming-languages]
draft: true
---
```

The filename determines the URL: `/my-post/`. Changing the title does not change
the URL. Remove `draft: true` to publish. Future-dated posts are excluded from
production until their publication date; another build is needed to publish them.
Use `modDatetime` to record later edits. Posts remain ordered by publication date.

The article layout generates a sticky contents sidebar on the left from level 2
and 3 headings, highlighting the section currently being read. Below 1280px it
becomes a collapsible contents list above the article. Notes have a small label
and a fine rule; at 1440px and above, top-level notes sit in the right margin.
Otherwise they remain inline. Notes keep their document order and stack without
overlapping, with no JavaScript required. A thin bar tracks reading progress.
Markdown supports footnotes and callouts:

```markdown
> [!NOTE]
> A useful aside.

A claim with a footnote.[^detail]

[^detail]: The extra explanation.
```

## Typography

Typography uses IBM Plex Sans for prose and headings, and IBM Plex Mono for code,
navigation, and dates. Variable WOFF2 fonts are self-hosted in `public/fonts`,
with `font-display: swap` and system fallbacks. Their SIL Open Font License is
included alongside them. Font families are configured in `src/styles/theme.css`;
sizes and treatments are in `src/styles/custom.css`.

## Syntax highlighting

Use ordinary fenced blocks with `haskell`, `rust`, `purescript`, or `cachet` as
the language. `src/syntax/cachet.tmLanguage.json` is the custom TextMate grammar,
registered in `astro.config.ts` under `markdown.shikiConfig.langs`.

The initial Cachet keyword list follows the local compiler lexer's reserved words
at the time of migration. Strings, escapes, interpolation, numbers, comments,
operators and declaration names have rules too. Uppercase-name colouring is a
syntactic convention, not name resolution. Update the grammar as the language
evolves; it has no runtime dependency on the compiler checkout.

To add another language, import its TextMate grammar and add it to `langs`.
Standard TextMate scopes pick up the configured light and dark themes.
The site palette and Shiki themes in `src/syntax/monochrome.ts` are adapted from
[vim-monochrome](https://github.com/kcsongor/vim-monochrome): neutral text, bold
keywords, italic comments, and red (light) or teal (dark) literals.
The interface stays monochrome; figures use the separate `--figure-accent`
token. Active contents entries are indicated by bold text only.

Shiki also supports `// [!code highlight]`, `// [!code ++]` and `// [!code --]`
annotations for highlighted lines and diffs. Code-fence metadata such as
`file="example.cachet"` adds a filename label. Highlighting runs at build time.

## Interactive figures

Use `.mdx` when a post needs a component:

```mdx
import CacheLineChart from '../../components/figures/CacheLineChart.astro';

Here is the comparison:

<CacheLineChart />
```

`src/content/posts/interactive-example.mdx` is a complete draft example.
The chart renders an initial SVG at build time, then a small browser script
updates it. The slider models distinct cache lines touched, not execution time.
No client-side framework or server is required. Framework components can be
added later through Astro integrations if needed.

## Migration notes

- All 12 original posts keep their root-level URLs, dates and code samples.
- `/feed.xml` remains an Atom feed and retains historical entry IDs;
  `/rss.xml` is also available.
- `src/syntax/legacy-headings.json` preserves old Kramdown section anchors and
  uses them in the generated table of contents.
- `tests/legacy-posts.json` records original code hashes and section IDs.
  `scripts/verify-build.mjs` checks generated pages, historical anchors, local
  links, feed entries, search output and draft exclusion. Intentional edits to
  historical code samples require updating their recorded hashes.
- The previous theme's sample setup page and Ruby/Grunt toolchain are removed.
- The unrelated `gadts/` working directory is not part of the site or Docker build.

AstroPaper source snapshot: `35cfa7fbe0b897306d27670d3819e55d5205f3dd`.
Its MIT licence is retained in `licenses/AstroPaper.txt`; the old theme's licence
is retained in `LICENSE`.
