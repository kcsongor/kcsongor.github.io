import { defineAstroPaperConfig } from "./src/types/config";

export default defineAstroPaperConfig({
  site: {
    url: "https://blog.csongor.co.uk",
    title: "Csongor Kiss",
    description: "Notes on programming languages, types, and systems.",
    author: "Csongor Kiss",
    profile: "https://github.com/kcsongor",
    ogImage: "social-card.svg",
    lang: "en",
    timezone: "UTC",
  },
  posts: { perPage: 12, perIndex: 12, scheduledPostMargin: 0 },
  features: {
    lightAndDarkMode: true,
    dynamicOgImage: false,
    showArchives: false,
    showBackButton: false,
    editPost: { enabled: false },
    search: "pagefind",
  },
  socials: [
    { name: "github", url: "https://github.com/kcsongor" },
  ],
  shareLinks: [],
});
