import { createHash } from "node:crypto";
import grammar from "./cachet.tmLanguage.json" with { type: "json" };

// Astro reuses its Shiki instance across dev reloads. Shiki keeps grammars
// by name and scope, so give each revision its own identity and a stable alias.
const revision = createHash("sha256")
  .update(JSON.stringify(grammar))
  .digest("hex")
  .slice(0, 12);

export default {
  ...grammar,
  name: `cachet-${revision}`,
  scopeName: `${grammar.scopeName}.${revision}`,
  aliases: ["cachet"],
};
