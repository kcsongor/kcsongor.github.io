import costs from "../../data/loom/api-costs.json" with { type: "json" };

const byId = new Map(costs.rows.map(row => [row.run_id, row]));
export function apiCost(row) {
  const cost = byId.get(row.run_id);
  if (!cost || cost.model !== row.model || cost.usd === null) return null;
  return {
    usd: cost.usd,
    label: `~$${cost.usd.toFixed(2)}`,
    description: cost.basis === "cli_list_price_estimate"
      ? "Estimated API cost (USD): Claude Code’s recorded list-price estimate for the run; not subscription spend."
      : "Estimated API cost (USD): standard short-context rates checked 25 October 2026, including cached input and reasoning output. Request sizes were not retained; premiums are not included.",
  };
}
