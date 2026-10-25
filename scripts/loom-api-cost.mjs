// Prices verified 25 October 2026; USD per million tokens.
export const openAIRates = {
  "gpt-6.1-sol": { input: 2, cached: 0.1, output: 10 },
  "gpt-6-astra": { input: 10, cached: 1, output: 50 },
};

export function estimateApiCost(model, report) {
  // Gemini's raw input/cache and output/thinking overlap is unresolved.
  if (model.startsWith("gemini")) return { usd: null, basis: "unresolved_token_accounting" };
  if (["claude-opus-5-5", "claude-sonnet-5-5"].includes(model)) {
    const usd = report?.estimated_cost_usd;
    return Number.isFinite(usd) && usd >= 0
      ? { usd, basis: "cli_list_price_estimate" }
      : { usd: null, basis: "missing_usage" };
  }
  const rates = openAIRates[model];
  const usage = report?.usage;
  if (!rates || !usage) return { usd: null, basis: "missing_usage" };
  const { input_tokens: input, cached_input_tokens: cached, output_tokens: output, cache_write_input_tokens: writes } = usage;
  if (![input, cached, output].every(n => Number.isFinite(n) && n >= 0) || cached > input || writes !== 0) {
    return { usd: null, basis: "incomplete_token_accounting" };
  }
  // Codex input includes cache hits; output already includes reasoning.
  // Request sizes are not retained, so no long-context or service-tier premium is inferred.
  return {
    usd: ((input - cached) * rates.input + cached * rates.cached + output * rates.output) / 1e6,
    basis: "standard_short_context_estimate",
  };
}
