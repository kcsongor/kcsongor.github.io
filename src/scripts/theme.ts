const preference = window.matchMedia("(prefers-color-scheme: dark)");
function storedTheme() {
  try { return localStorage.getItem("theme"); } catch { return null; }
}
function applyTheme(theme: string) {
  document.documentElement.dataset.theme = theme;
  document.documentElement.classList.toggle("dark", theme === "dark");
  document.querySelector("#theme-btn")?.setAttribute("aria-label", `Switch to ${theme === "dark" ? "light" : "dark"} theme`);
  document.querySelector('meta[name="theme-color"]')?.setAttribute("content", getComputedStyle(document.body).backgroundColor);
}
applyTheme(document.documentElement.dataset.theme ?? (preference.matches ? "dark" : "light"));
document.querySelector("#theme-btn")?.addEventListener("click", () => {
  const theme = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
  try { localStorage.setItem("theme", theme); } catch { /* Theme still works without storage. */ }
  applyTheme(theme);
});
preference.addEventListener("change", ({ matches }) => {
  if (!storedTheme()) applyTheme(matches ? "dark" : "light");
});
