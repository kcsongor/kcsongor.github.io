import "./footnotes";

for (const heading of document.querySelectorAll<HTMLElement>("#article :is(h2,h3,h4,h5,h6)[id]")) {
  const link = document.createElement("a");
  link.className = "heading-link";
  link.href = `#${heading.id}`;
  link.textContent = "#";
  link.setAttribute("aria-label", `Link to ${heading.textContent}`);
  heading.append(link);
}
for (const block of document.querySelectorAll<HTMLPreElement>("#article pre")) {
  block.tabIndex = 0;
  if (!block.querySelector("code")) continue;
  if (!navigator.clipboard) continue;
  const wrapper = document.createElement("div");
  wrapper.className = "code-container";
  block.before(wrapper);
  wrapper.append(block);
  const button = document.createElement("button");
  button.className = "copy-code";
  button.type = "button";
  button.textContent = "Copy";
  button.setAttribute("aria-label", "Copy code");
  button.setAttribute("aria-live", "polite");
  wrapper.append(button);
  button.addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(block.querySelector("code")?.textContent ?? "");
      button.textContent = "Copied";
    } catch { button.textContent = "Couldn't copy"; }
    setTimeout(() => { button.textContent = "Copy"; }, 1500);
  });
}

const article = document.querySelector<HTMLElement>("#article");
const post = document.querySelector<HTMLElement>("#main-content");
const progress = document.querySelector<HTMLElement>(".reading-progress");
const progressFill = document.querySelector<HTMLElement>("[data-reading-progress-fill]");
const contentsLinks = Array.from(document.querySelectorAll<HTMLAnchorElement>("a[data-section]"));
const sections = Array.from(new Set(contentsLinks.map(link => link.dataset.section)))
  .map(id => id ? document.getElementById(id) : null)
  .filter((heading): heading is HTMLElement => heading !== null);
const sidebar = document.querySelector<HTMLElement>("[data-contents-sidebar]");

if (article && post && progress && progressFill) {
  progress.hidden = false;
  let pendingFrame = false;
  let activeSection = "";
  let lastPercent = -1;

  function updateReadingPosition() {
    pendingFrame = false;
    if (!article || !post || !progress || !progressFill) return;
    const articleBottom = article.getBoundingClientRect().bottom;
    const postTop = post.getBoundingClientRect().top;
    const distance = articleBottom - postTop - window.innerHeight;
    const fraction = distance > 0
      ? Math.min(1, Math.max(0, -postTop / distance))
      : articleBottom <= window.innerHeight ? 1 : 0;
    const readingLine = Math.min(120, window.innerHeight * 0.2);
    let current = "";
    for (const section of sections) {
      if (section.getBoundingClientRect().top > readingLine) break;
      current = section.id;
    }
    // The final section may never reach the reading line on a short page.
    if (articleBottom <= window.innerHeight + 1 && sections.length) {
      current = sections[sections.length - 1].id;
    }

    progressFill.style.transform = `scaleX(${fraction})`;
    const percent = Math.round(fraction * 100);
    if (percent !== lastPercent) {
      progress.setAttribute("aria-valuenow", String(percent));
      lastPercent = percent;
    }
    if (current === activeSection) return;
    activeSection = current;
    for (const link of contentsLinks) {
      if (link.dataset.section === current) link.setAttribute("aria-current", "location");
      else link.removeAttribute("aria-current");
    }

    // Scroll only the sidebar's own overflow, never the article or the window.
    if (sidebar && sidebar.offsetWidth > 0) {
      const activeLink = sidebar.querySelector<HTMLElement>('[aria-current="location"]');
      if (activeLink) {
        const container = sidebar.getBoundingClientRect();
        const link = activeLink.getBoundingClientRect();
        if (link.top < container.top) sidebar.scrollTop += link.top - container.top - 8;
        else if (link.bottom > container.bottom) sidebar.scrollTop += link.bottom - container.bottom + 8;
      }
    }
  }

  function scheduleUpdate() {
    if (pendingFrame) return;
    pendingFrame = true;
    requestAnimationFrame(updateReadingPosition);
  }
  window.addEventListener("scroll", scheduleUpdate, { passive: true });
  window.addEventListener("resize", scheduleUpdate);
  window.addEventListener("hashchange", scheduleUpdate);
  window.addEventListener("pageshow", scheduleUpdate);
  new ResizeObserver(scheduleUpdate).observe(article);
  document.fonts.ready.then(scheduleUpdate);
  for (const link of contentsLinks) {
    link.addEventListener("click", () => {
      const details = link.closest<HTMLDetailsElement>("[data-mobile-contents]");
      if (details) details.open = false;
    });
  }
  scheduleUpdate();
}
