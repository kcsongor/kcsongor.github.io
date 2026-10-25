const article = document.querySelector<HTMLElement>("#article");
const wide = window.matchMedia("(min-width: 1440px)");

// Markdown NOTEs follow the block they explain. A float must precede that
// block to sit beside it; retain the original position for inline reading.
const inlineNotes = Array.from(article?.querySelectorAll<HTMLElement>(
  ':scope > .callout[data-callout="note"]:not(.footnote-sidenote)',
) ?? []).map(note => {
  let anchor = note.previousElementSibling;
  while (anchor?.matches('.callout[data-callout="note"]')) {
    anchor = anchor.previousElementSibling;
  }
  if (!anchor) return null;
  const position = document.createComment("Inline note position");
  note.before(position);
  return { note, anchor, position };
}).filter(item => item !== null);

const positionInlineNotes = () => {
  for (const { note, anchor, position } of inlineNotes) {
    if (wide.matches) anchor.before(note);
    else position.after(note);
    note.toggleAttribute("data-margin-note", wide.matches);
  }
};
positionInlineNotes();
wide.addEventListener("change", positionInlineNotes);

const notes = new Map<string, HTMLElement>();
article?.querySelectorAll<HTMLElement>(".footnote-sidenote[data-footnote-id]").forEach(note => {
  notes.set(note.dataset.footnoteId!, note);
});
const references = Array.from(article?.querySelectorAll<HTMLAnchorElement>("a[data-footnote-ref]") ?? [])
  .map(link => {
    let id;
    try { id = decodeURIComponent(link.hash.slice(1)); } catch { return null; }
    const margin = notes.get(id);
    const footer = document.getElementById(id);
    return margin && footer ? { link, margin, footer, description: link.getAttribute("aria-describedby") } : null;
  })
  .filter(ref => ref !== null);

if (article && references.length) {
  const highlight = () => {
    for (const ref of references) {
      const active = references.some(other => other.margin === ref.margin
        && (other.link.matches(":hover") || other.link === document.activeElement));
      ref.margin.classList.toggle("footnote-active", active);
      ref.footer.classList.toggle("footnote-active", active);
    }
  };
  for (const { link } of references) {
    for (const event of ["mouseenter", "mouseleave", "focus", "blur"]) {
      link.addEventListener(event, highlight);
    }
  }
  const syncLinks = () => {
    for (const { link, margin, footer, description } of references) {
      link.setAttribute("href", `#${wide.matches ? margin.id : footer.id}`);
      if (wide.matches) link.setAttribute("aria-describedby", margin.id);
      else if (description) link.setAttribute("aria-describedby", description);
      else link.removeAttribute("aria-describedby");
    }
    // Existing bookmarks and viewport changes must also land on the visible note.
    let id;
    try { id = decodeURIComponent(location.hash.slice(1)); } catch { return; }
    const ref = references.find(ref => ref.margin.id === id || ref.footer.id === id);
    if (ref) {
      const target = wide.matches ? ref.margin : ref.footer;
      if (id !== target.id) {
        history.replaceState(history.state, "", `#${target.id}`);
        target.scrollIntoView({ behavior: "instant", block: "nearest" });
      }
    }
  };
  article.dataset.responsiveFootnotes = "";
  syncLinks();
  wide.addEventListener("change", syncLinks);
  window.addEventListener("hashchange", syncLinks);
}
