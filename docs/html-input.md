# HTML input contract for qnda

qnda converts project-specific HTML into EPUB XHTML. This document describes elements and label conventions the **general** EPUB pipeline understands. Project overlays may localize caption text, reference wording, or heading demotion.

## Custom elements

### Inline math: `<eq>`

Body is LaTeX math without delimiters. qnda wraps it as `$…$`, renders a PNG (or SVG), and replaces the element with an inline image.

### Display math: `<equation>` or `<math>`

Body is display LaTeX. If the body is not already in a numbered environment, qnda wraps it in `align*`. Optional attribute:

- `id="eq:…"` or `id="eqn:…"` — anchor for cross-references and dense numbering (see below).

The rendered image sits in a `<span class="displaymath">`. When `id` is present, it is copied to that wrapper so in-book links resolve.

### Cross-references: `<ref label="…">`

Resolved through a label map built from headings, figures, tables, equations, and optional `book.aux`. Unknown prefixes still show the numeric counter when one exists, instead of a generic placeholder.

### Internal links

Use `id` on headings, captions, and figures as before. `<pageref label="…">` links to the file that contains the label.

## Label prefixes

| Prefix | Role |
|--------|------|
| `eq:` / `eqn:` | Equations (required on `\label{…}` inside display math for dense numbering) |
| `fig:`, `tbl:`, chapter/section ids | As in existing qnda HTML (see README) |

Equation references are shown as `(N)` in the default English pipeline, where `N` is the dense equation number or counter from aux.

## Optional `book.aux`

If `book.aux` exists in the working directory, qnda reads `\newlabel{label}{{counter}{page}}` entries and follows `\input{…}` / `\@input{…}` recursively. Missing files yield no entries; cycles are ignored.

Aux labels supply text for `\ref` inside isolated TeX jobs (theorems, examples, etc.). Page numbers from aux are not copied into EPUB ref text—only the counter string is used.

## Math images

- Path pattern: `images/png/eq<hash>.png` (content hash of the TeX body)
- Render density: **200 dpi** for PNG
- Duplicate bodies share one file; concurrent builds deduplicate render jobs by path

Use `--math png` (default) or `--math svg`. `--math mathml` skips image generation.

## Bibliography

If the book file contains a `<bibliography/>` section, qnda builds `bibliography.xhtml` and rewrites citations. Otherwise bibliography processing is skipped.

## ISBN

`<isbn>` inside `<bookinfo>` is optional. Without it, metadata and colophon still generate; identifier falls back to the book title.
