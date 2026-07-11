# README snippet renderer

GitHub does not know the Cavalry language, so fenced code blocks in the
top-level `README.md` get no syntax highlighting. This tool renders each example
program to a coloured *Scalable Vector Graphics* (SVG) image instead, using the
same *TextMate* grammar that drives the VS Code extension. The README embeds
those images, so its snippets look highlighted on GitHub.

## How it works

The generator loads the grammar from
`editors/vscode/syntaxes/cavalry.tmLanguage.json` and the two bundled
*light-plus* / *dark-plus* themes, then merges the per-scope colour
customisations from `.vscode/settings.json` (italic specification keywords, blue
function calls) into both. It tokenises each snippet with *Shiki* and emits one
`<text>` element per line, one coloured `<tspan>` per token, at a fixed
monospace advance. Because the colours come from the grammar and the editor
settings, the images stay in step with what the extension shows — change the
grammar, re-run, and every image updates.

Each snippet has a *slug* (`euclidean-division`) shared by three things: its
source `snippets/<slug>.cav`, its two `docs/snippet-<slug>-{light,dark}.svg`
images, and a marker block in the README that the generator owns:

```html
<!-- snippet: euclidean-division -->
<a href="docs/readme-snippets/snippets/euclidean-division.cav">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/snippet-euclidean-division-dark.svg">
    <img alt="Cavalry code snippet" src="docs/snippet-euclidean-division-light.svg">
  </picture>
</a>
<!-- /snippet -->
```

The generator rewrites only what sits between `<!-- snippet: <slug> -->` and
`<!-- /snippet -->`; the surrounding prose and headings are never touched. The
image links back to its source `.cav`, and the `<picture>` serves the dark image
under a dark colour scheme, the light one otherwise.

## Running

The tool needs *Node.js* 20.11 or newer (for `import.meta.dirname`). From this
directory:

```bash
npm install
npm run build     # render the SVGs and fill in the README blocks
npm run check     # verify everything on disk is up to date (exit 1 if not)
```

`build` walks the README markers, renders each referenced snippet, and writes
its two SVGs. It errors if a marker names a missing `.cav`, or a `.cav` has no
marker — so a half-added snippet is caught. `check` does the same work in memory
and compares against the committed files without writing anything.

## Adding or editing a snippet

Editing is a one-liner: change a `.cav` file and re-run `npm run build`. To add a
snippet, do both halves, then rebuild:

1. Create `snippets/<slug>.cav`.
2. Add a marker block for `<slug>` to the README (copy the shape above).

## Staying in sync automatically

Two mechanisms run `npm run check` so a stale image can never be committed:

- **CI** — the `snippets` job in `.github/workflows/ci.yml` installs the
  dependencies and runs the check on every push and pull request.
- **Pre-commit hook** — `.githooks/pre-commit` runs the check when a staged
  change touches the grammar, the editor settings, a snippet, or the README.
  Enable it once per clone:

  ```bash
  git config core.hooksPath .githooks
  ```

## Caveats

An SVG embedded through `<img>` renders as an opaque image, so its text is not
selectable and external fonts do not load; the images fall back to whatever
monospace font the viewer has, which the `0.6`-em advance assumes. GitHub's
Markdown sanitiser strips inline `<svg>`, so `<img>` is the only embedding path,
and the snippet text no longer lives in the README source — it lives here, in
`snippets/`. That split is the price of highlighting a language GitHub has never
heard of.
