// Render the Cavalry snippets referenced by the top-level README to themed
// SVGs, and fill in the README's <picture> blocks. The grammar and editor
// colour customizations that drive the VS Code extension are the source of the
// colours, so the images stay in step with the extension. See README.md.
//
//   node gen.mjs           write the SVGs and rewrite the README blocks
//   node gen.mjs --check   fail (exit 1) if anything on disk is out of date
//
// Each snippet is a <slug>.cav file in ./snippets/ paired with a marker block
// in the README:
//
//   <!-- snippet: euclidean-division -->
//   ...generated <a>/<picture>...
//   <!-- /snippet -->
//
// The generator owns everything between those markers; the surrounding prose is
// never touched.
import { createHighlighter } from 'shiki'
import lightPlus from 'shiki/themes/light-plus.mjs'
import darkPlus from 'shiki/themes/dark-plus.mjs'
import { readFileSync, writeFileSync, readdirSync, existsSync } from 'node:fs'
import { join, resolve } from 'node:path'

const CHECK = process.argv.includes('--check')

const HERE = import.meta.dirname
const ROOT = resolve(HERE, '../..')
const DOCS = resolve(HERE, '..')
const SNIPPETS = join(HERE, 'snippets')

// Files whose <!-- snippet: … --> markers the generator owns. Each carries the
// relative prefixes its links need, because the two files sit at different
// depths: the top-level README is at the repo root, the examples gallery lives
// here in assets/readme-snippets/. A snippet may appear in both.
const TARGETS = [
  {
    path: join(ROOT, 'README.md'),
    cav: slug => `assets/readme-snippets/snippets/${slug}.cav`,
    svg: (slug, v) => `assets/snippet-${slug}-${v}.svg`,
  },
  {
    path: join(HERE, 'EXAMPLES.md'),
    cav: slug => `snippets/${slug}.cav`,
    svg: (slug, v) => `../snippet-${slug}-${v}.svg`,
  },
]

const fail = msg => { console.error(`gen.mjs: ${msg}`); process.exit(1) }

const GRAMMAR = JSON.parse(
  readFileSync(join(ROOT, 'editors/vscode/syntaxes/cavalry.tmLanguage.json'), 'utf8'))

// Mirror the editor's per-scope customizations (italic specification keywords,
// blue function calls, ...) from .vscode/settings.json into both themes, so the
// SVGs match what the VS Code extension renders. Tolerate JSONC comments.
const settings = JSON.parse(
  readFileSync(join(ROOT, '.vscode/settings.json'), 'utf8')
    .replace(/\/\*[\s\S]*?\*\//g, '').replace(/(^|[^:])\/\/.*$/gm, '$1'))
const rules = settings['editor.tokenColorCustomizations']?.textMateRules ?? []
const withRules = t => ({ ...t, name: `cav-${t.name}`, tokenColors: [...(t.tokenColors ?? []), ...rules] })
const LIGHT = withRules(lightPlus), DARK = withRules(darkPlus)

const FONT = 'ui-monospace, "SF Mono", Menlo, Consolas, monospace'
// ADV is the per-character advance for width/layout. 0.62em leaves a small
// margin over the ~0.6em of typical monospace fonts so the widest line never
// clips the right edge in fonts that run slightly wider (e.g. macOS preview).
const SIZE = 14, LINE = 21, ADV = SIZE * 0.62, PADX = 18, PADY = 16
const esc = s => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')

const hl = await createHighlighter({
  themes: [LIGHT, DARK],
  langs: [{ ...GRAMMAR, name: 'cavalry' }],
})

function render(code, themeName) {
  const { tokens, bg } = hl.codeToTokens(code, { lang: 'cavalry', theme: themeName })
  const cols = Math.max(...tokens.map(l => l.reduce((n, t) => n + t.content.length, 0)), 1)
  const w = Math.ceil(PADX * 2 + cols * ADV)
  const h = PADY * 2 + tokens.length * LINE
  const lines = tokens.map((line, i) => {
    const y = PADY + i * LINE + SIZE
    const spans = line.map(t => {
      const fs = t.fontStyle || 0 // Shiki bitmask: 1 italic, 2 bold, 4 underline
      let attr = `fill="${t.color}"`
      if (fs & 1) attr += ' font-style="italic"'
      if (fs & 2) attr += ' font-weight="bold"'
      if (fs & 4) attr += ' text-decoration="underline"'
      return `<tspan ${attr}>${esc(t.content)}</tspan>`
    }).join('')
    return `  <text x="${PADX}" y="${y}" xml:space="preserve">${spans}</text>`
  }).join('\n')
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}" font-family='${FONT}' font-size="${SIZE}">
  <rect width="100%" height="100%" fill="${bg}" rx="10"/>
${lines}
</svg>
`
}

// The marker block for a snippet: a link to the source .cav wrapping a
// theme-aware <picture>. The link prefixes come from the target file so each
// resolves relative to its own location. Emitted verbatim so re-running is a
// no-op.
const block = (slug, t) => `<!-- snippet: ${slug} -->
<a href="${t.cav(slug)}">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="${t.svg(slug, 'dark')}">
    <img alt="Cavalry code snippet" src="${t.svg(slug, 'light')}">
  </picture>
</a>
<!-- /snippet -->`

const MARKER = /<!-- snippet: (\S+) -->[\s\S]*?<!-- \/snippet -->/g
const rel = p => p.replace(`${ROOT}/`, '')

// Read every target file and collect the slugs it references.
const files = TARGETS.map(t => {
  const src = readFileSync(t.path, 'utf8')
  return { t, src, slugs: [...src.matchAll(MARKER)].map(m => m[1]) }
})
const allSlugs = [...new Set(files.flatMap(f => f.slugs))]
const fileSlugs = readdirSync(SNIPPETS)
  .filter(f => f.endsWith('.cav')).map(f => f.replace(/\.cav$/, '')).sort()

// Guard against the ways the mapping can drift: a slug listed twice in one file,
// a marker naming a snippet that doesn't exist, and a snippet source shown by no
// marker at all. A slug appearing in more than one file is fine (a snippet may
// be both a README highlight and a gallery entry), so dupes are checked per file.
for (const f of files) {
  const dupes = f.slugs.filter((s, i) => f.slugs.indexOf(s) !== i)
  if (dupes.length) fail(`duplicate markers in ${rel(f.t.path)}: ${[...new Set(dupes)].join(', ')}`)
}
const missing = allSlugs.filter(s => !fileSlugs.includes(s))
const orphans = fileSlugs.filter(s => !allSlugs.includes(s))
if (missing.length) fail(`markers reference snippets with no source file: ${missing.join(', ')}`)
if (orphans.length) fail(`snippet source files shown by no marker: ${orphans.map(s => s + '.cav').join(', ')}`)

// Compute every output (each target file + the SVGs) into a path -> content map.
const outputs = new Map()
for (const slug of allSlugs) {
  const code = readFileSync(join(SNIPPETS, `${slug}.cav`), 'utf8').replace(/\n+$/, '')
  outputs.set(join(DOCS, `snippet-${slug}-light.svg`), render(code, LIGHT.name))
  outputs.set(join(DOCS, `snippet-${slug}-dark.svg`), render(code, DARK.name))
}
for (const f of files) outputs.set(f.t.path, f.src.replace(MARKER, (_, slug) => block(slug, f.t)))

if (CHECK) {
  const stale = [...outputs]
    .filter(([p, content]) => !existsSync(p) || readFileSync(p, 'utf8') !== content)
    .map(([p]) => p.replace(`${ROOT}/`, ''))
  if (stale.length) {
    console.error('gen.mjs: out of date. Run `npm run build` in assets/readme-snippets and commit:')
    for (const p of stale) console.error(`  - ${p}`)
    process.exit(1)
  }
  console.log(`gen.mjs: up to date (${allSlugs.length} snippets).`)
} else {
  for (const [p, content] of outputs) writeFileSync(p, content)
  console.log(`Rendered ${allSlugs.length} snippets (light + dark) and updated ${files.length} file(s).`)
}
