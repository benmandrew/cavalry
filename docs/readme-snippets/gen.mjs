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
const README = join(ROOT, 'README.md')

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
const SIZE = 14, LINE = 21, ADV = SIZE * 0.6, PADX = 18, PADY = 16
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

// The README block for a snippet: a link to the source .cav wrapping a
// theme-aware <picture>. Emitted verbatim so re-running is a no-op.
const block = slug => `<!-- snippet: ${slug} -->
<a href="docs/readme-snippets/snippets/${slug}.cav">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/snippet-${slug}-dark.svg">
    <img alt="Cavalry code snippet" src="docs/snippet-${slug}-light.svg">
  </picture>
</a>
<!-- /snippet -->`

const MARKER = /<!-- snippet: (\S+) -->[\s\S]*?<!-- \/snippet -->/g

const readmeSrc = readFileSync(README, 'utf8')
const readmeSlugs = [...readmeSrc.matchAll(MARKER)].map(m => m[1])
const fileSlugs = readdirSync(SNIPPETS)
  .filter(f => f.endsWith('.cav')).map(f => f.replace(/\.cav$/, '')).sort()

// Guard against the two ways the mapping can drift: a marker naming a snippet
// that doesn't exist, and a snippet source with no marker to render it.
const dupes = readmeSlugs.filter((s, i) => readmeSlugs.indexOf(s) !== i)
const missing = readmeSlugs.filter(s => !fileSlugs.includes(s))
const orphans = fileSlugs.filter(s => !readmeSlugs.includes(s))
if (dupes.length) fail(`duplicate README markers: ${[...new Set(dupes)].join(', ')}`)
if (missing.length) fail(`README references snippets with no source file: ${missing.join(', ')}`)
if (orphans.length) fail(`snippet source files with no README marker: ${orphans.map(s => s + '.cav').join(', ')}`)

// Compute every output (README + SVGs) into a path -> content map.
const outputs = new Map()
for (const slug of readmeSlugs) {
  const code = readFileSync(join(SNIPPETS, `${slug}.cav`), 'utf8').replace(/\n+$/, '')
  outputs.set(join(DOCS, `snippet-${slug}-light.svg`), render(code, LIGHT.name))
  outputs.set(join(DOCS, `snippet-${slug}-dark.svg`), render(code, DARK.name))
}
outputs.set(README, readmeSrc.replace(MARKER, (_, slug) => block(slug)))

if (CHECK) {
  const stale = [...outputs]
    .filter(([p, content]) => !existsSync(p) || readFileSync(p, 'utf8') !== content)
    .map(([p]) => p.replace(`${ROOT}/`, ''))
  if (stale.length) {
    console.error('gen.mjs: out of date. Run `npm run build` in docs/readme-snippets and commit:')
    for (const p of stale) console.error(`  - ${p}`)
    process.exit(1)
  }
  console.log(`gen.mjs: up to date (${readmeSlugs.length} snippets).`)
} else {
  for (const [p, content] of outputs) writeFileSync(p, content)
  console.log(`Rendered ${readmeSlugs.length} snippets (light + dark) and updated the README.`)
}
