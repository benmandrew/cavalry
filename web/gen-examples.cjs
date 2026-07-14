// Generate dist/examples.js -- the list of example programs the editor's picker
// offers -- from the snippets embedded in the examples gallery
// (assets/readme-snippets/EXAMPLES.md), so the two never drift. Each
// `<!-- snippet: slug -->` marker contributes one example: its code is
// assets/readme-snippets/snippets/<slug>.cav and its title is the nearest
// preceding `## ` heading (the gallery's own section names, minus the ★ that
// flags the ones also shown on the front page). Order follows the file top to
// bottom.
//
// Output is a plain script setting globalThis.cavalryExamples; it lands in
// dist/ (gitignored, like the other build artifacts) and is loaded before
// app.js. Run standalone or via `npm run build`.
const fs = require("fs");
const path = require("path");

const ROOT = path.join(__dirname, "..");
const GALLERY = path.join(ROOT, "assets", "readme-snippets", "EXAMPLES.md");
const SNIPPETS = path.join(ROOT, "assets", "readme-snippets", "snippets");
const OUT = path.join(__dirname, "dist", "examples.js");

const gallery = fs.readFileSync(GALLERY, "utf8");

const markerRe = /<!-- snippet: (\S+) -->/g;
const examples = [];
let m;
while ((m = markerRe.exec(gallery))) {
  const slug = m[1];
  // Title: the last `## ` heading before this marker, minus the ★ flag.
  const headings = gallery.slice(0, m.index).match(/^##\s+.+$/gm);
  const title = headings
    ? headings[headings.length - 1]
        .replace(/^##\s+/, "")
        .replace(/\s*★\s*$/, "")
        .trim()
    : slug;
  const file = path.join(SNIPPETS, `${slug}.cav`);
  if (!fs.existsSync(file)) {
    console.error(`gen-examples: gallery references missing snippet ${slug}.cav`);
    process.exit(1);
  }
  const code = fs.readFileSync(file, "utf8").replace(/\n+$/, "");
  examples.push({ slug, title, code });
}

if (!examples.length) {
  console.error("gen-examples: no snippet markers found in EXAMPLES.md");
  process.exit(1);
}

fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(
  OUT,
  `globalThis.cavalryExamples = ${JSON.stringify(examples, null, 2)};\n`,
);
console.log(`gen-examples: wrote ${examples.length} examples to dist/examples.js`);
