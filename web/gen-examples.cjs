// Generate dist/examples.js -- the list of example programs the editor's picker
// offers -- from the very snippets embedded in the top-level README, so the two
// never drift. Each `<!-- snippet: slug -->` marker under "## Example programs"
// contributes one example: its code is assets/readme-snippets/snippets/<slug>.cav
// and its title is the nearest preceding `### ` heading (the README's own
// section names). Order follows the README top to bottom.
//
// Output is a plain script setting globalThis.cavalryExamples; it lands in
// dist/ (gitignored, like the other build artifacts) and is loaded before
// app.js. Run standalone or via `npm run build`.
const fs = require("fs");
const path = require("path");

const ROOT = path.join(__dirname, "..");
const README = path.join(ROOT, "README.md");
const SNIPPETS = path.join(ROOT, "assets", "readme-snippets", "snippets");
const OUT = path.join(__dirname, "dist", "examples.js");

const readme = fs.readFileSync(README, "utf8");
const start = readme.indexOf("## Example programs");
if (start < 0) {
  console.error("gen-examples: no '## Example programs' section in README.md");
  process.exit(1);
}
const section = readme.slice(start);

const markerRe = /<!-- snippet: (\S+) -->/g;
const examples = [];
let m;
while ((m = markerRe.exec(section))) {
  const slug = m[1];
  // Title: the last `### ` heading before this marker.
  const headings = section.slice(0, m.index).match(/^###\s+.+$/gm);
  const title = headings
    ? headings[headings.length - 1].replace(/^###\s+/, "").trim()
    : slug;
  const file = path.join(SNIPPETS, `${slug}.cav`);
  if (!fs.existsSync(file)) {
    console.error(`gen-examples: README references missing snippet ${slug}.cav`);
    process.exit(1);
  }
  const code = fs.readFileSync(file, "utf8").replace(/\n+$/, "");
  examples.push({ slug, title, code });
}

if (!examples.length) {
  console.error("gen-examples: no snippet markers found under '## Example programs'");
  process.exit(1);
}

fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(
  OUT,
  `globalThis.cavalryExamples = ${JSON.stringify(examples, null, 2)};\n`,
);
console.log(`gen-examples: wrote ${examples.length} examples to dist/examples.js`);
