// Bundled by esbuild into dist/highlight.js, exposing a global
// `cavalryHighlight`. Live syntax highlighting for the editor overlay, driven
// by the SAME TextMate grammar and per-scope colour customizations as the
// README snippet SVGs (assets/readme-snippets/gen.mjs) -- so the in-browser
// editor and the docs render identically.
//
// Uses Shiki's JavaScript regex engine (oniguruma-to-es), not the oniguruma
// wasm engine, so this stays a single self-contained JS bundle with no extra
// wasm to fetch -- the grammar is simple enough to translate cleanly.
import { createHighlighterCoreSync } from "shiki/core";
import { createJavaScriptRegexEngine } from "shiki/engine/javascript";
import lightPlus from "shiki/themes/light-plus.mjs";
import darkPlus from "shiki/themes/dark-plus.mjs";
import grammar from "../../editors/vscode/syntaxes/cavalry.tmLanguage.json";
// NOTE: imported through esbuild's json loader, so this must stay strict JSON
// (no comments). gen.mjs reads the same file but tolerates JSONC.
import settings from "../../.vscode/settings.json";

// Mirror the editor's per-scope customizations (italic specification keywords,
// blue function calls) into both themes, exactly as gen.mjs does for the SVGs.
const rules = settings["editor.tokenColorCustomizations"]?.textMateRules ?? [];
const withRules = (t) => ({
  ...t,
  name: `cav-${t.name}`,
  tokenColors: [...(t.tokenColors ?? []), ...rules],
});
const LIGHT = withRules(lightPlus);
const DARK = withRules(darkPlus);

const highlighter = createHighlighterCoreSync({
  themes: [LIGHT, DARK],
  langs: [{ ...grammar, name: "cavalry" }],
  engine: createJavaScriptRegexEngine({ forgiving: true }),
});

const esc = (s) =>
  s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

// Render `code` to an HTML string of styled <span>s with literal newlines
// preserved, for a `white-space: pre` layer sitting under the textarea. We
// build the markup ourselves (rather than codeToHtml) to drop Shiki's <pre>
// background/padding and to keep the rendered text matching the textarea's
// value character-for-character, so the caret lines up.
function toHtml(code, dark) {
  const { tokens } = highlighter.codeToTokens(code, {
    lang: "cavalry",
    theme: dark ? DARK.name : LIGHT.name,
  });
  let html = tokens
    .map((line) =>
      line
        .map((t) => {
          const fs = t.fontStyle || 0; // bitmask: 1 italic, 2 bold, 4 underline
          let style = `color:${t.color}`;
          if (fs & 1) style += ";font-style:italic";
          if (fs & 2) style += ";font-weight:bold";
          if (fs & 4) style += ";text-decoration:underline";
          return `<span style="${style}">${esc(t.content)}</span>`;
        })
        .join(""),
    )
    .join("\n");
  // Shiki may drop a trailing newline that the textarea keeps (as a blank final
  // line). Reconstruct the plain text it emitted and append whatever suffix of
  // the input it left off, so the two layers stay aligned to the last line.
  const plain = tokens
    .map((line) => line.map((t) => t.content).join(""))
    .join("\n");
  if (code.length > plain.length) html += esc(code.slice(plain.length));
  return html;
}

globalThis.cavalryHighlight = { toHtml };
