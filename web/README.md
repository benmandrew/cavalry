# Cavalry in the browser

A client-side build of the verifier: write Cavalry in the editor and it is
checked entirely in the browser, with no server round-trip. The OCaml pipeline
is compiled to JavaScript with *js_of_ocaml*, and Z3 runs as a WebAssembly
module.

## How it fits together

The verifier splits cleanly at one seam. Everything up to and including the
*weakest-liberal-precondition* calculus is pure OCaml; only the prover call
shells out. So the OCaml half is compiled to JS and made to *print* each proof
obligation as SMT-LIB2 (via Why3's `Driver.print_task`) rather than spawn Z3,
and the JS half feeds those strings to Z3-wasm.

```
 editor (index.html, app.js)
    │  source string
    ▼
 verifier.bc.js         OCaml pipeline compiled by js_of_ocaml
    │  cavalryObligations(src) -> JSON { procedures:[ obligations:[ smtlib, ceSmtlib, ceId ] ] }
    ▼
 verify-core.js         async solve loop (one Z3 context per obligation)
    │  smtlib          (on failure: ceSmtlib -> raw model -> cavalryRenderCounterexample(ceId, …))
    ▼
 z3-built.js + .wasm    Z3 4.16, Emscripten; solves in its own worker threads
    │  unsat | sat | unknown
    ▼
 verdict + located diagnostics + counterexample
```

Everything runs on the main thread. `cavalryObligations` is a fast synchronous
call; each Z3 solve is awaited, and because Z3 works in its own threads the
awaits hand the main thread back to the event loop, so typing stays responsive.
A monotonic *generation* counter tags each run: a newer keystroke supersedes an
in-flight run (it bails between obligations) and late results are dropped. A
syntax/type error mid-edit keeps the last good verdict on screen rather than
flashing.

Why3's standard library and the Z3 driver are read from files: with no real
filesystem in the browser, they are embedded (via `ocaml-crunch`) and served
into the js_of_ocaml pseudo-filesystem with `Sys_js.mount`. `Smt.Prover`'s
browser hook then builds the environment and driver from those, bypassing
Why3's prover-detection (which needs a native Z3 binary).

A failed obligation also carries a *counterexample* — the entry-state values
that break the spec. Natively, Why3 runs Z3 and parses its model in one call;
in the browser those halves are split across the same seam as the verdict. Each
obligation is printed a second time with the *counterexamples* driver (which
emits `(get-model)`), and the Why3 `printing_info` that maps the printed SMT
names back to source variables is retained under the integer `ceId`. On a `sat`
answer, `verify-core.js` hands the raw model text and `ceId` back to
`cavalryRenderCounterexample`, which parses the model with Why3's `smtv2` parser
and renders it through the *same* `render_counterexample`/`format_counterexample`
the native CLI uses — arrays expanded, internal WLP variables hidden. The
counterexample driver is a non-incremental variant (`z3_487_ce_batch.drv`),
since the stock one emits a `(check-sat)` per incremental step and Z3-wasm
evaluates the whole script at once. It is best-effort: a model that fails to
parse leaves the verdict untouched and simply shows no witness.

## Editor

The editor is a plain `<textarea>` with its text made transparent, laid over a
highlighted `<pre>` and a line-number gutter that JS keeps scrolled in lockstep
— so the caret sits over syntax-coloured code with no editor framework. The
highlighting is driven by the same VS Code TextMate grammar
(`editors/vscode/syntaxes/cavalry.tmLanguage.json`) and per-scope colour
customizations (`.vscode/settings.json`) that render the README's snippet SVGs,
so the browser editor and the docs match. `src/highlight-entry.js` bundles
[Shiki](https://shiki.style) into `dist/highlight.js`; it uses Shiki's
JavaScript regex engine (`oniguruma-to-es`) rather than the oniguruma wasm, so
the bundle is self-contained JS with no extra wasm to fetch.

A header picker offers a handful of example programs so a first-time visitor can
tour the language's features. The list is generated, not hand-maintained:
`gen-examples.cjs` reads the same snippets the top-level README embeds
(`assets/readme-snippets/snippets/`), takes their order and section titles from
the README itself, and writes `dist/examples.js` (loaded before `app.js`). Add a
snippet to the README and it shows up in the picker on the next build.

## Cross-origin isolation

Z3-wasm uses threads, hence `SharedArrayBuffer`, hence the page must be
*cross-origin isolated*. `serve.cjs` sends `Cross-Origin-Opener-Policy:
same-origin` and `Cross-Origin-Embedder-Policy: require-corp`. Any host for this
site must set the same two headers (plain GitHub Pages cannot, without a
service-worker shim).

## Build and run

The toolchain is provided by the dev shell (`nix develop` at the repo root): it
supplies Node and, via opam, the OCaml web dependencies declared in the separate
`cavalry-web` package (`js_of_ocaml`, `zarith_stubs_js`, `crunch`, `yojson`).
Outside nix, install them with `opam install --deps-only .` (which now covers
both `cavalry` and `cavalry-web`) and provide Node yourself. The core
`cavalry` package does not depend on any of these, so `opam install cavalry`
stays lean.

```bash
npm install            # z3-solver, shiki, esbuild, puppeteer-core
npm run build          # vendor Why3 data + Z3 assets, build verifier.bc.js, bundle z3-api.js + highlight.js
npm run serve          # http://localhost:8099
```

`npm run build` runs five steps you can invoke separately: `vendor` (copy the
Why3 stdlib/drivers and Z3's `z3-built.{js,wasm}` into place), `build:ocaml`
(dune → `verifier.bc.js`), `build:z3api` (esbuild bundles z3-solver's API),
`build:highlight` (esbuild bundles the editor's Shiki highlighter), and
`build:examples` (`gen-examples.cjs` writes `dist/examples.js`).

The web build is gated to dune's `web` profile (`dune build --profile web`),
which `npm run build` passes. A plain `dune build` -- and the core CI matrix --
skips it, so js_of_ocaml is not required to build the CLI, least of all on the
OCaml 4.14 leg. The Why3 stdlib and drivers are committed under `why3data/`
(embedded into the JS at build time), so the build needs neither the `why3` CLI
nor a prover binary; refresh them with `npm run refresh:why3data` when bumping
the pinned Why3.

## Tests

```bash
npm run test:node      # OCaml pipeline + Z3 (node): verdicts on valid/invalid/example programs
npm run test:browser   # headless Chrome: drives verify-while-typing end to end
```

`test:browser` starts its own server and drives Chrome via `puppeteer-core`;
`CHROME_PATH` selects the browser (defaults to the macOS install locally, set by
CI to the runner's Chrome). Both run in CI's `web` job (`.github/workflows/`,
pinned to OCaml 5.5.0), which also builds the site with `npm run build`.
