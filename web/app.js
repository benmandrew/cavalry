// Main thread: the editor, the debounce/generation logic, and rendering.
// It owns no verification logic -- it ships source to the worker and paints
// whatever verdict comes back, discarding anything stale.

const editor = document.getElementById("editor");
const statusPill = document.getElementById("status");
const results = document.getElementById("results");
const gutter = document.getElementById("gutter");
const highlightCode = document.getElementById("highlight-code");
const examplePicker = document.getElementById("example-picker");

// The example programs come from dist/examples.js (generated from the README
// snippets). Fall back to a single built-in program if that bundle is missing,
// so the editor is never empty.
const FALLBACK = {
  slug: "euclidean-division",
  title: "Euclidean division",
  code: `// Compute q = x / y and r = x % y by repeated subtraction.
procedure euclidean_div () =
  requires { x >= 0 }
  ensures { x = q * y + r && 0 <= r && r < y }
  writes { q, r }
  q := 0;
  r := x;
  while r >= y do
    invariant { x = q * y + r && 0 <= r }
    r := r - y;
    q := q + 1
  end
end

{ true }
x := 42;
y := 17;
q := 0;
r := 0;
euclidean_div()
{ q = 2 && r = 8 }`,
};

const EXAMPLES =
  Array.isArray(self.cavalryExamples) && self.cavalryExamples.length
    ? self.cavalryExamples
    : [FALLBACK];
// Open on the Hoare-triple example if present (the simplest starting point),
// else the first one.
const DEFAULT_SLUG =
  EXAMPLES.find((e) => e.slug === "hoare-triple")?.slug ?? EXAMPLES[0].slug;

// Populate the header picker and load the chosen program (its highlight and
// gutter repainted, and -- once Z3 is up -- a fresh verification kicked off).
for (const ex of EXAMPLES) {
  const opt = document.createElement("option");
  opt.value = ex.slug;
  opt.textContent = ex.title;
  examplePicker.appendChild(opt);
}
examplePicker.value = DEFAULT_SLUG;
editor.value = (EXAMPLES.find((e) => e.slug === DEFAULT_SLUG) ?? EXAMPLES[0]).code;

examplePicker.addEventListener("change", () => {
  const ex = EXAMPLES.find((e) => e.slug === examplePicker.value);
  if (!ex) return;
  editor.value = ex.code;
  editor.scrollTop = 0;
  editor.scrollLeft = 0;
  refreshEditor();
  verifyNow();
});

// --- Editor chrome: line-number gutter + live TextMate highlighting ---------
// The visible text is a highlighted <pre> under a transparent textarea; the
// gutter is a third parallel column. Highlighting is synchronous and cheap for
// these small programs, so it runs on every keystroke (verification stays
// debounced below).

const darkQuery = matchMedia("(prefers-color-scheme: dark)");

// Repaint the highlight layer from the current source. Falls back to plain
// (escaped, via textContent) if the Shiki bundle failed to load.
function paintHighlight() {
  if (self.cavalryHighlight) {
    highlightCode.innerHTML = self.cavalryHighlight.toHtml(editor.value, darkQuery.matches);
  } else {
    highlightCode.textContent = editor.value;
  }
}

// Rebuild the gutter to one number per source line.
function paintGutter() {
  const n = editor.value.split("\n").length;
  let s = "";
  for (let i = 1; i <= n; i++) s += (i > 1 ? "\n" : "") + i;
  gutter.textContent = s;
}

// Keep the highlight layer and gutter scrolled in step with the textarea. The
// highlight <pre> and gutter are overflow:hidden but still scroll under program
// control, so we drive them from the textarea's scroll offsets.
function syncScroll() {
  const hl = document.getElementById("highlight");
  hl.scrollTop = editor.scrollTop;
  hl.scrollLeft = editor.scrollLeft;
  gutter.scrollTop = editor.scrollTop;
}

function refreshEditor() {
  paintGutter();
  paintHighlight();
  syncScroll();
}

editor.addEventListener("scroll", syncScroll);
darkQuery.addEventListener("change", paintHighlight);
refreshEditor();

// gen: monotonic id for the latest request. currentGen: the gen we still want
// rendered -- results older than it are dropped. lastGood: the last successful
// verdict, kept on screen (dimmed) when the program becomes unparseable mid-edit
// so the panel does not flash on every keystroke.
let gen = 0;
let currentGen = 0;
let lastGood = null;
let solve = null; // set once Z3 has loaded

// Per-obligation solve budget. Matches the native CLI's default --timeout; a
// solve that blows it is interrupted and reported as a "timeout" (see
// makeSolver in verify-core.js).
const SOLVE_TIMEOUT_MS = 10000;

function setPill(cls, text) {
  statusPill.className = "pill " + cls;
  statusPill.textContent = text;
}

// A count-up clock shown in the status pill while a verification is in flight,
// so a slow (or hung, up to the per-obligation timeout) solve is visibly making
// time rather than looking frozen. [busyLabel] holds the latest progress text
// (e.g. "verifying… 2/3"); the ticker appends elapsed seconds every 100ms.
let busyTimer = null;
let busyStart = 0;
let busyLabel = "verifying…";

function paintBusy() {
  const secs = ((performance.now() - busyStart) / 1000).toFixed(1);
  setPill("busy", `${busyLabel} ${secs}s`);
}

// Start (or restart -- a newer run supersedes the timer) the count-up.
function startBusy() {
  busyStart = performance.now();
  busyLabel = "verifying…";
  if (busyTimer) clearInterval(busyTimer);
  paintBusy();
  busyTimer = setInterval(paintBusy, 100);
}

function stopBusy() {
  if (busyTimer) {
    clearInterval(busyTimer);
    busyTimer = null;
  }
}

// Verify the current editor contents. Runs on the main thread: the OCaml
// pipeline (cavalryObligations) is a fast synchronous call, and each Z3 solve is
// awaited -- Z3 runs in its own worker threads, so awaiting yields the main
// thread back to the event loop and typing stays responsive. A newer keystroke
// bumps currentGen; in-flight runs see the mismatch (isStale) and bail.
async function verifyNow() {
  if (!solve) return;
  gen += 1;
  const myGen = gen;
  currentGen = myGen;
  startBusy();
  let parsed;
  try {
    parsed = JSON.parse(self.cavalryObligations(editor.value));
  } catch (e) {
    if (myGen === currentGen) {
      stopBusy();
      setPill("bad", "error");
      results.replaceChildren();
      const v = document.createElement("div");
      v.className = "verdict bad";
      v.textContent = "Internal error: " + (e && e.message || e);
      results.appendChild(v);
    }
    return;
  }
  const result = await VerifyCore.solveObligations(parsed, solve, {
    isStale: () => myGen !== currentGen,
    onProgress: (done, total) => {
      if (myGen === currentGen) {
        busyLabel = `verifying… ${done}/${total}`;
        paintBusy();
      }
    },
    renderCounterexample: (id, output, candidate) =>
      self.cavalryRenderCounterexample(id, output, candidate),
  });
  if (myGen !== currentGen || result.stale) return; // superseded
  render(myGen, result);
}

let debounceTimer = null;
editor.addEventListener("input", () => {
  refreshEditor(); // highlight + gutter update immediately; verification waits
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(verifyNow, 300);
});

function jumpTo(line, col) {
  const lines = editor.value.split("\n");
  let pos = 0;
  for (let i = 0; i < line - 1 && i < lines.length; i++) pos += lines[i].length + 1;
  pos += Math.max(0, col - 1);
  editor.focus();
  editor.setSelectionRange(pos, pos);
  syncScroll(); // focusing may scroll the textarea to the caret; follow it
}

function locSpan(loc) {
  if (!loc) return "";
  const s = document.createElement("span");
  s.className = "loc";
  s.textContent = `line ${loc.line}:${loc.col}`;
  s.onclick = () => jumpTo(loc.line, loc.col);
  return s;
}

function render(gen, result, { stale } = {}) {
  stopBusy(); // a terminal verdict is about to own the pill
  results.className = stale ? "stale" : "";
  results.replaceChildren();

  const verdict = document.createElement("div");
  results.appendChild(verdict);

  if (!result.ok) {
    // A parse/type error. Keep the last good verdict dimmed above the error.
    if (lastGood) render.appendLastGood(results);
    verdict.className = "verdict busy";
    verdict.textContent = result.kind === "type" ? "Type error" : "Syntax error";
    const ul = document.createElement("ul");
    ul.className = "findings";
    const li = document.createElement("li");
    li.className = "err";
    li.append(result.error + "  ");
    const ls = locSpan(result.loc);
    if (ls) li.append(ls);
    ul.appendChild(li);
    results.appendChild(ul);
    setPill("busy", result.kind === "type" ? "type error" : "syntax error");
    return;
  }

  lastGood = { gen, result };
  if (result.verified) {
    verdict.className = "verdict ok";
    verdict.textContent = "✓ Verified";
    const note = document.createElement("div");
    note.className = "note";
    note.textContent = `${result.total} proof obligation${result.total === 1 ? "" : "s"} discharged by Z3.`;
    results.appendChild(note);
    setPill("ok", "verified");
  } else {
    verdict.className = "verdict bad";
    verdict.textContent = `✗ Not verified — ${result.failures.length} of ${result.total} obligation${result.total === 1 ? "" : "s"} failed`;
    const ul = document.createElement("ul");
    ul.className = "findings";
    for (const f of result.failures) {
      const li = document.createElement("li");
      const where = f.proc ? `${f.proc}: ` : "";
      li.append(`${where}${f.expl} `);
      const ls = locSpan(f.loc);
      if (ls) li.append(ls);
      const v = document.createElement("span");
      v.className = "note";
      v.textContent =
        f.verdict === "timeout"
          ? `  (timed out after ${SOLVE_TIMEOUT_MS / 1000}s)`
          : `  (${f.verdict})`;
      li.append(v);
      // A counterexample block (variable = value lines), when Z3 produced one.
      if (f.counterexample) {
        const pre = document.createElement("pre");
        pre.className = "counterexample";
        pre.textContent = f.counterexample.replace(/\n$/, "");
        li.append(pre);
      }
      ul.appendChild(li);
    }
    results.appendChild(ul);
    setPill("bad", "not verified");
  }
}

// Re-render the retained good verdict, dimmed, above a fresh error.
render.appendLastGood = (root) => {
  const d = document.createElement("div");
  d.className = "note";
  d.style.marginBottom = "8px";
  const r = lastGood.result;
  d.textContent = r.verified
    ? `(last verified: ${r.total} obligations)`
    : `(last result: ${r.failures.length}/${r.total} failed)`;
  root.appendChild(d);
};

// Load Z3 once (its wasm is ~32 MB, so this takes a moment), then do a first
// verification of the sample.
(async () => {
  try {
    const { Z3 } = await self.z3api.init();
    solve = VerifyCore.makeSolver(Z3, SOLVE_TIMEOUT_MS);
    verifyNow();
  } catch (e) {
    stopBusy();
    setPill("bad", "Z3 failed");
    results.replaceChildren();
    const v = document.createElement("div");
    v.className = "verdict bad";
    v.textContent = "Failed to load Z3: " + (e && e.message || e);
    results.appendChild(v);
  }
})();
