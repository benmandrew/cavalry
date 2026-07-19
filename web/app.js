// Main thread: the editor, the debounce/generation logic, and rendering.
// It owns no verification logic -- it ships source to the OCaml pipeline and Z3,
// then paints the proof outline, per-obligation verdicts, and the detail pane,
// discarding anything stale.

const editor = document.getElementById("editor");
const statusPill = document.getElementById("status");
const gutter = document.getElementById("gutter");
const highlightCode = document.getElementById("highlight-code");
const examplePicker = document.getElementById("example-picker");
const procTabs = document.getElementById("proc-tabs");
const outlineEl = document.getElementById("outline");
const detailEl = document.getElementById("detail");
const summaryEl = document.getElementById("verdict-summary");
const stepRange = document.getElementById("step-range");
const stepPrev = document.getElementById("step-prev");
const stepNext = document.getElementById("step-next");
const stepLabel = document.getElementById("step-label");
const lineHighlight = document.getElementById("line-highlight");
// The source line currently marked in the editor (the selected step's line), or
// null. Declared up here because refreshEditor() -> syncScroll() reads it during
// the initial paint, before the editor-metrics block below runs.
let highlightedLine = null;

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
  positionEditorHighlight();
}

function refreshEditor() {
  paintGutter();
  paintHighlight();
  syncScroll();
}

editor.addEventListener("scroll", syncScroll);
darkQuery.addEventListener("change", paintHighlight);
refreshEditor();

// Editor metrics, mirrored from the CSS custom properties, so the outline can
// scroll the source to a given line without stealing focus (jumpTo, which
// focuses and selects, is reserved for an explicit click on a location).
const LINE_H = 21;
const PAD_Y = 16;

function revealLine(line) {
  if (!line) return;
  const top = PAD_Y + (line - 1) * LINE_H;
  const view = editor.clientHeight;
  if (top < editor.scrollTop + LINE_H || top > editor.scrollTop + view - LINE_H) {
    editor.scrollTop = Math.max(0, top - view / 2);
    syncScroll();
  }
}

// The #line-highlight band is drawn behind the editor and repositioned on every
// scroll to track [highlightedLine] (declared up top for the initial paint).
function positionEditorHighlight() {
  if (!highlightedLine) return;
  lineHighlight.style.top = PAD_Y + (highlightedLine - 1) * LINE_H - editor.scrollTop + "px";
}

function setEditorHighlight(line) {
  highlightedLine = line || null;
  if (!highlightedLine) {
    lineHighlight.style.display = "none";
    return;
  }
  lineHighlight.style.display = "block";
  positionEditorHighlight();
}

// gen: monotonic id for the latest request. currentGen: the gen we still want
// rendered -- results older than it are dropped.
let gen = 0;
let currentGen = 0;
let solve = null; // set once Z3 has loaded

// Proof-pane state: the parsed pipeline output (procedures + obligations +
// outline), the last solve result (verdicts) or null while solving, the last
// good parse kept on screen (dimmed) when the program becomes unparseable, and
// the currently-selected procedure and step within it.
let parsedNow = null;
let lastGood = null;
let activeProcIdx = 0;
let activeStepIdx = 0;

// Per-obligation solve budget. Matches the native CLI's default --timeout; a
// solve that blows it is interrupted and reported as a "timeout" (see
// makeSolver in verify-core.js).
const SOLVE_TIMEOUT_MS = 10000;

function setPill(cls, text) {
  statusPill.className = "pill " + cls;
  statusPill.textContent = text;
}

function setSummary(cls, text) {
  summaryEl.className = "verdict-summary " + cls;
  summaryEl.textContent = text;
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

// --- Verdict helpers --------------------------------------------------------
// An obligation's verdict is "unsat" (proved), a failing token, or undefined
// (not yet solved). Map to a status class shared by tabs, outline rails, and the
// detail pane.
function statusOf(verdict) {
  if (verdict === undefined || verdict === null) return "pending";
  if (verdict === "unsat") return "ok";
  if (verdict === "sat") return "bad";
  return "warn"; // unknown / timeout
}

// Combine several statuses into the worst, for a line or a whole procedure.
function worst(statuses) {
  if (statuses.includes("bad")) return "bad";
  if (statuses.includes("warn")) return "warn";
  if (statuses.includes("pending")) return "pending";
  return statuses.length ? "ok" : "none";
}

function verdictText(verdict) {
  if (verdict === undefined || verdict === null) return "solving…";
  if (verdict === "unsat") return "proved";
  if (verdict === "timeout") return `timeout (${SOLVE_TIMEOUT_MS / 1000}s)`;
  if (verdict === "sat") return "refuted";
  return verdict; // unknown
}

// The trimmed source text of a 1-based line, for interleaving in the outline.
function sourceLine(line) {
  if (!line) return "";
  const lines = editor.value.split("\n");
  return (lines[line - 1] ?? "").trim();
}

// A coarse label for a statement, inferred from its source text -- purely a
// reading aid next to the outline assertion.
function inferKind(text) {
  const t = text.trim();
  if (/^while\b/.test(t)) return "while";
  if (/^if\b/.test(t)) return "if";
  if (/\barray\s*\(/.test(t) && t.includes(":=")) return "array alloc";
  if (/^[A-Za-z_]\w*\s*\[.*\]\s*:=/.test(t)) return "array write";
  if (t.includes(":=")) return "assignment";
  if (/^[A-Za-z_]\w*\s*\(/.test(t)) return "call";
  return "";
}

// --- Jumping ----------------------------------------------------------------
function jumpTo(line, col) {
  const lines = editor.value.split("\n");
  let pos = 0;
  for (let i = 0; i < line - 1 && i < lines.length; i++) pos += lines[i].length + 1;
  pos += Math.max(0, col - 1);
  editor.focus();
  editor.setSelectionRange(pos, pos);
  syncScroll();
}

// --- Steps ------------------------------------------------------------------
// Build the ordered list of steps for a procedure: one per outline entry (the
// assertion threaded before a statement), each with the obligations located on
// its line. Any obligation with no location (a whole-procedure postcondition) or
// whose line matches no statement is gathered into a trailing "postcondition"
// step, so nothing Z3 checked is hidden.
function stepsFor(proc) {
  const outline = proc.outline || [];
  const obs = proc.obligations || [];
  const lineOf = (o) => (o.loc ? o.loc.line : null);
  const outlineLines = new Set(outline.map((s) => (s.loc ? s.loc.line : null)));

  const steps = outline.map((s) => ({
    kind: "stmt",
    loc: s.loc,
    assertion: s.assertion,
    obligations: obs.filter((o) => o.loc && s.loc && lineOf(o) === s.loc.line),
  }));

  const trailing = obs.filter((o) => !o.loc || !outlineLines.has(lineOf(o)));
  if (trailing.length) {
    steps.push({ kind: "post", loc: null, assertion: null, obligations: trailing });
  }
  return steps;
}

// --- Rendering: procedure tabs ---------------------------------------------
function renderTabs() {
  procTabs.replaceChildren();
  if (!parsedNow || !parsedNow.ok) return;
  parsedNow.procedures.forEach((proc, i) => {
    const btn = document.createElement("button");
    btn.className = "proc-tab" + (i === activeProcIdx ? " active" : "");
    btn.type = "button";
    const dot = document.createElement("span");
    const st = worst((proc.obligations || []).map((o) => statusOf(o.verdict)));
    dot.className = "dot" + (st === "none" ? "" : " " + st);
    btn.appendChild(dot);
    btn.appendChild(document.createTextNode(proc.name));
    btn.onclick = () => selectProc(i);
    procTabs.appendChild(btn);
  });
}

// --- Rendering: the outline -------------------------------------------------
function renderOutline() {
  outlineEl.replaceChildren();
  if (!parsedNow || !parsedNow.ok) return;
  const proc = parsedNow.procedures[activeProcIdx];
  if (!proc) return;
  const steps = stepsFor(proc);

  if (!steps.length) {
    const p = document.createElement("div");
    p.className = "ol-bookend";
    p.textContent = "// no statements to outline";
    outlineEl.appendChild(p);
    return;
  }

  steps.forEach((step, i) => {
    const row = document.createElement("div");
    row.className = "ol-row" + (i === activeStepIdx ? " active" : "");
    row.onclick = () => selectStep(i);

    const lineStatus = worst(step.obligations.map((o) => statusOf(o.verdict)));

    // The assertion line: "{ … }", brace tinted by any obligation on the line.
    const assert = document.createElement("div");
    assert.className = "ol-assert" + (lineStatus === "none" ? "" : " " + lineStatus);
    const open = document.createElement("span");
    open.className = "brace";
    open.textContent = "{";
    const body = document.createElement("span");
    body.textContent =
      step.kind === "post" ? "postcondition — whole procedure" : step.assertion;
    const close = document.createElement("span");
    close.className = "brace";
    close.textContent = "}";
    assert.append(open, body, close);
    row.appendChild(assert);

    // The statement line: its source line number (gutter-themed), the source
    // text, and an inferred kind label.
    if (step.kind === "stmt") {
      const ln = step.loc && step.loc.line;
      const src = sourceLine(ln);
      const stmt = document.createElement("div");
      stmt.className = "ol-stmt";
      const num = document.createElement("span");
      num.className = "ol-lineno";
      num.textContent = ln ? String(ln) : "";
      const text = document.createElement("span");
      text.textContent = src;
      stmt.append(num, text);
      const kind = inferKind(src);
      if (kind) {
        const k = document.createElement("span");
        k.className = "kw";
        k.textContent = kind;
        stmt.appendChild(k);
      }
      row.appendChild(stmt);
    }
    outlineEl.appendChild(row);
  });
}

// --- Rendering: the detail pane for the active step ------------------------
function renderDetail() {
  detailEl.replaceChildren();
  if (!parsedNow || !parsedNow.ok) return;
  const proc = parsedNow.procedures[activeProcIdx];
  if (!proc) return;
  const steps = stepsFor(proc);
  const step = steps[activeStepIdx];
  if (!step) {
    const p = document.createElement("div");
    p.className = "placeholder";
    p.textContent = "Select a step in the outline.";
    detailEl.appendChild(p);
    return;
  }

  const h = document.createElement("h2");
  h.textContent = step.kind === "post" ? "Postcondition" : "Proof step";
  detailEl.appendChild(h);

  if (step.kind === "stmt") {
    const stmtText = sourceLine(step.loc && step.loc.line);
    detailEl.appendChild(
      field("Statement" + (step.loc ? ` (line ${step.loc.line})` : ""), (el) => {
        const code = document.createElement("span");
        code.className = "assertion";
        code.textContent = stmtText || "—";
        el.appendChild(code);
        const k = inferKind(stmtText);
        if (k) {
          const tag = document.createElement("span");
          tag.className = "kw";
          tag.textContent = k;
          el.appendChild(tag);
        }
      })
    );
    detailEl.appendChild(
      field("Assertion that must hold here", (el) =>
        el.appendChild(renderAssertion(step.assertion))
      )
    );
  }

  // Obligations located on this step.
  const obsWrap = document.createElement("div");
  obsWrap.className = "field";
  const lbl = document.createElement("div");
  lbl.className = "label";
  lbl.textContent = step.obligations.length
    ? "Proof obligations checked here"
    : "";
  obsWrap.appendChild(lbl);

  if (!step.obligations.length) {
    const note = document.createElement("div");
    note.className = "placeholder";
    note.textContent =
      "This step only propagates the assertion; Z3 checks no separate obligation here.";
    obsWrap.appendChild(note);
  } else {
    for (const o of step.obligations) obsWrap.appendChild(obligationCard(o));
  }
  detailEl.appendChild(obsWrap);
}

function field(label, fill) {
  const f = document.createElement("div");
  f.className = "field";
  const l = document.createElement("div");
  l.className = "label";
  l.textContent = label;
  f.appendChild(l);
  fill(f);
  return f;
}

// --- Structured assertion rendering -----------------------------------------
// A WLP assertion is usually "[forall ys.] a1 && ... -> g1 && ..." -- a list of
// assumptions implying a list of guarantees, sometimes under quantifiers. Split
// it into those parts (respecting parentheses/brackets, so nested -> and && are
// left whole) for a readable list rather than one dense line. Anything that does
// not fit the shape falls back to the raw string.

// Indices of every occurrence of [tok] in [s] at bracket depth 0.
function topLevelSplits(s, tok) {
  const idx = [];
  let depth = 0;
  for (let i = 0; i + tok.length <= s.length; i++) {
    const c = s[i];
    if (c === "(" || c === "[") depth++;
    else if (c === ")" || c === "]") depth--;
    else if (depth === 0 && s.startsWith(tok, i)) {
      idx.push(i);
      i += tok.length - 1;
    }
  }
  return idx;
}

// Split [s] on top-level occurrences of [tok] into trimmed, non-empty parts.
function splitTop(s, tok) {
  const idx = topLevelSplits(s, tok);
  const parts = [];
  let start = 0;
  for (const j of idx) {
    parts.push(s.slice(start, j).trim());
    start = j + tok.length;
  }
  parts.push(s.slice(start).trim());
  return parts.filter((p) => p.length);
}

// Decompose an assertion into { quantifier, assumptions, guarantees }. The
// printer emits " -> " and " && " with surrounding spaces, so matching those
// tokens never collides with "-" (subtraction) or "!=" etc.
function decomposeAssertion(s) {
  s = s.trim();
  let quantifier = "";
  // Peel leading quantifiers: "forall y1, y2." / "exists z." (binder vars carry
  // no "." of their own, so the first "." ends the binder list).
  let m;
  while ((m = /^(forall|exists)\s+[^.]*\.\s*/.exec(s))) {
    quantifier += (quantifier ? " " : "") + m[0].trim();
    s = s.slice(m[0].length);
  }
  const arrows = topLevelSplits(s, " -> ");
  let assumptions = [];
  let guarantees;
  if (arrows.length) {
    // Everything left of the first top-level -> is the assumption; the rest (a
    // possibly-nested implication) is what must then hold.
    assumptions = splitTop(s.slice(0, arrows[0]), " && ");
    guarantees = splitTop(s.slice(arrows[0] + 4), " && ");
  } else {
    guarantees = splitTop(s, " && ");
  }
  return { quantifier, assumptions, guarantees };
}

function bulletList(items) {
  const ul = document.createElement("ul");
  ul.className = "assert-list";
  for (const it of items) {
    const li = document.createElement("li");
    li.textContent = it;
    ul.appendChild(li);
  }
  return ul;
}

// Build the structured DOM for an assertion (or a plain block if it is a single
// atom with nothing to split).
function renderAssertion(assertion) {
  const wrap = document.createElement("div");
  wrap.className = "assertion-structured";
  const { quantifier, assumptions, guarantees } = decomposeAssertion(assertion);

  // Nothing to structure: one guarantee, no assumptions, no quantifier -> plain.
  if (!quantifier && !assumptions.length && guarantees.length <= 1) {
    const a = document.createElement("div");
    a.className = "assertion";
    a.textContent = assertion;
    return a;
  }

  if (quantifier) {
    const q = document.createElement("div");
    q.className = "assert-quant";
    q.textContent = quantifier;
    wrap.appendChild(q);
  }
  if (assumptions.length) {
    const lbl = document.createElement("div");
    lbl.className = "assert-tag";
    lbl.textContent = "Assume";
    wrap.append(lbl, bulletList(assumptions));
    const imp = document.createElement("div");
    imp.className = "assert-implies";
    imp.textContent = "⟹";
    wrap.appendChild(imp);
  }
  const lbl = document.createElement("div");
  lbl.className = "assert-tag";
  lbl.textContent = assumptions.length ? "Show" : "Must hold";
  wrap.append(lbl, bulletList(guarantees));
  return wrap;
}

function obligationCard(o) {
  const st = statusOf(o.verdict);
  const card = document.createElement("div");
  card.className = "ob " + st;

  const head = document.createElement("div");
  head.className = "ob-head";
  const expl = document.createElement("span");
  expl.className = "ob-expl";
  expl.textContent = o.expl;
  const v = document.createElement("span");
  v.className = "ob-verdict";
  // The verdict, plus this obligation's Z3 solve time once it has landed.
  v.textContent = verdictText(o.verdict) + (o.verdict && o.ms != null ? ` · ${o.ms} ms` : "");
  head.append(expl, v);
  card.appendChild(head);

  if (o.loc) {
    const loc = document.createElement("span");
    loc.className = "loc";
    loc.textContent = `line ${o.loc.line}:${o.loc.col}`;
    loc.onclick = () => jumpTo(o.loc.line, o.loc.col);
    card.appendChild(loc);
  }

  // The counterexample, when Z3 produced one for a failing obligation.
  if (o.counterexample) {
    const pre = document.createElement("pre");
    pre.className = "counterexample";
    pre.textContent = o.counterexample.replace(/\n$/, "");
    card.appendChild(pre);
  }

  // The SMT-LIB2 sent to Z3, folded away by default.
  if (o.smtlib) {
    const det = document.createElement("details");
    const sum = document.createElement("summary");
    sum.textContent = "SMT-LIB2 sent to Z3";
    const pre = document.createElement("pre");
    pre.textContent = o.smtlib;
    det.append(sum, pre);
    card.appendChild(det);
  }
  return card;
}

// --- Stepper ----------------------------------------------------------------
function renderStepper() {
  const proc = parsedNow && parsedNow.ok && parsedNow.procedures[activeProcIdx];
  const n = proc ? stepsFor(proc).length : 0;
  stepRange.max = String(Math.max(0, n - 1));
  stepRange.value = String(activeStepIdx);
  stepRange.disabled = n <= 1;
  stepPrev.disabled = activeStepIdx <= 0;
  stepNext.disabled = activeStepIdx >= n - 1;
  stepLabel.textContent = n ? `step ${activeStepIdx + 1} / ${n}` : "";
}

function selectStep(i) {
  const proc = parsedNow && parsedNow.ok && parsedNow.procedures[activeProcIdx];
  const n = proc ? stepsFor(proc).length : 0;
  if (!n) return;
  activeStepIdx = Math.max(0, Math.min(n - 1, i));
  const steps = stepsFor(proc);
  const step = steps[activeStepIdx];
  const line = step && step.loc ? step.loc.line : null;
  setEditorHighlight(line);
  if (line) revealLine(line);
  // Re-mark the active row without a full rebuild.
  for (const row of outlineEl.children) row.classList.remove("active");
  const row = outlineEl.children[activeStepIdx];
  if (row) row.classList.add("active");
  renderStepper();
  renderDetail();
}

function selectProc(i) {
  activeProcIdx = i;
  // Start each procedure at its postcondition end (the bottom), the natural
  // origin of a backward WLP reading; the user steps up towards the precondition.
  const proc = parsedNow.procedures[i];
  activeStepIdx = Math.max(0, stepsFor(proc).length - 1);
  renderTabs();
  renderOutline();
  renderStepper();
  renderDetail();
  const steps = stepsFor(proc);
  const step = steps[activeStepIdx];
  const line = step && step.loc ? step.loc.line : null;
  setEditorHighlight(line);
  if (line) revealLine(line);
}

stepPrev.onclick = () => selectStep(activeStepIdx - 1);
stepNext.onclick = () => selectStep(activeStepIdx + 1);
stepRange.oninput = () => selectStep(Number(stepRange.value));

// --- Top-level render for a parsed program ---------------------------------
// Called synchronously once the OCaml pipeline returns, before Z3 has solved:
// the outline is shown immediately with obligation verdicts pending; the
// streamed onVerdict callback fills them in as each solve lands.
function renderParsed(parsed, { keepStep } = {}) {
  parsedNow = parsed;
  document.querySelector(".proof").classList.remove("stale");
  detailEl.classList.remove("stale");

  if (!parsed.ok) {
    renderError(parsed);
    return;
  }
  lastGood = parsed;
  // A flat list of every obligation, in the order solveObligations resolves
  // them, so a streamed verdict updates the right object (which the outline and
  // detail share by reference) via its index.
  parsed._flat = [];
  for (const p of parsed.procedures) for (const o of p.obligations) parsed._flat.push(o);

  if (activeProcIdx >= parsed.procedures.length) activeProcIdx = 0;
  const proc = parsed.procedures[activeProcIdx];
  const n = stepsFor(proc).length;
  if (!keepStep || activeStepIdx >= n) activeStepIdx = Math.max(0, n - 1);
  renderTabs();
  renderOutline();
  renderStepper();
  renderDetail();
  const step = stepsFor(proc)[activeStepIdx];
  setEditorHighlight(step && step.loc ? step.loc.line : null);
}

// Jump the outline to the first refuted step, so a failure lands the reader on
// the problem rather than wherever they were reading. Called once per run, when
// the first failing verdict streams in.
function focusFirstFailure() {
  for (let pi = 0; pi < parsedNow.procedures.length; pi++) {
    const si = firstFailingStep(pi);
    if (si >= 0) {
      activeProcIdx = pi;
      activeStepIdx = si;
      renderTabs();
      renderOutline();
      renderStepper();
      renderDetail();
      const step = stepsFor(parsedNow.procedures[pi])[si];
      const line = step && step.loc ? step.loc.line : null;
      setEditorHighlight(line);
      if (line) revealLine(line);
      return;
    }
  }
}

// The final verdict summary, once every obligation has been solved. The total
// time is the sum of the per-obligation Z3 solve times.
function finishVerify(result) {
  const totalMs = (result.obligations || []).reduce((s, o) => s + (o.ms || 0), 0);
  const t = totalMs >= 1000 ? (totalMs / 1000).toFixed(2) + " s" : totalMs + " ms";
  if (result.verified) {
    setSummary("ok", `✓ ${result.total} obligation${result.total === 1 ? "" : "s"} proved · ${t}`);
    setPill("ok", "verified");
  } else {
    setSummary("bad", `✗ ${result.failures.length} of ${result.total} failed · ${t}`);
    setPill("bad", "not verified");
  }
}

// The index of the first step in a procedure carrying a failed obligation, or -1.
function firstFailingStep(procIdx) {
  const steps = stepsFor(parsedNow.procedures[procIdx]);
  for (let i = 0; i < steps.length; i++) {
    if (steps[i].obligations.some((o) => o.verdict && o.verdict !== "unsat")) return i;
  }
  return -1;
}

// A parse/type error: keep the last good outline visible but dimmed, and put the
// message (with a clickable location) at the top of the detail pane.
function renderError(parsed) {
  if (lastGood) {
    parsedNow = lastGood;
    renderTabs();
    renderOutline();
    renderStepper();
    renderDetail();
    document.querySelector(".proof").classList.add("stale");
  } else {
    outlineEl.replaceChildren();
    detailEl.replaceChildren();
  }
  const kind = parsed.kind === "type" ? "Type error" : "Syntax error";
  setSummary("busy", kind);
  setPill("busy", parsed.kind === "type" ? "type error" : "syntax error");

  const banner = document.createElement("div");
  banner.className = "field";
  const h = document.createElement("h2");
  h.textContent = kind;
  const msg = document.createElement("div");
  msg.className = "assertion";
  msg.textContent = parsed.error;
  banner.append(h, msg);
  if (parsed.loc) {
    const loc = document.createElement("span");
    loc.className = "loc";
    loc.textContent = `line ${parsed.loc.line}:${parsed.loc.col}`;
    loc.onclick = () => jumpTo(parsed.loc.line, parsed.loc.col);
    banner.appendChild(loc);
  }
  detailEl.classList.remove("stale");
  detailEl.prepend(banner);
}

// --- Verify loop ------------------------------------------------------------
async function verifyNow() {
  if (!solve) return;
  gen += 1;
  const myGen = gen;
  currentGen = myGen;
  startBusy();
  setSummary("busy", "verifying…");
  let parsed;
  try {
    parsed = JSON.parse(self.cavalryObligations(editor.value));
  } catch (e) {
    if (myGen === currentGen) {
      stopBusy();
      setPill("bad", "error");
      setSummary("bad", "internal error");
      detailEl.replaceChildren();
      const v = document.createElement("div");
      v.className = "placeholder";
      v.textContent = "Internal error: " + ((e && e.message) || e);
      detailEl.appendChild(v);
    }
    return;
  }
  // Paint the outline immediately (verdicts pending) so it is visible while Z3
  // works, keeping the reader's step position across a re-verify.
  renderParsed(parsed, { keepStep: true });
  if (!parsed.ok) {
    stopBusy();
    return;
  }

  // Stream each verdict onto its obligation as it lands, colouring the outline
  // and detail live. [focused] makes the outline jump to the first refuted step
  // exactly once (not on every subsequent failure).
  let focused = false;
  const result = await VerifyCore.solveObligations(parsed, solve, {
    isStale: () => myGen !== currentGen,
    onProgress: (done, total) => {
      if (myGen === currentGen) {
        busyLabel = `verifying… ${done}/${total}`;
        paintBusy();
      }
    },
    onVerdict: (record, index) => {
      if (myGen !== currentGen || !parsedNow._flat) return;
      const o = parsedNow._flat[index];
      if (o) {
        o.verdict = record.verdict;
        o.counterexample = record.counterexample;
        o.ms = record.ms;
      }
      const failed = record.verdict && record.verdict !== "unsat";
      if (failed && !focused) {
        focused = true;
        focusFirstFailure();
      } else {
        renderTabs();
        renderOutline();
        renderDetail();
      }
    },
    renderCounterexample: (id, output, candidate) =>
      self.cavalryRenderCounterexample(id, output, candidate),
  });
  if (myGen !== currentGen || result.stale) return; // superseded
  stopBusy();
  finishVerify(result);
}

let debounceTimer = null;
editor.addEventListener("input", () => {
  refreshEditor(); // highlight + gutter update immediately; verification waits
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(verifyNow, 300);
});

// Paint the proof outline immediately from the synchronous OCaml pipeline --
// it needs no prover -- so the reader sees the structure while Z3's wasm (~32
// MB) is still loading; obligation verdicts fill in once solving runs.
try {
  if (self.cavalryObligations) {
    renderParsed(JSON.parse(self.cavalryObligations(editor.value)), {});
  }
} catch (_) {
  /* leave the pane empty until the first real verify */
}

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
    setSummary("bad", "Z3 failed to load");
    detailEl.replaceChildren();
    const v = document.createElement("div");
    v.className = "placeholder";
    v.textContent = "Failed to load Z3: " + ((e && e.message) || e);
    detailEl.appendChild(v);
  }
})();
