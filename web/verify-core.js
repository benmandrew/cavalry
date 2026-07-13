// Solve orchestration, shared by the browser worker (loaded via importScripts)
// and the node end-to-end test (loaded via require). Pure logic: given the
// parsed obligations from the OCaml pipeline and an async Z3 solve function, it
// discharges each obligation and reports the verdict. No DOM, no postMessage.

// Wrap a low-level Z3 instance as [solve(smtlib) -> answer]. A fresh context per
// obligation keeps them independent -- assertions from one never bleed into the
// next -- which is why each obligation's SMT-LIB2 is self-contained.
function makeSolver(Z3) {
  return async function solve(smtlib) {
    const cfg = Z3.mk_config();
    const ctx = Z3.mk_context(cfg);
    try {
      const out = await Z3.eval_smtlib2_string(ctx, smtlib);
      return out.trim();
    } finally {
      Z3.del_context(ctx);
      Z3.del_config(cfg);
    }
  };
}

// Discharge every obligation. [parsed] is the OCaml pipeline's JSON result; a
// parse/type error ([parsed.ok === false]) is passed straight through. Each
// obligation is [unsat] when proved; anything else ([sat]/[unknown]) is a
// failure. [isStale] is polled between obligations so a superseded run (the user
// typed again) bails without finishing -- cancellation without terminating the
// worker. [onProgress] reports (done, total) for the UI.
//
// [renderCounterexample(ceId, output, candidate) -> string] is optional: when
// given, a failing obligation's counterexample twin ([ceSmtlib]) is solved and
// its model rendered to a display block attached as [failure.counterexample].
// [candidate] is false only when Z3 answered [sat] (a confirmed refutation).
async function solveObligations(parsed, solve, opts) {
  opts = opts || {};
  const { onProgress, isStale, renderCounterexample } = opts;
  if (!parsed.ok) return parsed;
  const all = [];
  for (const p of parsed.procedures) {
    for (const o of p.obligations) {
      all.push({
        proc: p.name || "<main>", expl: o.expl, loc: o.loc,
        smtlib: o.smtlib, ceSmtlib: o.ceSmtlib, ceId: o.ceId,
      });
    }
  }
  const total = all.length;
  const failures = [];
  let done = 0;
  for (const ob of all) {
    if (isStale && isStale()) return { ok: true, stale: true };
    const answer = await solve(ob.smtlib);
    if (answer !== "unsat") {
      let counterexample = "";
      if (renderCounterexample && ob.ceSmtlib != null && ob.ceId != null) {
        // The CE twin embeds (get-model), so its output is the answer token
        // followed by the model; [candidate] unless Z3 confirmed with [sat].
        const ceOut = await solve(ob.ceSmtlib);
        counterexample = renderCounterexample(ob.ceId, ceOut, !ceOut.startsWith("sat"));
      }
      failures.push({ proc: ob.proc, expl: ob.expl, loc: ob.loc, verdict: answer, counterexample });
    }
    done += 1;
    if (onProgress) onProgress(done, total);
  }
  return { ok: true, verified: failures.length === 0, failures, total };
}

const VerifyCore = { makeSolver, solveObligations };
if (typeof module !== "undefined" && module.exports) module.exports = VerifyCore;
else if (typeof self !== "undefined") self.VerifyCore = VerifyCore;
