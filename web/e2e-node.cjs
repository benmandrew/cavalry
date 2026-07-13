const path = require('path');
// Built by dune into _build (npm run build:ocaml). Sets globalThis.cavalryObligations.
require(path.join(__dirname, '..', '_build', 'default', 'web', 'verifier.bc.js'));
const { init } = require('z3-solver');                 // node build
const { makeSolver, solveObligations } = require(path.join(__dirname, 'verify-core.js'));

const programs = {
  valid:   `{ x >= 0 }\ny := x + 1\n{ y > x }`,
  invalid: `{ x >= 0 }\ny := x + 1\n{ y > x + 5 }`,
  euclid:  require('fs').readFileSync(path.join(__dirname, '..', 'example.cav'), 'utf8'),
  syntax:  `{ x >= 0 }\ny := x +\n{ y > x }`,
};

const renderCounterexample = (id, output, candidate) =>
  globalThis.cavalryRenderCounterexample(id, output, candidate);

(async () => {
  const { Z3 } = await init();
  const solve = makeSolver(Z3);
  for (const [name, src] of Object.entries(programs)) {
    const parsed = JSON.parse(globalThis.cavalryObligations(src));
    const res = await solveObligations(parsed, solve, { renderCounterexample });
    if (!res.ok) { console.log(`${name.padEnd(8)} -> ${res.kind} error: ${res.error} @ ${JSON.stringify(res.loc)}`); continue; }
    const status = res.verified ? 'VERIFIED' : `FAILED (${res.failures.length}/${res.total})`;
    console.log(`${name.padEnd(8)} -> ${status} [${res.total} obligations]`);
    for (const f of res.failures) {
      console.log(`             x ${f.proc}: ${f.expl} (${f.verdict}) @ ${JSON.stringify(f.loc)}`);
      if (f.counterexample) for (const line of f.counterexample.replace(/\n$/, '').split('\n')) console.log(`             ${line}`);
    }
  }
  process.exit(0);
})();
