// End-to-end browser test: drive the actual page in headless Chrome, exercising
// verify-while-typing. Confirms the whole client-side stack -- OCaml-in-JS,
// Z3-wasm in a worker, debounce/generations, rendering -- works in a browser.
const puppeteer = require("puppeteer-core");
const { spawn } = require("child_process");
const path = require("path");

// Chrome location: CHROME_PATH lets CI point at the runner's browser; the
// default is the macOS install used in local dev.
const CHROME =
  process.env.CHROME_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const PORT = Number(process.env.PORT) || 8099;
const URL = `http://localhost:${PORT}/`;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Start the static server (COOP/COEP) as a child so the test is self-contained;
// returns a handle to kill on exit.
function startServer() {
  const proc = spawn("node", [path.join(__dirname, "serve.cjs"), String(PORT)], {
    stdio: "ignore",
  });
  return proc;
}

async function pill(page) {
  return page.$eval("#status", (el) => el.textContent.trim());
}

// Poll the status pill until it matches, or fail.
async function waitPill(page, re, label, timeout = 60000) {
  const start = Date.now();
  while (Date.now() - start < timeout) {
    const t = await pill(page);
    if (re.test(t)) { console.log(`  ok: ${label} -> "${t}"`); return t; }
    await sleep(150);
  }
  throw new Error(`timeout waiting for ${label}; pill="${await pill(page)}"`);
}

async function setEditor(page, text) {
  await page.$eval("#editor", (el, t) => {
    el.value = t;
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }, text);
}

(async () => {
  const server = startServer();
  await sleep(500); // let it bind
  const browser = await puppeteer.launch({
    executablePath: CHROME,
    headless: "new",
    args: ["--no-sandbox", "--disable-setuid-sandbox"],
  });
  const page = await browser.newPage();
  page.on("console", (m) => { if (m.type() === "error") console.log("  [console.error]", m.text()); });
  page.on("pageerror", (e) => console.log("  [pageerror]", e.message));

  let failed = false;
  try {
    await page.goto(URL, { waitUntil: "load" });
    // crossOriginIsolated must be true or Z3 threads won't start.
    const isolated = await page.evaluate(() => self.crossOriginIsolated);
    console.log(`  crossOriginIsolated = ${isolated}`);

    // 1. The sample (euclidean_div) should verify once Z3 loads.
    await waitPill(page, /^verified$/, "sample verifies");

    // 1b. Editor chrome: the gutter numbers every line, the highlight layer is
    // coloured by the TextMate grammar, and its text matches the textarea
    // exactly (so the caret lines up over the coloured code).
    const chrome = await page.evaluate(() => {
      const ed = document.getElementById("editor");
      const gut = document.getElementById("gutter");
      const hl = document.getElementById("highlight-code");
      const lines = ed.value.split("\n").length;
      return {
        lines,
        gutterLines: gut.textContent.split("\n").length,
        colouredSpans: hl.querySelectorAll('span[style*="color"]').length,
        aligned: hl.textContent === ed.value,
      };
    });
    if (chrome.gutterLines !== chrome.lines)
      throw new Error(`gutter has ${chrome.gutterLines} lines, editor has ${chrome.lines}`);
    if (chrome.colouredSpans < 1) throw new Error("highlight layer has no coloured tokens");
    if (!chrome.aligned) throw new Error("highlight text does not match editor value");
    console.log(`  ok: gutter + highlight (${chrome.lines} lines, ${chrome.colouredSpans} coloured tokens)`);

    // 1c. Example picker: populated from the README snippets, and selecting one
    // loads it into the editor and re-verifies. failing-spec is expected to
    // fail, which also exercises the picker-driven verification path.
    const picker = await page.evaluate(() => {
      const sel = document.getElementById("example-picker");
      return { count: sel.options.length, slugs: [...sel.options].map((o) => o.value) };
    });
    if (picker.count < 2) throw new Error(`example picker has ${picker.count} options`);
    if (!picker.slugs.includes("failing-spec"))
      throw new Error("example picker missing expected snippets: " + picker.slugs.join(", "));
    console.log(`  ok: example picker (${picker.count} examples)`);
    await page.select("#example-picker", "failing-spec");
    await waitPill(page, /not verified/, "picked failing-spec -> not verified");
    await page.select("#example-picker", "hoare-triple");
    await waitPill(page, /^verified$/, "picked hoare-triple -> verified");

    // 2. Verify-while-typing: break the postcondition -> not verified.
    await setEditor(page, `{ x >= 0 }\ny := x + 1\n{ y > x + 5 }`);
    await waitPill(page, /not verified/, "broken postcondition -> not verified");
    const fail = await page.$eval("#results", (el) => el.textContent);
    if (!/postcondition may not hold/.test(fail)) throw new Error("missing failure explanation: " + fail);
    console.log("  ok: failure explanation shown");

    // 3. Fix it -> verified again.
    await setEditor(page, `{ x >= 0 }\ny := x + 1\n{ y > x }`);
    await waitPill(page, /^verified$/, "fixed -> verified");

    // 4. Mid-edit syntax error -> syntax error, last-good kept.
    await setEditor(page, `{ x >= 0 }\ny := x +\n{ y > x }`);
    await waitPill(page, /syntax error/, "incomplete -> syntax error");

    // 5. Type error surfaces distinctly.
    await setEditor(page, `{ x >= 0 }\ny := x && 1\n{ y > x }`);
    await waitPill(page, /type error/, "type mismatch -> type error");

    console.log("\nALL BROWSER CHECKS PASSED");
  } catch (e) {
    failed = true;
    console.log("\nFAILED: " + e.message);
  } finally {
    await browser.close();
    server.kill();
  }
  process.exit(failed ? 1 : 0);
})();
