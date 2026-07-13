// Minimal static server for the client-side verifier. Two things matter beyond
// serving files: the cross-origin-isolation headers (COOP/COEP) that Z3's
// threaded wasm needs for SharedArrayBuffer, and a correct application/wasm MIME
// type so the browser streams-compiles the 32 MB module.
const http = require("http");
const fs = require("fs");
const path = require("path");

const root = __dirname;
const port = Number(process.argv[2]) || 8099;

const types = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".cjs": "text/javascript; charset=utf-8",
  ".wasm": "application/wasm",
  ".svg": "image/svg+xml",
  ".json": "application/json; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".map": "application/json; charset=utf-8",
};

// Build artifacts live under dune's _build, not the source tree.
const built = {
  "/verifier.bc.js": path.join(root, "..", "_build", "default", "web", "verifier.bc.js"),
};

const server = http.createServer((req, res) => {
  let rel = decodeURIComponent(req.url.split("?")[0]);
  if (rel === "/") rel = "/index.html";
  const file = built[rel] ? path.normalize(built[rel]) : path.normalize(path.join(root, rel));
  const allowed = root + path.sep;
  const buildRoot = path.normalize(path.join(root, "..", "_build")) + path.sep;
  if (!file.startsWith(allowed) && !file.startsWith(buildRoot)) { res.writeHead(403).end("forbidden"); return; }
  fs.readFile(file, (err, data) => {
    if (err) { res.writeHead(404).end("not found: " + rel); return; }
    res.writeHead(200, {
      "Content-Type": types[path.extname(file)] || "application/octet-stream",
      // Cross-origin isolation: required for SharedArrayBuffer (Z3 pthreads).
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Resource-Policy": "same-origin",
      "Cache-Control": "no-cache",
    });
    res.end(data);
  });
});

server.listen(port, () => console.log(`serving ${root} on http://localhost:${port}`));
