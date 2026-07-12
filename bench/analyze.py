#!/usr/bin/env python3
"""Analyse hyperfine benchmark JSON into statistics and a Markdown summary.

Reads bench/results/<prog>.json (one per program, each with the raw per-run
sample times for the interp/zarith/native variants) and reports, per program:

  * mean +/- stddev, median and min wall time for each variant;
  * speedup factors (interp/variant) with a 95 % bootstrap confidence interval,
    so the ratio comes with an uncertainty rather than a bare point estimate;
  * a Welch t-test (unequal variances) for interp-vs-native and interp-vs-zarith,
    establishing that the timing differences are statistically significant;
  * a startup-adjusted "compute-only" speedup that subtracts each variant's
    measured fixed startup (from the `trivial` program) before dividing, so the
    interpreter is not unfairly charged for process/parse overhead.

No third-party dependencies: the t-test p-value uses a normal approximation
(valid here -- dozens of runs, and the effect sizes are large) and the ratio CI
uses a nonparametric bootstrap.
"""

import json
import math
import pathlib
import random
import statistics as st

random.seed(1234)
HERE = pathlib.Path(__file__).parent
RESULTS = HERE / "results"
PROGRAMS = ["sum_loop", "collatz", "primes_trial", "sieve", "ackermann"]
VARIANTS = ["interp", "zarith", "native"]

LABELS = {
    "sum_loop": "Arithmetic loop (modular checksum)",
    "collatz": "Branchy loop (Collatz stopping times)",
    "primes_trial": "Nested loops (trial-division primes)",
    "sieve": "Array-heavy (Sieve of Eratosthenes)",
    "ackermann": "Recursion (Ackermann ack(3,8))",
}

# Short name + one-line "shape" shown on the chart rows and table, keyed by program.
CHART = {
    "sum_loop": ("Modular checksum", "Arithmetic loop"),
    "collatz": ("Collatz stopping times", "Branchy loop · div/mod"),
    "primes_trial": ("Trial-division primes", "Nested loops"),
    "sieve": ("Sieve of Eratosthenes", "Array-heavy · O(n) copy per write"),
    "ackermann": ("Ackermann ack(3,8)", "Recursion · call overhead"),
}


def load(prog):
    """Return {variant: [sample times in seconds]} for one program."""
    data = json.loads((RESULTS / f"{prog}.json").read_text())
    return {r["command"]: r["times"] for r in data["results"]}


def welch_t(a, b):
    """Welch's t statistic, degrees of freedom, and two-sided p (normal approx)."""
    ma, mb = st.mean(a), st.mean(b)
    va, vb = st.variance(a), st.variance(b)
    na, nb = len(a), len(b)
    se = math.sqrt(va / na + vb / nb)
    if se == 0:
        return float("inf"), float("nan"), 0.0
    t = (ma - mb) / se
    df = (va / na + vb / nb) ** 2 / (
        (va / na) ** 2 / (na - 1) + (vb / nb) ** 2 / (nb - 1)
    )
    p = math.erfc(abs(t) / math.sqrt(2))  # normal approx to the two-sided tail
    return t, df, p


def bootstrap_ratio_ci(num, den, iters=10000, alpha=0.05):
    """95% CI for mean(num)/mean(den) by resampling each sample with replacement."""
    ratios = []
    for _ in range(iters):
        rn = st.mean(random.choices(num, k=len(num)))
        rd = st.mean(random.choices(den, k=len(den)))
        ratios.append(rn / rd)
    ratios.sort()
    lo = ratios[int((alpha / 2) * iters)]
    hi = ratios[int((1 - alpha / 2) * iters)]
    return lo, hi


def fmt_ms(x):
    return f"{x * 1000:.1f}"


def main():
    startup = load("trivial")
    startup_mean = {v: st.mean(startup[v]) for v in VARIANTS}

    lines = []
    out = lines.append
    out("# Cavalry: interpreter vs compiled benchmark\n")
    meta = (RESULTS / "meta.txt").read_text().strip()
    out("```\n" + meta + "\n```\n")
    out(
        "Fixed startup (empty program, mean): "
        + ", ".join(f"{v} {fmt_ms(startup_mean[v])} ms" for v in VARIANTS)
        + ".\n"
    )

    # Per-program detail + a headline table.
    headline = [
        "| Workload | interp (ms) | zarith (ms) | native (ms) | "
        "native speedup | zarith speedup |",
        "|---|--:|--:|--:|--:|--:|",
    ]

    summary = {"startup_ms": {v: startup_mean[v] * 1000 for v in VARIANTS}, "programs": {}}
    rows = {}  # per-program figures for the HTML report

    for prog in PROGRAMS:
        t = load(prog)
        m = {v: st.mean(t[v]) for v in VARIANTS}
        sd = {v: st.stdev(t[v]) for v in VARIANTS}
        med = {v: st.median(t[v]) for v in VARIANTS}
        mn = {v: min(t[v]) for v in VARIANTS}

        # Speedups vs interpreter, with bootstrap CIs.
        sp_native = m["interp"] / m["native"]
        sp_native_ci = bootstrap_ratio_ci(t["interp"], t["native"])
        sp_zarith = m["interp"] / m["zarith"]
        sp_zarith_ci = bootstrap_ratio_ci(t["interp"], t["zarith"])
        sp_nz = m["zarith"] / m["native"]  # native over zarith

        # Welch tests (significance of interp being slower).
        t_nat, df_nat, p_nat = welch_t(t["interp"], t["native"])
        t_zar, df_zar, p_zar = welch_t(t["interp"], t["zarith"])

        # Startup-adjusted compute-only speedup (subtract fixed startup).
        comp = {v: max(m[v] - startup_mean[v], 1e-9) for v in VARIANTS}
        sp_native_comp = comp["interp"] / comp["native"]

        headline.append(
            f"| {LABELS[prog]} | {fmt_ms(m['interp'])} | {fmt_ms(m['zarith'])} "
            f"| {fmt_ms(m['native'])} | {sp_native:.1f}x | {sp_zarith:.1f}x |"
        )

        out(f"\n## {LABELS[prog]}  (`{prog}`)\n")
        out("| variant | mean ms | stddev ms | median ms | min ms | n |")
        out("|---|--:|--:|--:|--:|--:|")
        for v in VARIANTS:
            out(
                f"| {v} | {fmt_ms(m[v])} | {fmt_ms(sd[v])} | {fmt_ms(med[v])} "
                f"| {fmt_ms(mn[v])} | {len(t[v])} |"
            )
        out("")
        out(
            f"- **native is {sp_native:.1f}x faster than interp** "
            f"(95% CI [{sp_native_ci[0]:.1f}, {sp_native_ci[1]:.1f}]x); "
            f"Welch t={t_nat:.1f}, df={df_nat:.0f}, p={p_nat:.1e}."
        )
        out(
            f"- zarith is {sp_zarith:.1f}x faster than interp "
            f"(95% CI [{sp_zarith_ci[0]:.1f}, {sp_zarith_ci[1]:.1f}]x); "
            f"Welch t={t_zar:.1f}, df={df_zar:.0f}, p={p_zar:.1e}."
        )
        out(f"- native is {sp_nz:.1f}x faster than zarith (the compile default).")
        out(
            f"- startup-adjusted (compute-only) native speedup: "
            f"{sp_native_comp:.1f}x."
        )

        summary["programs"][prog] = {
            "label": LABELS[prog],
            "mean_ms": {v: m[v] * 1000 for v in VARIANTS},
            "stddev_ms": {v: sd[v] * 1000 for v in VARIANTS},
            "median_ms": {v: med[v] * 1000 for v in VARIANTS},
            "min_ms": {v: mn[v] * 1000 for v in VARIANTS},
            "n": {v: len(t[v]) for v in VARIANTS},
            "speedup_native_over_interp": sp_native,
            "speedup_native_over_interp_ci95": list(sp_native_ci),
            "speedup_zarith_over_interp": sp_zarith,
            "speedup_zarith_over_interp_ci95": list(sp_zarith_ci),
            "speedup_native_over_zarith": sp_nz,
            "speedup_native_over_interp_computeonly": sp_native_comp,
            "welch_interp_vs_native": {"t": t_nat, "df": df_nat, "p": p_nat},
            "welch_interp_vs_zarith": {"t": t_zar, "df": df_zar, "p": p_zar},
        }

        name, kind = CHART[prog]
        rows[prog] = {
            "key": prog, "name": name, "kind": kind,
            "interp": round(m["interp"] * 1000, 1), "sdI": round(sd["interp"] * 1000, 1),
            "zarith": round(m["zarith"] * 1000, 1), "sdZ": round(sd["zarith"] * 1000, 1),
            "native": round(m["native"] * 1000, 1), "sdN": round(sd["native"] * 1000, 1),
            "spN": round(sp_native, 1), "spNlo": round(sp_native_ci[0], 1),
            "spNhi": round(sp_native_ci[1], 1), "spZ": round(sp_zarith, 1),
            "t": round(t_nat, 1),
        }

    body = "\n".join(["# Headline\n", *headline, "", *lines[1:]])
    (RESULTS / "summary.md").write_text(body + "\n")
    (RESULTS / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    render_report(rows, startup_mean)
    print(body)
    print(
        f"\nWrote {RESULTS/'summary.md'}, {RESULTS/'summary.json'}, "
        f"{RESULTS/'report.html'} and {RESULTS/'report_local.html'}"
    )


def parse_meta():
    """meta.txt (key: value per line) -> dict; tolerant of a missing file."""
    d = {}
    p = RESULTS / "meta.txt"
    if p.exists():
        for line in p.read_text().splitlines():
            if ":" in line:
                k, _, v = line.partition(":")
                d[k.strip()] = v.strip()
    return d


def render_report(rows, startup_mean):
    """Fill bench/report_template.html with this run's figures and write the
    body-only report.html (for publishing as an Artifact) and the standalone
    report_local.html (open locally in a browser)."""
    tmpl_path = HERE / "report_template.html"
    if not tmpl_path.exists():
        return  # template optional; summary.* are the canonical machine output
    ordered = sorted(rows.values(), key=lambda r: r["spN"], reverse=True)
    sp = {r["key"]: r["spN"] for r in rows.values()}
    arith = [sp[k] for k in ("sum_loop", "collatz", "primes_trial") if k in sp]
    nz = [r["zarith"] / r["native"] for r in rows.values() if r["native"] > 0]
    ns = [len(load(k)["native"]) for k in rows] + [len(load(k)["zarith"]) for k in rows]
    ts = [r["t"] for r in rows.values()]
    meta = parse_meta()

    def rng(lo, hi, dec=0):
        f = f"{{:.{dec}f}}"
        a, b = f.format(lo), f.format(hi)
        return f"{a}&ndash;{b}" if a != b else a

    chips = []
    cpu, cores = meta.get("cpu", "unknown"), meta.get("cores", "")
    chips.append(f"{cpu}" + (f" · {cores} cores" if cores and cores != "unknown" else ""))
    if meta.get("os"):
        chips.append(meta["os"])
    if meta.get("hyperfine"):
        chips.append(meta["hyperfine"])
    chips.append(f"{meta.get('warmup','3')} warmups · {meta.get('min_runs','20')}+ timed runs each")
    if meta.get("date"):
        chips.append(meta["date"][:10])
    meta_html = "".join(f'<span class="chip">{c}</span>' for c in chips)

    n_interp = len(load(next(iter(rows)))["interp"])
    repl = {
        "@@DATA@@": json.dumps(ordered),
        "@@RANGE@@": f"{round(min(sp.values()))}&times; to {round(max(sp.values()))}&times;",
        "@@SPMAX@@": str(round(max(sp.values()))),
        "@@SPMIN@@": str(round(min(sp.values()))),
        "@@STARTUP_INTERP@@": fmt_ms(startup_mean["interp"]),
        "@@STARTUP_ZARITH@@": fmt_ms(startup_mean["zarith"]),
        "@@STARTUP_NATIVE@@": fmt_ms(startup_mean["native"]),
        "@@NRUNS@@": str(n_interp),
        "@@RUNRANGE@@": f"{min(ns)}&ndash;{max(ns)}",
        "@@TRANGE@@": rng(min(ts), max(ts)),
        "@@SP_ARITH@@": rng(min(arith), max(arith)) + "&times;",
        "@@SP_RECUR@@": str(round(sp.get("ackermann", 0))),
        "@@SP_ARRAY@@": str(round(sp.get("sieve", 0))),
        "@@NZ_RANGE@@": rng(min(nz), max(nz), 1) + "&times;",
        "@@META@@": meta_html,
    }
    body = tmpl_path.read_text()
    for k, v in repl.items():
        body = body.replace(k, v)
    (RESULTS / "report.html").write_text(body)

    # Standalone: the body-only report begins with its own <title> line; lift it
    # into a real <head> and wrap in a full document.
    lines = body.split("\n")
    if lines and lines[0].startswith("<title>"):
        lines = lines[1:]
    standalone = (
        "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        "<title>Cavalry — interpreter vs compiled benchmark</title>\n"
        "<style>html,body{margin:0}</style>\n</head>\n<body>\n"
        + "\n".join(lines)
        + "\n</body>\n</html>\n"
    )
    (RESULTS / "report_local.html").write_text(standalone)


if __name__ == "__main__":
    main()
