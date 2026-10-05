"""Measure reading ``Program`` body instructions from Python.

- baseline: ``program.instructions`` (clones the whole body into a new list on every access,
  which is what ``body_instructions`` used to do)
- new:      ``program.body_instructions`` (a view over the body)

Build first:  maturin develop --release --features stubs
Run:          python benches/body_instructions.py [--quick] [--json out.json]
"""

import argparse
import ctypes
import gc
import json
import os
import platform
import statistics
import subprocess
import sys
import time

GATE = "RX(pi/2) {q}"
PULSE = (
    'PULSE {q} "rf" drag_gaussian(duration: 6.000000000000001e-08, fwhm: 1.5000000000000002e-08, '
    "t0: 3.0000000000000004e-08, anh: -190000000.0, alpha: -1.6453719598238201, "
    "scale: 0.168265925924524, phase: 0.0, detuning: 0)"
)
CORPORA = {"gate": GATE, "pulse": PULSE}
IMPLS = {
    "baseline": lambda p: p.instructions,
    "new": lambda p: p.body_instructions,
}


def make_program(corpus: str, n: int):
    from quil._quil.program import Program

    line = CORPORA[corpus]
    return Program.parse("\n".join(line.format(q=i % 8) for i in range(n)))


# --------------------------------------------------------------------------- timing


def measure(setup, op, repeatable=False, min_time=0.2, min_repeats=5, max_repeats=200):
    """Median seconds of ``op(state)``, with a fresh untimed ``setup()`` before each run.

    A ``repeatable`` op leaves state unchanged, so it runs many times per sample on one setup.
    """
    if repeatable:
        state = setup()
        t0 = time.perf_counter()
        op(state)
        inner = max(1, int(1e-3 / max(time.perf_counter() - t0, 1e-9)))
        samples = []
        for _ in range(max(min_repeats, 7)):
            gc.disable()
            t0 = time.perf_counter_ns()
            for _ in range(inner):
                op(state)
            t1 = time.perf_counter_ns()
            gc.enable()
            samples.append((t1 - t0) / 1e9 / inner)
        return statistics.median(samples)

    times = []
    start = time.perf_counter()
    while len(times) < max_repeats and (
        len(times) < min_repeats or time.perf_counter() - start < min_time
    ):
        state = setup()
        gc.disable()
        t0 = time.perf_counter_ns()
        op(state)
        t1 = time.perf_counter_ns()
        gc.enable()
        times.append((t1 - t0) / 1e9)
    return statistics.median(times)


def full_pass(view):
    for _ in view:
        pass


def warmed(corpus, n, get):
    """A fresh program whose cache (if any) has been filled by one full pass."""
    p = make_program(corpus, n)
    full_pass(get(p))
    return p


def timing_scenarios(corpus, n, get):
    from quil._quil.instructions import Gate, Qubit

    mid = n // 2
    appended = Gate("Z", [], [Qubit(0)], [])
    appends = 50
    return {
        "T1 cold v[i]": (lambda: make_program(corpus, n), lambda p: get(p)[mid], 1),
        "T1 warm v[i]": (lambda: warmed(corpus, n, get), lambda p: get(p)[mid], 1, True),
        "T2 1 full pass": (lambda: make_program(corpus, n), lambda p: full_pass(get(p)), n),
        "T3 10 full passes": (
            lambda: make_program(corpus, n),
            lambda p: [full_pass(get(p)) for _ in range(10)],
            10 * n,
        ),
        "T7 list(v) + 10 passes": (
            lambda: make_program(corpus, n),
            lambda p: [full_pass(snapshot) for snapshot in [list(get(p))] for _ in range(10)],
            10 * n,
        ),
        "T4 append + v[-1]": (
            lambda: warmed(corpus, n, get),
            lambda p: [(p.add_instruction(appended), get(p)[-1]) for _ in range(appends)],
            appends,
        ),
        "T5 invalidate + pass": (
            lambda: warmed(corpus, n, get),
            lambda p: (p.resolve_placeholders(), full_pass(get(p))),
            n,
        ),
        "T6 len(v)": (lambda: make_program(corpus, n), lambda p: len(get(p)), 1, True),
    }


def run_timing(sizes):
    rows = []
    for corpus in CORPORA:
        for n in sizes:
            for impl, get in IMPLS.items():
                for scenario, (setup, op, units, *repeatable) in timing_scenarios(corpus, n, get).items():
                    seconds = measure(setup, op, repeatable=bool(repeatable))
                    rows.append(
                        dict(corpus=corpus, n=n, impl=impl, scenario=scenario,
                             seconds=seconds, per_unit=seconds / units, units=units)
                    )
                    print(f"  {corpus:5} n={n:<7} {impl:8} {scenario:22} {seconds * 1e3:10.3f} ms",
                          file=sys.stderr)
    # The resolve_placeholders() cost inside T5 is the same for all impls; measure it alone.
    for corpus in CORPORA:
        for n in sizes:
            seconds = measure(lambda: make_program(corpus, n), lambda p: p.resolve_placeholders())
            rows.append(dict(corpus=corpus, n=n, impl="-", scenario="resolve_placeholders alone",
                             seconds=seconds, per_unit=seconds / n, units=n))
    return rows


# --------------------------------------------------------------------------- memory


class MallocStats(ctypes.Structure):
    _fields_ = [
        ("blocks_in_use", ctypes.c_uint),
        ("size_in_use", ctypes.c_size_t),
        ("max_size_in_use", ctypes.c_size_t),
        ("size_allocated", ctypes.c_size_t),
    ]


_libc = ctypes.CDLL(None)
_libc.malloc_zone_statistics.argtypes = [ctypes.c_void_p, ctypes.POINTER(MallocStats)]


def snapshot():
    """(total heap bytes across all malloc zones, RSS bytes).

    No tracemalloc: its per-allocation records live in the same malloc zones
    and would add ~74 B to every Python object counted here.
    """
    import psutil

    gc.collect()
    stats = MallocStats()
    _libc.malloc_zone_statistics(None, ctypes.byref(stats))
    return stats.size_in_use, psutil.Process().memory_info().rss


def memory_child(corpus, n, impl, scenario):
    """Runs in a subprocess with PYTHONMALLOC=malloc, so the malloc zones see every allocation."""
    import quil._quil.program  # noqa: F401  (keep import allocations out of the program size)

    get = IMPLS[impl]
    before_parse = snapshot()
    p = make_program(corpus, n)
    base = snapshot()
    held = None

    if scenario == "M2 retained after 1 pass":
        full_pass(get(p))
        after = snapshot()
    elif scenario == "M3 live at end of 1 pass":
        view = get(p)
        last = len(view) - 1
        after = None
        for i, x in enumerate(view):
            if i == last:
                after = snapshot()  # the peak for all three: every allocation so far is still live
    elif scenario == "M4 holding list(v)":
        held = list(get(p))
        after = snapshot()
    else:
        raise ValueError(scenario)

    del held
    return dict(
        program_heap=base[0] - before_parse[0],
        heap=after[0] - base[0],
        rss=after[1] - base[1],
    )


MEMORY_SCENARIOS = ["M2 retained after 1 pass", "M3 live at end of 1 pass", "M4 holding list(v)"]


def run_memory(sizes):
    rows = []
    env = dict(os.environ, PYTHONMALLOC="malloc")
    for corpus in CORPORA:
        for n in sizes:
            for impl in IMPLS:
                for scenario in MEMORY_SCENARIOS:
                    out = subprocess.run(
                        [sys.executable, __file__, "--memory-child", corpus, str(n), impl, scenario],
                        env=env, capture_output=True, text=True, check=True,
                    ).stdout
                    result = json.loads(out)
                    rows.append(dict(corpus=corpus, n=n, impl=impl, scenario=scenario, **result))
                    print(f"  {corpus:5} n={n:<7} {impl:8} {scenario:26} "
                          f"heap {result['heap'] / 1e6:9.2f} MB",
                          file=sys.stderr)
    return rows


# --------------------------------------------------------------------------- report


def fmt_time(s):
    return f"{s * 1e9:.0f} ns" if s < 1e-6 else f"{s * 1e6:.1f} µs" if s < 1e-3 else f"{s * 1e3:.1f} ms"


def print_report(timing, memory):
    print("## Timing (median)\n")
    for corpus in CORPORA:
        print(f"### {corpus}\n")
        print("| scenario | n | " + " | ".join(IMPLS) + " |")
        print("|---|---:|" + "---:|" * len(IMPLS))
        keys = sorted({(r["scenario"], r["n"]) for r in timing if r["corpus"] == corpus and r["impl"] != "-"})
        for scenario, n in keys:
            cell = {r["impl"]: r["seconds"] for r in timing
                    if (r["corpus"], r["scenario"], r["n"]) == (corpus, scenario, n)}
            print(f"| {scenario} | {n:,} | " + " | ".join(fmt_time(cell[i]) for i in IMPLS) + " |")
        for r in timing:
            if r["corpus"] == corpus and r["impl"] == "-":
                print(f"| ({r['scenario']}) | {r['n']:,} | {fmt_time(r['seconds'])} | | |")
        print()

    print("## Memory (bytes above the loaded program; per-instruction in parentheses)\n")
    for corpus in CORPORA:
        print(f"### {corpus}\n")
        print("| scenario | n | " + " | ".join(IMPLS) + " |")
        print("|---|---:|" + "---:|" * len(IMPLS))
        for scenario in MEMORY_SCENARIOS:
            for n in sorted({r["n"] for r in memory}):
                cell = {r["impl"]: r["heap"] for r in memory
                        if (r["corpus"], r["scenario"], r["n"]) == (corpus, scenario, n)}
                print(f"| {scenario} | {n:,} | "
                      + " | ".join(f"{cell[i] / 1e6:.2f} MB ({cell[i] / n:.0f} B)" for i in IMPLS) + " |")
        sizes = {r["n"]: r["program_heap"] for r in memory if r["corpus"] == corpus}
        print("| (program itself) | | " + ", ".join(f"n={n:,}: {b / 1e6:.2f} MB ({b / n:.0f} B)"
                                                   for n, b in sorted(sizes.items())) + " | | |")
        print()


def main():
    if len(sys.argv) > 1 and sys.argv[1] == "--memory-child":
        corpus, n, impl, scenario = sys.argv[2], int(sys.argv[3]), sys.argv[4], sys.argv[5]
        print(json.dumps(memory_child(corpus, n, impl, scenario)))
        return

    parser = argparse.ArgumentParser()
    parser.add_argument("--quick", action="store_true", help="only n=1,000 and 10,000")
    parser.add_argument("--json", help="write raw results here")
    parser.add_argument("--memory-only", action="store_true",
                        help="rerun only the memory cases, keeping timing from an existing --json file")
    args = parser.parse_args()
    sizes = [1_000, 10_000] if args.quick else [1_000, 10_000, 100_000]

    print(f"# Python {platform.python_version()} on {platform.machine()} {platform.mac_ver()[0]}\n")
    if args.memory_only:
        with open(args.json) as f:
            timing = json.load(f)["timing"]
    else:
        print("timing...", file=sys.stderr)
        timing = run_timing(sizes)
    print("memory...", file=sys.stderr)
    memory = run_memory(sizes)
    print_report(timing, memory)
    if args.json:
        with open(args.json, "w") as f:
            json.dump(dict(timing=timing, memory=memory), f, indent=1)


if __name__ == "__main__":
    main()
