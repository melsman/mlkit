# Native X64 milestone-8 measurements

Collected 2026-10-22 on the ThinkPad; see `host.txt` for hardware/tool versions.
Baseline: `ac46a18e`; candidate: `03e40cf2`. Both compilers were built with
installed MLKit v4.7.15, in isolated source snapshots with clean compiler caches.
All measured programs link the candidate tree's same runtime archive:
`f3d132595619b860330804a97491098c910816dd4099f2abacbe21dd0fc6cd82` (SHA-256).
No later no-spawn policy changes are included in this comparison.

AC power; balanced profile; powersave governor with balance_performance preference.
Local loops and protected diagnostics use `taskset -c 0`. Four-worker helper and
parallel-program tests use `taskset -c 0,2,4,6`: distinct performance cores, avoiding
SMT siblings and efficiency cores. No unrelated compiler build runs during timing.
Each comparison has nine paired runs, alternating order and verifying output.
Times from `/usr/bin/time -p` have hundredth-second resolution. The helper probe
uses CLOCK_MONOTONIC and includes region creation, filling/verification, joining
and deallocation; it does not measure CAS alone.

CSV files retain every sample:

- `local.csv`: MLKit `-no_gc -par`.
- `forced.csv`: same, plus `--alloc_protect_always`.
- `reml-protected.csv`: ReML `-par -no_par0`, using inference (the loop is local).
- `reml-default.csv`: ReML `-par -par0`.
- `runtime.csv`: MLKit `-no_gc -par --alloc_protect_always --no_inline_allocation`.
- `helpers.csv`: compatible helper entry, single/shared/mixed/expansion/private.
- `programs.csv`: merge sort and allocation/computation; the shared program script
  reduces the latter's computation rounds from 100 to 10, as in the ARM64 comparison.
- `protected-diagnosis.csv`: diagnostic variants described below.

`*-size.txt` reports GNU `size` aggregates and per-unit text. The section reports
also give executable `.text` independently of read-only data/unwind tables. Paths
inside these reports identify the retained temporary build artifacts.

## Protected diagnostic variants

Reproduce with `SML_LIB` pointing to the common runtime tree and
`FORCED_BENCHMARK` pointing to the directory retained by the forced local-loop
benchmark, then run `taskset -c 0 sh diagnose-protected.sh`. This is a manual
experiment for that bounded, two-word protected fixture only; its transformations
are not production code or a correctness generalization.

- `baseline` and `fast`: the original measured executables.
- `unchecked`: new unit/helper, with only the protected helper's size guard removed.
- `stack`: new unit with the old protected helper exposed at the new entry name.
- `direct`: new helper with the site's dispatch/local path replaced by a direct call.

These distinguish helper implementation from dispatch/size-check overhead, but
also change code layout. They do not isolate FP-transfer latency from pointer
reconstruction or instruction placement.

Reproduction entry points are documented in `../../../native-inline-allocation-results.md`.
All benchmarks remain manual; no timing collection is added to CI.
