# GC ML return convention: implementation and runtime results

GC-mode ML epilogues now use `br x30`, matching their explicit-continuation
`b`/`br` calls. Non-GC ML epilogues keep `ret`, paired with `bl`/`blr`.
Native runtime helpers and C bridge returns also retain `ret`.

Exported callbacks now enter GC-mode ML code with an explicit continuation
and `br x17`, preserving the native call/return pairing around the bridge.
Collection remains deferred across callbacks, so this bridge needs no inline
GC descriptor. Unit initialisation already uses the correct ML call helper.
Parallel thread entry remains `blr`/`ret`: GC with parallelism is rejected by
the current backend. Exception transfers and native helper calls are unchanged.

This implements the finding from the
[mlyacc return-prediction investigation](arm64-mlyacc-return-prediction.md).
There is no inlining, register allocation, instruction scheduling or runtime
change in this implementation.

## Recompiled Basis comparison

All three variants compile the entire Basis and benchmark sources afresh:
ARM64 before, ARM64 after, and X64. They use separate initially absent caches,
`M12RetBefore`, `M12RetAfter` and `M12RetX64`. The X64 compiler is also rebuilt
from the current source. All compiler builds use MLKit with `-gc`, the X64
host linker, and a 1 GiB stack; MLton is not used. The unchanged ARM64 baseline
compiler is `fbbd01b` (later commits through `78dfa6c` only change CI/docs).
Both ARM variants link the same runtime archive.

Measurements are GC-enabled on the same M2 Max, with X64 under Rosetta 2.
Each variant has one warmup and five measured runs, in deterministic shuffled
order. All compilation and tests finish before timing. Values below are median
elapsed seconds; percentages describe reductions in elapsed time.

| Benchmark | X64 / Rosetta 2 | ARM64 before | ARM64 after | ARM64 reduction | ARM64 after vs X64 |
| --- | ---: | ---: | ---: | ---: | ---: |
| nucleic | 0.9663 | 0.7240 | 0.6441 | 11.0% | 33.3% faster |
| mlyacc | 0.1903 | 0.2403 | 0.1468 | 38.9% | 22.8% faster |
| professor | 0.2333 | 0.2485 | 0.1911 | 23.1% | 18.1% faster |

These are contemporaneous measurements, including fresh library objects for
every variant; use these baselines rather than combining the results with
older timing sessions. The implementation affects Basis returns too, whereas
the preceding mlyacc diagnostic changed only benchmark-unit assembly.

All 54 warmup/measured executions match expected stdout. Collection counts
remain 3,106 for nucleic, 28 for mlyacc and 263 for professor across all variants.
CPU medians show the same improvement. The small five-run comparison establishes
the effect on these inputs, not a broad benchmark survey.

## Validation

- Native MLKit/ReML suite passes: no-GC, GC, generational GC, forced collection,
  profiling, callbacks, exception unwinding, closures, stack results, large
  frames, REPL and inline-descriptor/root checks.
- Parallel suite passes; optional Argobots remains unconfigured and is skipped.
- Emitter regression checks distinguish GC `br x30` from non-GC `ret`, including
  extra-GC-check and profiling configurations.
- A separate GC callback build passes its output check. Inspection verifies
  explicit continuation materialisation and `br x17` into ML, no `blr x17`,
  and a final native `ret` from the C bridge.
- Inspection of freshly generated benchmark/Basis assembly finds 8,162 ML
  `ret` sites before and 8,162 `br x30` sites after, with no remaining `ret`
  in those GC ML function bodies. Native helper returns are excluded.

[Raw measurements, executable/compiler/runtime hashes, scripts and suite logs](arm64-performance/return-fix/)
are retained. Scripts record local input/output paths and require adaptation
for another checkout. `compile.py` rejects already-existing Basis caches;
`measure.py` checks every output and records wall time, CPU time and GC data.
