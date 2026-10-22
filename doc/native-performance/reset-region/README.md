# Native shared-reset assessment

See [single-page-reset-results.md](../../single-page-reset-results.md) for the
implementation, methods, results and limits.

- `helper-samples.csv`: four runtime modes × four scenarios × five paired runs
  (160 rows per host), identical native driver/runtime around extracted helpers.
- `loop-final-samples.csv`: three generated ReML modes × five paired runs
  (30 rows per host), 200 million resets per executable.
- ARM64 `helper-only-long.csv` records the rejected helper-only placement;
  `helper-only-short.csv` is the earlier 20-million-reset screen. The final
  implementation retains ARM64's original sequential site fast path.
- ThinkPad `loop-short-samples.csv` is its earlier 20-million-reset screen.
- Helper/caller assembly, executable sizes and validation logs accompany samples.
  Active-profile summaries count decoded records; binary profiles are not included.

Reproduce manually with `SML_LIB`, `MLKIT_BEFORE`, `MLKIT_AFTER` and, for generated
ML, `REML_BEFORE`/`REML_AFTER`. Run `benchmark-reset-region.sh` and
`benchmark-reset-loop.sh` from `src/Compiler/Backend/tests/`. Finish correctness
builds before timing. X64 `BENCH_CPU` defaults to 0; ARM64 is not CPU-pinned.
The helper script supports `RESET_MODES` (default: no_gc par prof gengc).
Both scripts retain output directories and check all results. Never run them in CI.

ARM64 baseline compilers are the preserved native #251 build; the retained
compiler includes #253. X64 baseline compilers include #253 milestones 1–5.
Both sides of every comparison link the same runtime from the assessment tree.
Source/seed/runtime differences outside reset generation do not enter the
helper-body comparison. The ARM64 sequential site's instructions are unchanged
apart from its cold fallback target, so its timing variation is not evidence of
a faster reset algorithm.
