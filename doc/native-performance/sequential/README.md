# Three-run standard sequential assessment

See [the report](../../sequential-pr253-results.md) for methods, full timing ranges
and interpretation. Each host's `samples.csv` contains ten programs × two
compiler variants × exactly three executions (60 rows). All runs, including cold
first runs, are retained. `run.log` records completed output checks; manifests
retain executable and expected-output hashes, sizes and compiler/runtime identities.

Programs: kitfib35, kitlife35u, kitqsort, kittmergesort, kitkbjul9, kitsimple,
kitmandelbrot, fft, nucleic and ray, using unchanged tracked sources and data.
Flags: `-gc -no_par --no_basislib`, with fresh per-variant Basis/program caches.

Reproduce with `SML_LIB`, `MLKIT_BEFORE`, `MLKIT_AFTER` and run
`src/Compiler/Backend/tests/benchmark-sequential.sh` manually. Its small native
C timer uses wait4 CPU usage and CLOCK_MONOTONIC wall time. X64 `BENCH_CPU`
defaults to 0; ARM64 is unpinned. Do not invoke this script from CI.
