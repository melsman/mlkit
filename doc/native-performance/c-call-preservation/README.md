# C-call preservation assessment

See [the report](../../x64-c-call-preservation-results.md) for methods, results
and limits. `thinkpad/samples.csv` contains all 60 final samples (ten per
variant/workload); `exploratory-concurrent.csv` is an earlier overlapping-load
screen and is not used for the reported comparison. Assembly and size files
come from the final compile/run directory. Correctness logs retain their
original generated output paths. ARM64 logs use the preserved native #251 build.

Reproduce manually with `SML_LIB`, `MLKIT_BEFORE`, `MLKIT_AFTER` set and run
`src/Compiler/Backend/X64/tests/benchmark-c-call-preservation.sh` on Linux X64,
with other compilation/tests finished. `BENCH_CPU` defaults to 0. The script
checks results, alternates execution order and prints its retained output path.
Never add this timing script to CI.
