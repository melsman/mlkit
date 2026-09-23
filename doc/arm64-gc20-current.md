# Current ARM64 vs X64: 20 GC benchmarks

Measured September 23, 2026 on the same Apple M2 Max (32 GiB RAM), macOS
26.5.1. Compiler source revision: `36f68fd`. Both target compilers are built
with MLKit `-gc`, and every benchmark is compiled with `-gc`. The compiler
executables themselves are X64-hosted, with the established 1 GiB host stack;
compiler execution is outside the measurements. No MLton is used.

Both GC runtimes are rebuilt from the current source. The complete Basis and
all benchmark programs are compiled into initially absent, separate
`GC20Currentarm64` and `GC20Currentx64` caches. X64 programs run through
Rosetta 2, ARM64 programs natively. The source checkout is restored to its
original Darwin X64 configuration after building the runtime archives.

The 20 programs and original input sizes are the same as the
[earlier milestone-11 comparison](arm64-runtime-performance.md): ten from
`debs-icfp24` at `03d473d`, and ten from `mlkit-bench` at `5bff5c9`.

Each backend/benchmark gets one warmup and five measured executions, with
backend order shuffled deterministically. All compilation finishes before
measurement. Tables report medians of the five samples, not best times.
Elapsed time includes startup and benchmark I/O; all executions use
`-report_gc`. Every one of the 240 executions exits successfully and matches
expected stdout byte for byte.

Memory is the peak resident set size of each separate benchmark process,
reported by macOS `wait4`/`getrusage` as `ru_maxrss` in bytes, converted to MiB
(1 MiB = 1,048,576 bytes). We take the median of five per-run peaks. This
includes resident code, stack, heap and process-associated Rosetta memory;
it is not the ML heap size, allocation volume, virtual address reservation,
or a system-wide accounting of all translation overhead. Time uses Python's
monotonic `perf_counter`; CPU time is also retained in the raw samples.

ARM64 is faster on 19/20 benchmarks and uses less peak RSS on 18/20. Geometric mean ARM64/X64 ratios: 0.819 for time and 0.844 for peak RSS.

A ratio below 1 favours ARM64 in both tables. Small differences should not
be interpreted as precise backend effects; this compares native execution
with the complete X64/Rosetta configuration.

## Execution time

| Benchmark | X64 / Rosetta 2 (s) | ARM64 (s) | ARM / X64 |
|---|---:|---:|---:|
| barnes-hut | 0.8516 | 0.8405 | 0.99 |
| calc | 1.9557 | 1.2055 | 0.62 |
| DLX | 0.3575 | 0.2420 | 0.68 |
| fft | 0.3635 | 0.2874 | 0.79 |
| kbc | 0.7512 | 0.6693 | 0.89 |
| lexgen | 0.4229 | 0.3253 | 0.77 |
| life | 0.8026 | 0.6523 | 0.81 |
| logic | 1.0687 | 0.8045 | 0.75 |
| mandelbrot | 0.3036 | 0.1943 | 0.64 |
| mlyacc | 0.1896 | 0.1463 | 0.77 |
| mpuz | 0.4154 | 0.4082 | 0.98 |
| msort | 0.5392 | 0.4629 | 0.86 |
| nucleic | 0.9795 | 0.6598 | 0.67 |
| patricia | 3.8277 | 3.7613 | 0.98 |
| professor | 0.2451 | 0.2018 | 0.82 |
| ray | 0.3632 | 0.2850 | 0.78 |
| simple | 0.5743 | 0.5026 | 0.88 |
| uf | 0.1342 | 0.1769 | 1.32 |
| vliw | 0.4754 | 0.3255 | 0.68 |
| zebra | 0.7332 | 0.7079 | 0.97 |

## Peak resident memory

| Benchmark | X64 / Rosetta 2 (MiB) | ARM64 (MiB) | ARM / X64 |
|---|---:|---:|---:|
| barnes-hut | 5.5 | 4.3 | 0.78 |
| calc | 86.6 | 66.8 | 0.77 |
| DLX | 27.2 | 74.7 | 2.75 |
| fft | 82.2 | 79.2 | 0.96 |
| kbc | 10.9 | 9.6 | 0.88 |
| lexgen | 14.6 | 13.0 | 0.89 |
| life | 4.4 | 2.9 | 0.67 |
| logic | 5.2 | 3.5 | 0.68 |
| mandelbrot | 4.1 | 2.9 | 0.71 |
| mlyacc | 17.9 | 15.1 | 0.85 |
| mpuz | 4.1 | 2.9 | 0.71 |
| msort | 132.4 | 146.8 | 1.11 |
| nucleic | 6.6 | 4.8 | 0.72 |
| patricia | 6.8 | 5.4 | 0.80 |
| professor | 5.0 | 3.8 | 0.75 |
| ray | 14.4 | 12.3 | 0.85 |
| simple | 7.0 | 5.6 | 0.80 |
| uf | 9.6 | 7.6 | 0.79 |
| vliw | 18.6 | 15.1 | 0.81 |
| zebra | 4.5 | 3.0 | 0.66 |

[Raw samples, build manifest, compiler/runtime/executable hashes and scripts](arm64-performance/gc20-current/)
are retained. `run.py build` creates the isolated source copies and fresh
caches; `run.py measure` checks every output and records per-child resource
usage. Paths refer to this machine's checkouts and require adaptation elsewhere.
The build and runtime logs identify the compilers and archives used. The raw
memory measurement is taken for each child individually, not the cumulative
high-water mark across previous subprocesses.
