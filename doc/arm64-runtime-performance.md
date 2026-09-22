# ARM64 and X64 runtime performance

For the latest GC results after milestone 12, including memory usage, see
[the September 23 comparison](arm64-gc20-current.md). The measurements below
are the historical milestone-11 baseline.

Milestone 11 of issue #223 / PR #224 compares 20 benchmarks with GC (`-gc`)
and without GC (`-no_gc`). Region inference stays enabled in both modes.
X64 executables run under Rosetta 2; ARM64 executables run natively on the
same Apple M2 Max with 32 GiB RAM and macOS 26.5.1 (25F80). Measurements
were collected on September 21, 2026.

Across these workloads, native ARM64 takes **2.01× the X64/Rosetta time with
GC** and **1.52× without GC**, using the geometric mean of per-benchmark
median-time ratios. With GC, X64 is faster on all 20 programs. Without GC,
nucleic is about 19% faster on ARM64; patricia and msort are close to parity.
The largest GC gaps are logic (4.66×), barnes-hut (3.29×), and life (2.96×).
These measurements establish the differences; they do not identify their causes.

## Benchmark selection

Ten programs come from the published artifact for
[Double-Ended Bit-Stealing for Algebraic Data Types (ICFP 2024)](https://elsman.com/mlkit/pdf/icfp24main-p22-final.pdf):
calc, DLX, kbc, lexgen, logic, nucleic, patricia, ray, uf, and vliw.
The artifact is [debs-icfp24 at 03d473d](https://github.com/melsman/debs-icfp24/tree/03d473dcd7042183fffcf727e85eed6bbacf7f73/src).

The other ten are mlyacc, professor, barnes-hut, fft, life, mandelbrot, mpuz,
msort, simple, and zebra from
[mlkit-bench at 5bff5c9](https://github.com/melsman/mlkit-bench/tree/5bff5c9c925ac6fc43b1a98bf506edc40c8019df/benchmarks).
This includes workloads from the
[region inference / generational GC paper](https://elsman.com/pdf/gengc-techreport.pdf).
Original benchmark inputs and loop counts are retained. In particular,
nucleic uses the paper artifact's workload rather than the short compiler
regression test. No compiler other than MLKit is used in this comparison.

## Measurement

Both backends were built with MLKit `-gc` from the milestone 10 implementation
(`e248ccc`; checkout `8b4ec13` only changes CI seed URLs and documentation).
The compiler host architecture is ARM64 for both builds; the measured X64
programs are x86_64 binaries. Compilation is excluded from execution timings.
Architecture-specific Basis caches and runtime libraries are used. The X64
assembler and linker explicitly select `x86_64`; linking uses Apple clang 21.0.0.

Each configuration gets one warm-up and five measured executions in separate
processes. Configuration order reverses on alternate rounds. Tables report
median elapsed seconds including process startup and benchmark I/O; all samples
are retained in the raw data. Every execution must exit successfully and match
the benchmark's expected output byte for byte. This is a comparison of native
ARM64 against X64 **including Rosetta 2**, not an isolated measurement of ISA
or backend quality. Small differences should not be overinterpreted.

## Results

An ARM64/X64 ratio below 1 means ARM64 is faster. Times are seconds.

### With GC

| Benchmark | X64 / Rosetta | ARM64 native | ARM64 / X64 |
|---|---:|---:|---:|
| barnes-hut | 0.9342 | 3.0699 | 3.29 |
| calc | 2.5307 | 5.5824 | 2.21 |
| DLX | 0.3955 | 0.5119 | 1.29 |
| fft | 0.4406 | 0.7912 | 1.80 |
| kbc | 0.7858 | 1.1610 | 1.48 |
| lexgen | 0.4674 | 0.8698 | 1.86 |
| life | 0.7710 | 2.2785 | 2.96 |
| logic | 1.1131 | 5.1893 | 4.66 |
| mandelbrot | 0.3185 | 0.5420 | 1.70 |
| mlyacc | 0.2111 | 0.3990 | 1.89 |
| mpuz | 0.4273 | 0.7490 | 1.75 |
| msort | 0.5718 | 1.5129 | 2.65 |
| nucleic | 0.9860 | 1.6451 | 1.67 |
| patricia | 4.0043 | 10.7735 | 2.69 |
| professor | 0.2611 | 0.4711 | 1.80 |
| ray | 0.3913 | 0.5537 | 1.42 |
| simple | 0.6187 | 1.2497 | 2.02 |
| uf | 0.1406 | 0.2429 | 1.73 |
| vliw | 0.5217 | 0.8585 | 1.65 |
| zebra | 0.7865 | 1.4781 | 1.88 |

Geometric mean ARM64/X64 ratio: **2.01**.
ARM64 is faster on **0/20** benchmarks.

### Without GC

| Benchmark | X64 / Rosetta | ARM64 native | ARM64 / X64 |
|---|---:|---:|---:|
| barnes-hut | 1.5425 | 2.4734 | 1.60 |
| calc | 1.1723 | 2.2846 | 1.95 |
| DLX | 0.2376 | 0.3657 | 1.54 |
| fft | 0.3320 | 0.4477 | 1.35 |
| kbc | 0.6346 | 1.0526 | 1.66 |
| lexgen | 0.3988 | 0.5972 | 1.50 |
| life | 0.4985 | 0.7947 | 1.59 |
| logic | 0.7348 | 0.7909 | 1.08 |
| mandelbrot | 0.3177 | 0.4954 | 1.56 |
| mlyacc | 0.2127 | 0.2720 | 1.28 |
| mpuz | 0.2776 | 0.6692 | 2.41 |
| msort | 0.3883 | 0.4057 | 1.04 |
| nucleic | 1.0474 | 0.8457 | 0.81 |
| patricia | 2.2926 | 2.3094 | 1.01 |
| professor | 0.2142 | 0.4130 | 1.93 |
| ray | 0.3543 | 0.4868 | 1.37 |
| simple | 0.4572 | 1.2021 | 2.63 |
| uf | 0.1107 | 0.1717 | 1.55 |
| vliw | 0.4774 | 0.7578 | 1.59 |
| zebra | 0.5807 | 1.2669 | 2.18 |

Geometric mean ARM64/X64 ratio: **1.52**.
ARM64 is faster on **1/20** benchmarks.

All 80 benchmark/configuration combinations pass their expected-output checks
on the warm-up and all five measured runs (480 successful executions).
[Raw samples, hashes, and commands](arm64-performance/runtime/measurements.json)
include the minimum and maximum elapsed times as well as the medians.

## Reproduction

Use a space-free source path (a symlink is sufficient) and MLKit compilers for
the two backends. Build the host compilers with `-gc`; their entry points are
`src/Compiler/mlkitarm64.mlb` and `src/Compiler/mlkit64.mlb`. Build the matching
Darwin runtime libraries as described in [the backend guide](arm64-compiler.md).

```sh
git clone https://github.com/melsman/mlkit-bench.git /tmp/m11-bench
git -C /tmp/m11-bench checkout 5bff5c9c925ac6fc43b1a98bf506edc40c8019df
git clone --filter=blob:none --sparse https://github.com/melsman/debs-icfp24.git /tmp/m11-artifact
git -C /tmp/m11-artifact sparse-checkout set src
git -C /tmp/m11-artifact checkout 03d473dcd7042183fffcf727e85eed6bbacf7f73
make -C /tmp/m11-artifact/src/calc prepare \
  MLLEX=/path/to/mlkit-mllex MLYACC=/path/to/mlkit-mlyacc
python3 doc/arm64-performance/runtime/measure.py \
  --source /tmp/mlkit-source \
  --bench /tmp/m11-bench --bitstealing /tmp/m11-artifact \
  --arm64 /path/to/mlkit-arm64 --x64 /path/to/mlkit-x64-backend \
  --output /tmp/m11-results
```

The [measurement script](arm64-performance/runtime/measure.py) records compiler
commands, binary hashes, benchmark revisions, output hashes, and individual
samples. Use a fresh output directory for a new comparison.
