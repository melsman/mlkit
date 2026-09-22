# ARM64 vs X64: 20 benchmarks with `-no_gc`

Measured September 23, 2026 on Apple M2 Max (32 GiB RAM), macOS 26.5.1.
Source revision: `e9b80cc722e2b206888a9a27b48ea2327f53c0f4`. ARM64 programs run natively;
X64 programs run through Rosetta 2 on the same machine.

Both target compilers are rebuilt with MLKit `-gc`, using the established
1 GiB host stack. Compiler execution is outside the measurements. All
benchmark programs and both complete Basis libraries are compiled with
**`-no_gc`**, into initially absent `NoGC20Currentarm64` and
`NoGC20Currentx64` caches. No MLton is used.

Both non-GC runtimes (`runtimeSystem.a`) are rebuilt from source. The
checkout's original Darwin X64 configuration is restored afterward.
Executable architectures are checked, and neither `_gc` nor `_time_to_gc`
is present in the benchmark symbol tables. Both targets use a 256 MiB stack
reservation; memory measurements report resident pages, not that reservation.

This is the normal non-GC compiler configuration: region inference and
region-based allocation/reclamation remain enabled, and values are untagged.
It is not a GC executable run with `-disable_gc`. Because `-no_gc` also changes
representation and compiler settings, a comparison with GC results would not
isolate collector overhead alone.

The workload is the same 20 benchmarks and original input sizes as the
[GC comparison](arm64-gc20-current.md) and
[earlier milestone-11 report](arm64-runtime-performance.md): ten from
`debs-icfp24` at `03d473d` and ten from `mlkit-bench` at `5bff5c9`.

Each executable receives one warmup and five measured executions. Backend
order is shuffled deterministically. All compilation finishes before
measurement. Every one of the 240 executions exits successfully and matches
the benchmark's expected stdout byte for byte. Tables report medians of the
five samples. Timing includes process startup and benchmark I/O.

ARM64 is faster on 14/20 benchmarks and uses less peak RSS on 15/20. Geometric mean ARM64/X64 ratios: 0.847 for time and 0.868 for peak RSS.

A ratio below 1 favors ARM64. Small differences should not be interpreted as
precise architectural effects; this compares native ARM64 with X64 under
Rosetta 2, including their different runtime and code footprints.

## Execution time

| Benchmark | X64 / Rosetta 2 (s) | ARM64 (s) | ARM / X64 |
|---|---:|---:|---:|
| barnes-hut | 1.5142 | 1.0742 | 0.71 |
| calc | 1.1458 | 0.9431 | 0.82 |
| DLX | 0.2380 | 0.2442 | 1.03 |
| fft | 0.3140 | 0.2775 | 0.88 |
| kbc | 0.6206 | 0.6407 | 1.03 |
| lexgen | 0.3995 | 0.2877 | 0.72 |
| life | 0.4934 | 0.4842 | 0.98 |
| logic | 0.7046 | 0.4997 | 0.71 |
| mandelbrot | 0.3164 | 0.1919 | 0.61 |
| mlyacc | 0.2088 | 0.1532 | 0.73 |
| mpuz | 0.2809 | 0.3947 | 1.41 |
| msort | 0.3877 | 0.2896 | 0.75 |
| nucleic | 1.0904 | 0.5714 | 0.52 |
| patricia | 2.1783 | 1.6277 | 0.75 |
| professor | 0.2162 | 0.1893 | 0.88 |
| ray | 0.3496 | 0.2522 | 0.72 |
| simple | 0.4645 | 0.5504 | 1.18 |
| uf | 0.1126 | 0.1595 | 1.42 |
| vliw | 0.4731 | 0.3052 | 0.65 |
| zebra | 0.5654 | 0.5946 | 1.05 |

## Peak resident memory

DLX RSS varies appreciably across runs: X64 ranges from 164.7 to 226.7 MiB,
and ARM64 from 68.8 to 89.5 MiB. The table reports their medians.

Memory is each process's maximum resident set size from macOS
`wait4`/`getrusage` (`ru_maxrss` in bytes), divided by 1,048,576 to obtain MiB.
The table takes the median of five per-run maxima. It includes resident code,
stack, heap, and process-associated Rosetta memory. It does not measure total
allocation, virtual address reservation, or only the ML heap.

| Benchmark | X64 / Rosetta 2 (MiB) | ARM64 (MiB) | ARM / X64 |
|---|---:|---:|---:|
| barnes-hut | 635.3 | 636.3 | 1.00 |
| calc | 321.5 | 324.5 | 1.01 |
| DLX | 197.5 | 82.2 | 0.42 |
| fft | 55.8 | 52.4 | 0.94 |
| kbc | 8.4 | 7.0 | 0.84 |
| lexgen | 66.7 | 65.9 | 0.99 |
| life | 30.7 | 29.4 | 0.96 |
| logic | 815.3 | 816.6 | 1.00 |
| mandelbrot | 3.9 | 2.7 | 0.69 |
| mlyacc | 111.0 | 108.6 | 0.98 |
| mpuz | 4.0 | 2.7 | 0.69 |
| msort | 365.9 | 381.0 | 1.04 |
| nucleic | 1104.2 | 1106.6 | 1.00 |
| patricia | 8.9 | 7.6 | 0.85 |
| professor | 12.0 | 10.6 | 0.88 |
| ray | 17.8 | 17.5 | 0.98 |
| simple | 6.1 | 4.6 | 0.75 |
| uf | 9.4 | 6.6 | 0.70 |
| vliw | 171.9 | 171.0 | 0.99 |
| zebra | 119.6 | 118.9 | 0.99 |

[Raw samples, build manifest, hashes and scripts](arm64-performance/nogc20-current/)
are retained. `prepare.py` builds the host compilers and runtimes;
`run.py build` creates isolated source copies and fresh Basis caches;
`run.py measure` checks output and captures time and memory for each child
individually. Local paths require adaptation on another machine.
