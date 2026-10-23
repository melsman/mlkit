# Fifteen mlkit-bench programs: PR #253

This comparison supersedes the earlier ten-program test-tree screen. It uses fifteen unchanged programs from `~/gits/mlkit-bench/benchmarks`, revision `5293c79d478515912153d54b81bc7cea9a17b7a0`. Each executable ran exactly three times on native X64 and ARM64: 180 executions, all matching the benchmark reference outputs. No warm-up runs, discarded samples or additional timing runs were used.

Both variants use sequential MLKit with ordinary GC (`-gc -no_par`). A separate Basis library was built first with each matching compiler using `-c basis/repl.mlb`; normal benchmark compilation then reused that prepared library. Sources and caches were isolated from the user's checkout. Both versions on each host link the same runtime. This measures the complete PR against the preserved pre-PR compiler, rather than separating the register-preservation and reset changes. Compiler/runtime hashes, Basis build logs and evidence of prepared-library reuse accompany the raw data.

All compilation completed before timing. Execution order was before/after, after/before, before/after. X64 ran on the ThinkPad pinned to CPU 0 with a temporary idle inhibitor; ARM64 ran natively on macOS without CPU pinning. The manual timer measures child user/system CPU usage with `wait4`, monotonic wall time and maximum RSS. Timing includes process startup. Three samples describe these executions; they do not establish statistical confidence.

## Results

User CPU seconds are median [minimum, maximum]. Changes compare medians; negative means faster. Wall-time changes are shown separately, with every underlying sample retained in the CSV files.

### X64 — ThinkPad

| Program | Before CPU seconds | PR CPU seconds | CPU change | Wall change |
|---|---:|---:|---:|---:|
| barnes-hut | 0.6626 [0.6621, 0.6634] | 0.6622 [0.6609, 0.6625] | -0.06% | -0.27% |
| fft | 0.2655 [0.2645, 0.2739] | 0.2584 [0.2578, 0.2614] | -2.67% | -4.54% |
| fib37 | 0.2606 [0.2592, 0.2607] | 0.1828 [0.1819, 0.1830] | -29.86% | -29.83% |
| kbc | 0.3230 [0.3203, 0.3315] | 0.2871 [0.2852, 0.2906] | -11.11% | -11.12% |
| lexgen | 0.3496 [0.3488, 0.3513] | 0.3252 [0.3247, 0.3279] | -6.99% | -6.44% |
| life | 0.6242 [0.6205, 0.6243] | 0.6098 [0.6043, 0.6098] | -2.30% | -2.38% |
| logic | 1.2225 [1.2219, 1.2231] | 1.2150 [1.2133, 1.6709] | -0.61% | -0.52% |
| mandelbrot | 0.2848 [0.2846, 0.2850] | 0.2816 [0.2803, 0.2825] | -1.10% | -1.15% |
| mlyacc | 0.1399 [0.1394, 0.1444] | 0.1336 [0.1306, 0.1341] | -4.46% | -4.50% |
| msort | 0.4646 [0.4590, 0.4652] | 0.4584 [0.4575, 0.4625] | -1.33% | +0.39% |
| nucleic | 0.1245 [0.1244, 0.1252] | 0.1226 [0.1225, 0.1236] | -1.55% | -2.03% |
| ray | 0.9846 [0.9835, 0.9874] | 0.9471 [0.9462, 0.9481] | -3.80% | -3.57% |
| simple | 0.6307 [0.6299, 0.6321] | 0.6281 [0.6281, 0.6286] | -0.41% | -0.47% |
| tak | 0.5455 [0.5437, 0.5465] | 0.5146 [0.5140, 0.5150] | -5.67% | -5.76% |
| vliw | 0.3285 [0.3261, 0.3317] | 0.3056 [0.3049, 0.3080] | -6.96% | -7.02% |

Geometric mean of the 15 median ratios: **-5.58% user CPU**, -5.60% wall time. Excluding Fibonacci as a sensitivity summary: -3.55% user CPU across the other 14 programs. All 15 remain in the primary result.

### ARM64

| Program | Before CPU seconds | PR CPU seconds | CPU change | Wall change |
|---|---:|---:|---:|---:|
| barnes-hut | 0.8257 [0.8208, 0.8513] | 0.8279 [0.8197, 0.8436] | +0.27% | -0.67% |
| fft | 0.2772 [0.2763, 0.3041] | 0.2790 [0.2767, 0.3045] | +0.66% | +0.98% |
| fib37 | 0.2957 [0.2946, 0.3257] | 0.2802 [0.2783, 0.3096] | -5.23% | -5.06% |
| kbc | 0.3327 [0.3321, 0.3503] | 0.3313 [0.3303, 0.3512] | -0.43% | -1.32% |
| lexgen | 0.3124 [0.3115, 0.3268] | 0.3145 [0.3085, 0.3367] | +0.69% | +5.49% |
| life | 0.5642 [0.5632, 0.5895] | 0.5657 [0.5647, 0.5906] | +0.25% | +0.57% |
| logic | 0.9621 [0.9556, 0.9871] | 0.9545 [0.9488, 0.9723] | -0.79% | -0.54% |
| mandelbrot | 0.1713 [0.1702, 0.1961] | 0.1710 [0.1708, 0.1948] | -0.19% | -0.30% |
| mlyacc | 0.1328 [0.1316, 0.1542] | 0.1316 [0.1315, 0.1527] | -0.92% | -0.77% |
| msort | 0.4110 [0.4104, 0.4288] | 0.4074 [0.4045, 0.4349] | -0.86% | -0.72% |
| nucleic | 0.1249 [0.1242, 0.1482] | 0.1242 [0.1235, 0.1484] | -0.54% | -1.33% |
| ray | 1.0887 [1.0809, 1.1091] | 1.0865 [1.0859, 1.0922] | -0.20% | -1.26% |
| simple | 0.4236 [0.4214, 0.4486] | 0.4236 [0.4215, 0.4463] | -0.01% | -0.39% |
| tak | 0.6546 [0.6491, 0.6801] | 0.6495 [0.6435, 0.6719] | -0.77% | -1.34% |
| vliw | 0.3128 [0.3118, 0.3319] | 0.3140 [0.3117, 0.3355] | +0.36% | +0.36% |

Geometric mean of the 15 median ratios: **-0.52% user CPU**, -0.44% wall time. Excluding Fibonacci as a sensitivity summary: -0.18% user CPU across the other 14 programs. All 15 remain in the primary result.

## Interpretation and limits

X64's primary CPU aggregate improves 5.58%; ARM64's improves 0.52% and is broadly unchanged. On X64, KBC improves 11.1%, lexgen and VLIW about 7%, and several other programs show smaller gains. These are whole-PR observations, not isolated estimates of either optimisation's contribution.

Fibonacci needs particular care: X64 improves 29.9%, although its recursive hot function has the same instruction sequence modulo generated labels and does not call reset or C. Its entry moves from `0x74b79` to `0x74259`, changing its position within a 64-byte cache line by 32 bytes. Generated assembly and disassembly addresses are saved in `thinkpad/fib37-*`. Code placement is a plausible contributor; these measurements do not establish a specific branch-prediction or instruction-fetch mechanism. The large gain cannot be credited directly to reduced reset or foreign-call overhead. The aggregate excluding Fibonacci is therefore a useful secondary sensitivity check. ARM64 Fibonacci also improves about 5.2%; it likewise should not be treated as evidence of reset-path savings.

All samples are retained, including X64 logic's candidate user-CPU outlier (1.6709 seconds) and ARM64's early-run variability. ARM64 lexgen's median wall time increases 5.5%, while user CPU increases 0.7% with overlapping ranges. These three runs do not establish a systematic regression, but the wall result remains visible above. Tiny differences should not be read as proven improvements.

## Reproduction and evidence

Run the manual `src/Compiler/Backend/tests/benchmark-mlkit-bench.sh` with `SML_LIB` set to the common MLKit source/runtime checkout, `BENCH_ROOT` to the benchmark checkout, and `MLKIT_BEFORE` / `MLKIT_AFTER` to the compiler binaries. The script prepares each Basis, compiles all fifteen programs, runs exactly three pairs and compares every stdout result. Linux uses `BENCH_CPU` (default 0). Machine and compiler identities are saved with these results.

Raw CSVs, complete run logs, compiler/runtime identities, prepared Basis logs/hashes, output hashes and executable sizes are in [native-performance/mlkit-bench](native-performance/mlkit-bench/README.md). No performance tests or timing collection were added to CI.
