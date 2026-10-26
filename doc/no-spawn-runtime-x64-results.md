# ThinkPad X64: runtime changes and repeated sequential/parallel comparison

On 2026-10-26, twenty unchanged mlkit-bench programs ran five times in each of four configurations: **400 executions, all matching reference stdout**. This evaluates both PR #256 runtime changes and repeats the `-ri -no_gc` versus `-ri -par -no_gc` comparison with the updated compiler. It is a manual assessment; no performance measurements enter CI.

## Four controlled configurations

| Name | Compiler flags | Runtime page manager |
|---|---|---|
| Sequential | `-ri -no_par -no_gc` | Current sequential runtime |
| Original parallel | `-ri -par -no_gc` | Region.c before context passing, other runtime objects current |
| Context passing | `-ri -par -no_gc` | Ordinary parallel archive with context passing |
| Automatic | `-ri -par -no_gc` | Automatically selected ABI-compatible no-spawn archive |

All executables use the same new X64 compiler, built with installed **seed MLKit v4.7.15**, its matching Basis/runtime, and no other SML compiler. ReML was built with that seed too. The compiler source revision is `e1f50b8c9a20c832e7379e4324440e8ca045d265` (PR #256); only test/harness portability fixes were applied during this assessment. Exact compiler/runtime and executable hashes accompany the data.

The original-parallel archive is a controlled reconstruction: Region.c from master revision `a48b3679` (full revision saved with the data) is compiled using the same headers/options and substituted into the current parallel archive. Thus context passing is isolated without using the old compiler or changing unrelated runtime objects. The other two parallel variants share every non-Region object as well.

Separate sequential and parallel Basis libraries are prepared first. The three parallel variants share the prepared parallel Basis and generated ML objects; all their generated ML function addresses match for every program. Reference-only linker overrides select the original/context archives; the automatic variant keeps the compiler's normal selection. Actual linker arguments and compile logs are saved.

Hardware: ThinkPad Intel Core i7-1360P, native Linux X64, timed executions pinned to CPU 0. A temporary idle inhibitor prevents sleep; no persistent power, governor or system settings were changed. All builds and correctness tests finished before timing. Benchmark source revision: `5293c79d478515912153d54b81bc7cea9a17b7a0`. No workload resizing, warm-ups, excluded samples or resampling.

For each program, rounds rotate the four variants: sequential/original/context/automatic; original/context/automatic/sequential; context/automatic/sequential/original; automatic/sequential/original/context; automatic/context/original/sequential. The first four rounds balance order positions; the fifth remains recorded. The native timer captures wait4 user/system CPU, monotonic wall time and peak RSS in KiB.

## Aggregate results

Geometric means of per-program median ratios. Negative means lower time. The sensitivity column excludes Tak only; all twenty remain in the primary figures.

| Comparison | CPU, all 20 | Wall, all 20 | CPU, other 19 excluding Tak |
|---|---:|---:|---:|
| Context passing / original parallel | -2.04% | -1.78% | -2.07% |
| No-spawn / context passing | -1.57% | -1.47% | -1.63% |
| Both changes / original parallel | -3.58% | -3.23% | -3.67% |
| Original parallel / sequential | +2.28% | +1.91% | +4.44% |
| Context passing / sequential | +0.19% | +0.10% | +2.27% |
| Automatic parallel / sequential | -1.39% | -1.38% | +0.60% |

The runtime changes improve the parallel baseline: context passing lowers aggregate CPU time 2.04%, and page-pool specialization adds 1.57%, for a combined 3.58% reduction. The large Tak benefit in parallel compilation dominates the raw sequential/parallel aggregate; see the code inspection below before interpreting that number as runtime overhead.

## Per-program results

Median user CPU seconds. Changes are ratios of these medians, not medians of paired percentage changes.

| Program | Sequential | Original parallel | Context passing | Automatic | Context / original | Automatic / context | Automatic / sequential |
|---|---:|---:|---:|---:|---:|---:|---:|
| barnes-hut | 0.8001 | 0.8924 | 0.8432 | 0.7981 | -5.52% | -5.35% | -0.26% |
| fft | 0.1924 | 0.1941 | 0.1932 | 0.1858 | -0.49% | -3.80% | -3.40% |
| fib37 | 0.1832 | 0.1909 | 0.1910 | 0.1909 | +0.06% | -0.08% | +4.17% |
| kbc | 0.2492 | 0.3147 | 0.2725 | 0.2558 | -13.42% | -6.13% | +2.66% |
| lexgen | 0.2734 | 0.2949 | 0.2854 | 0.2766 | -3.22% | -3.08% | +1.18% |
| life | 0.4040 | 0.4098 | 0.4129 | 0.4126 | +0.75% | -0.07% | +2.12% |
| logic | 0.6612 | 0.6961 | 0.6894 | 0.6793 | -0.96% | -1.47% | +2.74% |
| mandelbrot | 0.2877 | 0.2851 | 0.2852 | 0.2851 | +0.04% | -0.02% | -0.88% |
| mlyacc | 0.1253 | 0.1269 | 0.1268 | 0.1206 | -0.05% | -4.89% | -3.75% |
| mpuz | 0.2700 | 0.2779 | 0.2772 | 0.2798 | -0.27% | +0.96% | +3.62% |
| msort | 0.2730 | 0.2766 | 0.2731 | 0.2715 | -1.26% | -0.57% | -0.55% |
| nucleic | 0.1014 | 0.1032 | 0.1021 | 0.1021 | -1.11% | +0.04% | +0.68% |
| professor | 0.1830 | 0.1958 | 0.1871 | 0.1850 | -4.40% | -1.13% | +1.11% |
| ratio | 0.4653 | 0.4620 | 0.4597 | 0.4598 | -0.51% | +0.03% | -1.18% |
| ray | 0.8973 | 0.9026 | 0.8959 | 0.8927 | -0.74% | -0.35% | -0.51% |
| simple | 0.6104 | 0.6105 | 0.6108 | 0.6097 | +0.05% | -0.17% | -0.12% |
| tak | 0.4897 | 0.3370 | 0.3320 | 0.3307 | -1.47% | -0.41% | -32.48% |
| tsp | 0.3159 | 0.3189 | 0.3189 | 0.3150 | +0.01% | -1.22% | -0.29% |
| vliw | 0.3213 | 0.3447 | 0.3268 | 0.3170 | -5.19% | -2.99% | -1.34% |
| zebra | 0.5896 | 0.6400 | 0.6267 | 0.6247 | -2.08% | -0.32% | +5.95% |

Barnes–Hut and KBC benefit from both changes. With automatic selection Barnes–Hut is approximately tied with sequential compilation, and KBC remains 2.7% slower. Other residual differences remain: Zebra is 6.0% slower, Fibonacci 4.2% slower and MPuz 3.6% slower. These timings do not establish one cause for those differences. In the page-pool comparison alone, MPuz is 1.0% slower; most other changes are improvements or small differences.

### Full CPU ranges

Minimum–maximum user CPU seconds, all five samples retained.

| Program | Sequential | Original parallel | Context passing | Automatic |
|---|---:|---:|---:|---:|
| barnes-hut | 0.7754–0.8080 | 0.8812–0.9004 | 0.8368–0.8617 | 0.7930–0.8071 |
| fft | 0.1913–0.1945 | 0.1933–0.1975 | 0.1867–0.1946 | 0.1822–0.1936 |
| fib37 | 0.1832–0.1835 | 0.1901–0.1910 | 0.1908–0.1911 | 0.1897–0.1913 |
| kbc | 0.2441–0.2508 | 0.3119–0.3163 | 0.2688–0.2810 | 0.2536–0.2588 |
| lexgen | 0.2700–0.2758 | 0.2899–0.2974 | 0.2797–0.2899 | 0.2735–0.2827 |
| life | 0.4030–0.4083 | 0.4087–0.4147 | 0.4075–0.4171 | 0.4049–0.4138 |
| logic | 0.6596–0.6904 | 0.6621–0.7117 | 0.6851–0.6976 | 0.6548–0.6884 |
| mandelbrot | 0.2867–0.2885 | 0.2842–0.2854 | 0.2851–0.2853 | 0.2843–0.2853 |
| mlyacc | 0.1199–0.1304 | 0.1253–0.1286 | 0.1213–0.1320 | 0.1174–0.1231 |
| mpuz | 0.2694–0.2710 | 0.2767–0.2810 | 0.2762–0.2817 | 0.2777–0.2838 |
| msort | 0.2621–0.2832 | 0.2724–0.2865 | 0.2611–0.2784 | 0.2653–0.2809 |
| nucleic | 0.0921–0.1079 | 0.0893–0.1063 | 0.0975–0.1156 | 0.0981–0.1099 |
| professor | 0.1821–0.1860 | 0.1949–0.1965 | 0.1856–0.1880 | 0.1817–0.1856 |
| ratio | 0.4603–0.4668 | 0.4581–0.4647 | 0.4574–0.4635 | 0.4591–0.4623 |
| ray | 0.8947–0.8982 | 0.8956–0.9033 | 0.8937–0.9045 | 0.8883–0.8982 |
| simple | 0.6101–0.6121 | 0.6098–0.6121 | 0.6096–0.6114 | 0.6091–0.6102 |
| tak | 0.4895–0.4900 | 0.3313–0.3397 | 0.3290–0.3339 | 0.3285–0.3379 |
| tsp | 0.3136–0.3210 | 0.3164–0.3217 | 0.3151–0.3260 | 0.3082–0.3183 |
| vliw | 0.3074–0.3277 | 0.3375–0.3453 | 0.3212–0.3450 | 0.3132–0.3199 |
| zebra | 0.5866–0.5932 | 0.6272–0.6417 | 0.6222–0.6329 | 0.6214–0.6295 |

## Tak and code placement

Tak is 32.5% faster with automatic parallel compilation, but its hot recursive function has **the same instruction sequence** in sequential and parallel code after normalizing names and branch targets. Both use a 40-byte frame, three recursive calls and a tail jump; the hot function contains no allocation or C call on its normal path. The overflow branches target the same runtime helper.

The entry moves from `0xabe2` to `0xc642`: offsets 34 and 2 within a 64-byte cache line, a 32-byte shift. Original/context/automatic parallel variants have identical ML function addresses, so this shift belongs to the sequential-versus-parallel compilation comparison, not the no-spawn runtime substitution.

This supports treating Tak as potentially affected by code placement. It does not prove instruction-fetch or branch-prediction causation, and its gain must not be credited to faster page management. All twenty programs remain in the primary aggregate; excluding Tak as a sensitivity check gives **0.60% CPU and 0.61% wall overhead** for optimized parallel compilation, compared with **4.44% CPU overhead** for the original parallel manager across the same nineteen programs. These are descriptive figures, not confidence bounds.

## Correctness and portability

Native X64 checks pass: context passing in the main thread and four worker threads; no-spawn growth/reset/deallocation/large objects/cleanup; retained and cached true/false spawn metadata; both frontends and prepared Basis use; profiling/Argobots fallback and REPL spawning; generated/C allocation stress, forced failure and register preservation; and all eight shared-reset modes (plain, GC, generational GC, profiling combinations and parallel combinations).

Linux exposed a missing generated exception entry in the C-only context/page-pool probes. The fixtures now provide an explicit aborting `raise_exn` stub: any unexpected ML exception fails the test. This is a test-link portability fix, not a runtime algorithm change. Both focused probes pass again on the Mac. The archive/object check script is now portable to Linux, and the focused checks join Linux's correctness target. The two-way page benchmark also accepts Linux and pins timed runs to CPU 0; the new four-way harness is manual only.

Five runs limit conclusions for small changes, and all measurements retain startup and operating-system effects. The runtime comparisons hold generated ML code and addresses fixed but do not isolate runtime instruction placement or page-reuse effects. The flag comparison intentionally allows generated code/layout to differ.

## Evidence and reproduction

Run `src/Compiler/Backend/tests/benchmark-parallel-runtime.sh` with SML_LIB, MLKIT, BENCH_ROOT and PARALLEL_ORIGINAL_RUNTIME, after building the current compiler/runtime with seed MLKit. Construct the original archive by compiling the recorded master Region.c with the current parallel runtime's headers/options and replacing only its Region.o member. The script prepares both Basis libraries, builds/checks all eighty executables, then runs the five rotating rounds. In this assessment preparation and measurement were split so validation finished before timing, using the same script sections.

[All 400 samples, output hashes, linker arguments, seed identities, correctness logs, and Tak disassembly](native-performance/no-spawn-runtime-x64/) are saved. Compiler/runtime hashes and the exact original Region revision are included. [The integrated Mac assessment](no-spawn-runtime-results.md) provides the ARM64 comparison: context passing alone was approximately neutral in its twenty-program screen, and the additional no-spawn page manager improved aggregate CPU time 1.11% in the integrated assessment.
