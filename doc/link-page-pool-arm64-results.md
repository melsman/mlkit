# Link-selected no-spawn page pool: Mac experiment

Historical experiment: see the [integrated automatic-selection assessment](no-spawn-runtime-results.md) for the completed implementation and seed-built compiler results.

This tests selecting a sequential page-pool implementation at link time while retaining the parallel ABI. No new dynamic `may_spawn` check is added to page allocation. The comparison is against the context-passing runtime, not against the original runtime or `-no_par` compilation.

## Scope and implementation

The experimental `MLKIT_NO_SPAWN_PAGE_POOL` compile-time switch builds a replacement `Region.o`. An archive is copied from the context-passing runtime and only that member is replaced. All other runtime objects and generated ML object files are identical. Region descriptor and context layouts remain parallel.

The replacement takes pages directly from `global_freelist`, returns reset/deallocated page chains there, omits the batch-transfer loop and Region.c freelist lock calls, and makes thread-freelist merging a no-op. Context lookups used solely for page management are compiled out. Region mutex/protection code outside page-pool management remains as before. The object has no undefined references to `thread_info`, `mutex_lock` or `mutex_unlock`; the lock symbols here denote the global-freelist wrappers, not every possible protected-region operation.

**This is a manually link-selected prototype**, used only for the known no-spawn benchmark executables. Automatic archive selection from the compiler's link-object boolean is not implemented. The default runtime is unchanged. The prototype is restricted to the non-profiling, non-GC POSIX parallel runtime; unsupported configurations fail compilation. It must not be selected for the REPL or executables that may spawn. No new archive is installed and no production link policy changes.

## Correctness

A focused C probe checks page growth and data preservation, multi-page reset returning pages to the global pool, large-object cleanup, deallocation, and thread-freelist cleanup without list corruption. It verifies that the thread-local freelist remains empty and that page counts balance. All checks pass. The manual script also checks undefined symbols and rejects the unsupported profiling combination. It adds no CI performance test.

All 200 timed benchmark executions match their reference stdout files. Every generated `F.*` function symbol has identical addresses between the two executables for all twenty programs. Runtime code and data placement may still differ, so these are whole page-manager comparisons rather than isolated measurements of one loop or lock.

## Measurement

2026-10-26, native ARM64 macOS. Same twenty unchanged mlkit-bench programs, revision `5293c79d478515912153d54b81bc7cea9a17b7a0`, flags `-ri -par -no_gc`. Same prepared parallel Basis and preserved final PR #253 compiler as the previous Mac experiments. All relinking finishes before timing. Five executions per binary; order alternates context/no-spawn, no-spawn/context, beginning and ending with context/no-spawn. No CPU pinning, warm-ups, excluded samples or extra timing runs. User/system CPU, monotonic wall time and maximum RSS are retained for every run. First-run overhead remains in the data.

CPU seconds: median [minimum, maximum]. Negative delta means the link-selected page pool is faster.

| Program | Context-passing runtime | No-spawn page pool | CPU delta | Wall delta |
|---|---:|---:|---:|---:|
| barnes-hut | 1.0243 [1.0155, 1.0345] | 0.9773 [0.9711, 0.9986] | -4.59% | -4.69% |
| fft | 0.2381 [0.2301, 0.2615] | 0.2290 [0.2246, 0.2630] | -3.81% | -3.87% |
| fib37 | 0.2015 [0.2003, 0.2297] | 0.2016 [0.2000, 0.2298] | +0.08% | +0.18% |
| kbc | 0.3315 [0.3306, 0.3520] | 0.3124 [0.3111, 0.3277] | -5.75% | -4.57% |
| lexgen | 0.2622 [0.2588, 0.2777] | 0.2581 [0.2525, 0.2827] | -1.58% | -1.09% |
| life | 0.4205 [0.4185, 0.4407] | 0.4243 [0.4228, 0.4489] | +0.90% | +0.31% |
| logic | 0.4304 [0.4271, 0.4514] | 0.4277 [0.4261, 0.4513] | -0.63% | -0.53% |
| mandelbrot | 0.1666 [0.1650, 0.1934] | 0.1675 [0.1640, 0.1948] | +0.54% | +1.26% |
| mlyacc | 0.1183 [0.1165, 0.1396] | 0.1181 [0.1160, 0.1464] | -0.15% | -0.59% |
| mpuz | 0.2762 [0.2730, 0.3044] | 0.2764 [0.2737, 0.3053] | +0.10% | -0.18% |
| msort | 0.2298 [0.2283, 0.2539] | 0.2289 [0.2268, 0.2586] | -0.39% | -0.15% |
| nucleic | 0.0830 [0.0814, 0.1066] | 0.0822 [0.0813, 0.1078] | -0.93% | -0.52% |
| professor | 0.1638 [0.1620, 0.1881] | 0.1587 [0.1565, 0.1837] | -3.09% | -2.59% |
| ratio | 0.5527 [0.5496, 0.5740] | 0.5498 [0.5473, 0.5748] | -0.51% | -0.14% |
| ray | 0.8858 [0.8821, 0.9069] | 0.8799 [0.8757, 0.9021] | -0.67% | -0.15% |
| simple | 0.4136 [0.4123, 0.4408] | 0.4124 [0.4109, 0.4504] | -0.30% | +0.12% |
| tak | 0.4450 [0.4410, 0.4688] | 0.4455 [0.4412, 0.4698] | +0.11% | +0.28% |
| tsp | 0.3278 [0.3243, 0.3501] | 0.3256 [0.3229, 0.3523] | -0.68% | -0.60% |
| vliw | 0.2626 [0.2597, 0.2851] | 0.2546 [0.2521, 0.2799] | -3.03% | -2.90% |
| zebra | 0.5328 [0.5315, 0.5432] | 0.5309 [0.5248, 0.5510] | -0.36% | -0.65% |

Geometric mean of all twenty per-program median ratios: **-1.25% user CPU**, **-1.07% wall time**. All programs are included.

## Assessment

Compared with context passing alone, Barnes–Hut improves 4.6% and KBC 5.8%. Both have lower CPU time in every corresponding round, and their full observed CPU ranges do not overlap. This corroborates the earlier global-page-pool lead in a broader controlled comparison and shows that removing first-page context lookup alone did not account for the earlier gains.

FFT, Professor and VLIW also improve around 3–4%. Most remaining changes are small; Life is 0.9% slower with overlapping full ranges. Five samples do not establish statistical confidence, particularly for sub-percent differences. No claim is made that all programs improve.

The experiment changes page batching, pool accesses, page reuse order, context handling on remaining page operations, and infrequent synchronization together. It establishes the effect of the combined policy in these runs, not the individual contribution of each mechanism. There are no X64 results yet and no unsupported-mode performance claims.

The result supports further work on this approach. Before production use: implement conservative automatic link selection, retain the current runtime for REPL/may-spawn executables, validate all supported configurations and relevant profiling accounting, and assess the ThinkPad later. Keep this independent of making parallelism the default.

## Reproduction and evidence

The manual correctness/build script is `src/Runtime/tests/check-no-spawn-page-pool.sh`; it creates an experimental archive in scratch space from a built context-passing runtime. Use the same ML objects and replace only the runtime archive in each captured linker command. `doc/native-performance/link-page-pool-arm64/run.sh` records the five-run measurement order and exact programs; the build log and address checks document both variants. Timed archives/executables and detailed linker logs remain in `/private/tmp/mlkit-link-page-pool`.

[Raw samples and verification](native-performance/link-page-pool-arm64/) include all 200 samples, successful-run logs, symbol checks and hashes. [Context-passing assessment](page-context-arm64-results.md) documents the baseline change and prior validation. No performance measurements are added to CI.
