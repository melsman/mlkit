# Automatic no-spawn runtime selection: integrated ARM64 assessment

## Result

A built, installed `runtimeSystemParNoSpawn.a` is now selected automatically for supported parallel standalone executables whose retained link objects all report `may_spawn=false`. Both X64 and ARM64 selection code is updated. The runtime uses the parallel ABI and adds no dynamic page-policy check or new user flag.

The integrated Mac comparison uses the **seed-built MLKit compiler** for both configurations. Twenty benchmarks, five executions per binary: all 200 outputs match. The ordinary parallel archive used as the control already includes context passing; this comparison isolates the additional page-pool policy.

## Implementation and conservative cases

The no-spawn archive shares every parallel runtime object except Region.o, in the same archive-member order. Its page manager uses the global free-page pool directly, with no transfer loop, global-freelist lock calls, or context lookups solely for pool access. Freed/reset pages return directly to the same pool. Descriptor and context layouts remain parallel. Thread-freelist merging is a no-op for this policy; startup thread bookkeeping and unrelated runtime components are unchanged.

The linker callback computes the policy from the retained link objects, including cached metadata, and uses that same policy for generated startup code and runtime selection. REPL runtime creation explicitly passes the conservative policy, independent of the previous standalone link.

| Configuration | Runtime |
|---|---|
| POSIX `-par -no_gc`, no retained spawn | runtimeSystemParNoSpawn.a |
| POSIX `-par -no_gc`, retained spawn | runtimeSystemPar.a |
| Region/code profiling (`-rp`) | Existing profiling runtime |
| Argobots | Existing Argobots runtime |
| REPL | Existing runtime, conservatively parallel when `-par` is enabled |
| Non-parallel compilation | Existing sequential/GC runtime according to flags |

The new archive is built and installed by normal runtime targets, including Linux compatibility installation. It is not selected for GC, region-profiling or Argobots combinations. Ordinary runtime-only `+RTS -tp` time sampling remains supported: the unchanged parallel sampling engine and its synchronization stay in the shared profiler object; a live smoke test is described below. No claim is made that `may_spawn=false` means profiling cannot create its own native thread.

## Builds and correctness

Final MLKit and ReML binaries were built with installed **MLKit v4.7.23** and its matching `/usr/local/lib/mlkit` Basis/runtime. The exact seed command, compiler versions and hashes accompany the results. Earlier preliminary binaries were superseded: only the seed-built binaries generated the integrated benchmark executables and ran the final selection suite. Future work in this workspace must use seed MLKit, never MLton.

- The seed MLKit also compiles the X64 compiler sources to host objects; this validates source/type correctness, not native X64 execution.
- The Darwin runtime suite passes with twelve archives, including allocation, concurrent allocation/publication, context-passing checks, no-spawn pool semantics, installation and target guards.
- The archives have identical member lists and every non-Region object is byte-for-byte identical.
- No-spawn pool checks cover growth/data preservation, multi-page reset, large-object cleanup, deallocation and cleanup; unsupported profiling compilation is rejected.
- Both seed-built frontends pass the extended spawn-policy suite: retained spawning units, cached true/false metadata, normal/forced/C-fallback allocation, prepared Basis use, ReML separation modes, globals and real REPL spawning. Both REPL runtimes are freshly created and verified to use the full parallel archive.
- Object-only selection checks cover sequential, Argobots and Argobots-profiling output without requiring an Argobots installation.
- An additional untimed KBC run with automatic no-spawn selection and `+RTS -tp -tp_interval 1ms -p_file ... -RTS` matches stdout and produces a complete v10 trace, including sample records and the final close record.

Correctness/selection checks join existing CI suites. No benchmark runs or performance timing collection enter CI. Native X64 execution and performance measurements are deferred to the ThinkPad.

## Integrated measurement

2026-10-26, native ARM64 macOS. Benchmark revision is recorded in the raw results. The same twenty programs as the preceding Mac experiments are compiled with `-ri -par -no_gc`, using one separately prepared Basis cache built by the new compiler. All compilation and validation finish before timing begins.

Both configurations use normal invocations of the same seed-built compiler. A linker wrapper makes the **reference-only** substitution of `runtimeSystemParNoSpawn.a` with `runtimeSystemPar.a`. For the candidate it leaves automatic selection untouched. Actual linker arguments are recorded and checked for every executable. There is no new compiler strategy flag. All generated ML function symbols have identical addresses in each pair.

Five executions per binary alternate reference/candidate and candidate/reference, beginning and ending with reference/candidate. No CPU pinning, warm-ups, excluded samples or resampling. The timer records wait4 user/system CPU, monotonic wall time and peak RSS in KiB. Runtime code and placement can differ; these results assess the combined page-manager policy rather than attributing costs to a particular instruction or mechanism.

CPU seconds are median [minimum, maximum]; negative change means faster.

| Program | Context-passing reference | Automatic no-spawn runtime | CPU change | Wall change |
|---|---:|---:|---:|---:|
| barnes-hut | 1.0269 [1.0210, 1.0460] | 0.9860 [0.9821, 1.0045] | -3.99% | -4.12% |
| fft | 0.2267 [0.2224, 0.2540] | 0.2222 [0.2184, 0.2530] | -1.99% | -2.70% |
| fib37 | 0.2003 [0.1998, 0.2275] | 0.2000 [0.1997, 0.2301] | -0.14% | +0.06% |
| kbc | 0.3346 [0.3310, 0.3567] | 0.3150 [0.3142, 0.3328] | -5.87% | -4.99% |
| lexgen | 0.2608 [0.2597, 0.2853] | 0.2571 [0.2545, 0.2811] | -1.41% | -1.11% |
| life | 0.4188 [0.4152, 0.4456] | 0.4255 [0.4242, 0.4566] | +1.60% | +1.19% |
| logic | 0.4286 [0.4278, 0.4502] | 0.4283 [0.4245, 0.4513] | -0.07% | -0.96% |
| mandelbrot | 0.1697 [0.1674, 0.1906] | 0.1685 [0.1658, 0.1923] | -0.69% | -1.77% |
| mlyacc | 0.1180 [0.1167, 0.1406] | 0.1171 [0.1163, 0.1424] | -0.79% | -0.35% |
| mpuz | 0.2762 [0.2746, 0.3055] | 0.2764 [0.2752, 0.3044] | +0.07% | -0.26% |
| msort | 0.2283 [0.2278, 0.2575] | 0.2284 [0.2262, 0.2564] | +0.04% | +1.56% |
| nucleic | 0.0832 [0.0827, 0.1091] | 0.0825 [0.0817, 0.1078] | -0.81% | -1.62% |
| professor | 0.1651 [0.1646, 0.1913] | 0.1603 [0.1602, 0.1830] | -2.94% | -2.51% |
| ratio | 0.5533 [0.5526, 0.5795] | 0.5530 [0.5512, 0.5768] | -0.05% | +0.80% |
| ray | 0.8901 [0.8880, 0.9378] | 0.8813 [0.8783, 0.9017] | -0.99% | -1.26% |
| simple | 0.4096 [0.4092, 0.4417] | 0.4099 [0.4089, 0.4404] | +0.07% | +0.13% |
| tak | 0.4452 [0.4447, 0.4728] | 0.4477 [0.4463, 0.4713] | +0.57% | +0.74% |
| tsp | 0.3317 [0.3267, 0.3545] | 0.3274 [0.3270, 0.3537] | -1.30% | -1.36% |
| vliw | 0.2619 [0.2606, 0.2737] | 0.2561 [0.2534, 0.2809] | -2.21% | -2.03% |
| zebra | 0.5367 [0.5307, 0.5510] | 0.5308 [0.5274, 0.5595] | -1.10% | -0.80% |

Geometric mean of all twenty per-program median ratios: **-1.11% user CPU**, **-1.08% wall time**. All twenty programs remain in the aggregate.

## Assessment and remaining work

The integrated measurements corroborate the prototype's modest aggregate benefit: Barnes–Hut improves 4.0%, KBC 5.9%, Professor 2.9%, and VLIW 2.2%. The effect is not uniformly positive: Life is 1.6% slower, and several programs are effectively unchanged. Full ranges are reported; five samples do not establish statistical confidence for small changes. No samples were removed to improve the result.

The result supports keeping the archive-based implementation reviewable for further assessment. X64 runtime execution and performance validation remain deferred. Region-profiling and Argobots retain their existing runtimes; expanding the specialized policy to them is outside this change.

Reproduction: configure with a seed MLKit and matching library installation, build MLKit/ReML/runtime through the root Makefile (or pass MLCOMP explicitly to submakes), and run `src/Compiler/Backend/tests/benchmark-page-pool.sh` with SML_LIB, MLKIT and BENCH_ROOT. The script prepares a Basis, compiles both executable variants through normal compiler invocations, checks selected archive arguments and ML addresses, then performs five timing pairs and stdout comparisons. In this assessment preparation and measurement were split so all compiler/correctness work finished before measurement; the same script sections were used.

[Raw samples and validation](native-performance/no-spawn-runtime-arm64/) include all samples, actual linker arguments, build logs, compiler identities and selection/REPL evidence. Original scratch executables and full symbol lists remain at the location recorded with the data. Earlier [context-passing](page-context-arm64-results.md) and [manual archive selection](link-page-pool-arm64-results.md) reports are retained for provenance and are superseded by this integrated assessment.
