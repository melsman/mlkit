# Passing the existing context to first-page allocation: ARM64 assessment

Historical experiment: see the [integrated automatic-selection assessment](no-spawn-runtime-results.md) for the completed implementation and seed-built compiler results.

Experiment on 2026-10-26, using the same prepared parallel Basis and benchmark sources as the earlier Mac comparison: mlkit-bench revision `5293c79d478515912153d54b81bc7cea9a17b7a0`, flags `-ri -par -no_gc`. Twenty programs, five runs per binary, all 200 stdout outputs match their reference files. No warm-ups, excluded samples or resampling.

## Change and correctness

`allocateRegion0` already receives the calling thread's `Context`. It now passes that context to an internal `alloc_new_page_context`, instead of having first-page allocation recover it through `thread_info` and `pthread_getspecific`. The public `alloc_new_page(Gen *)` entry point retains lookup for callers without a context. Descriptor layouts, thread-local freelists, locking, no-spawn policy and public interfaces are unchanged. The optimization also applies when real threads are used; it has no main-thread cache or no-spawn-only assumption. Non-POSIX-thread variants retain their existing freelist policy.

All eleven native Darwin runtime variants built. The native runtime suite passes plain allocation, parallel allocation/publication, installation and target guards. A new correctness check instruments only Region.c's lookup calls: 2,000 region lifetimes each in the main thread and four worker threads must use zero lookups for first-page allocation; explicit use of the public page allocator must still perform lookup. It passes with this change and fails at the zero-lookup assertion with the unchanged allocator. The test joins the Darwin runtime correctness suite. No timing collection enters CI. Argobots was not available for execution testing; X64 measurements are deliberately deferred.

Separate untimed diagnostic runs confirm the effect, with `may_spawn=0` in all cases:

| Program | Baseline context lookups | Candidate context lookups | Reduction |
|---|---:|---:|---:|
| Barnes–Hut | 33,785,830 | 81,329 | 99.76% |
| KBC | 19,607,689 | 5,211 | 99.97% |

## Controlled measurement

Both executables in every pair use identical generated ML object files. The candidate archive differs from the baseline only in `Region.o`; the rest of the archive is copied from the original runtime. Every generated `F.*` function address is identical between variants for all twenty programs (`ml-address-checks.txt`). Runtime code addresses and shape may still differ, so these measurements assess the whole Region.c change, not an isolated instruction cost.

All relinking completed before timing. Round order alternates baseline/candidate, candidate/baseline, beginning and ending with baseline/candidate. Native ARM64 macOS runs are not CPU-pinned. Timer: wait4 user/system CPU, monotonic wall time and maximum RSS in KiB. First runs often have higher elapsed times; every sample is retained. Five samples give limited evidence for small differences. The earlier direct-context experiment had severe timing instability; its samples are separate and are not used in this assessment.

CPU seconds are median [minimum, maximum]; negative delta means faster.

| Program | Baseline CPU seconds | Candidate CPU seconds | CPU delta | Wall delta |
|---|---:|---:|---:|---:|
| barnes-hut | 1.0258 [1.0232, 1.0469] | 0.9997 [0.9936, 1.0261] | -2.54% | -2.53% |
| fft | 0.2333 [0.2210, 0.2522] | 0.2287 [0.2221, 0.2514] | -1.97% | -1.89% |
| fib37 | 0.2001 [0.1999, 0.2283] | 0.2003 [0.2000, 0.2325] | +0.09% | -0.08% |
| kbc | 0.3216 [0.3184, 0.3460] | 0.3216 [0.3205, 0.3473] | -0.01% | -0.03% |
| lexgen | 0.2489 [0.2481, 0.2783] | 0.2517 [0.2508, 0.2795] | +1.14% | +1.05% |
| life | 0.4045 [0.4028, 0.4348] | 0.4066 [0.4032, 0.4380] | +0.53% | +1.07% |
| logic | 0.4232 [0.4222, 0.4499] | 0.4256 [0.4216, 0.4496] | +0.56% | +0.24% |
| mandelbrot | 0.1779 [0.1659, 0.2077] | 0.1897 [0.1752, 0.2068] | +6.62% | +5.90% |
| mlyacc | 0.1227 [0.1212, 0.1257] | 0.1229 [0.1198, 0.1402] | +0.15% | +0.32% |
| mpuz | 0.2942 [0.2859, 0.3105] | 0.2900 [0.2836, 0.3120] | -1.43% | -5.75% |
| msort | 0.2439 [0.2381, 0.2594] | 0.2489 [0.2400, 0.2618] | +2.06% | +0.33% |
| nucleic | 0.0875 [0.0853, 0.0923] | 0.0845 [0.0835, 0.0937] | -3.33% | -6.63% |
| professor | 0.1647 [0.1633, 0.1683] | 0.1644 [0.1621, 0.1792] | -0.19% | -0.22% |
| ratio | 0.5498 [0.5452, 0.5738] | 0.5471 [0.5456, 0.5778] | -0.49% | -0.28% |
| ray | 0.8788 [0.8745, 0.9031] | 0.8810 [0.8719, 0.8997] | +0.24% | +0.21% |
| simple | 0.4074 [0.4072, 0.4395] | 0.4076 [0.4071, 0.4406] | +0.06% | +0.07% |
| tak | 0.4330 [0.4320, 0.4641] | 0.4328 [0.4321, 0.4647] | -0.05% | -0.01% |
| tsp | 0.3221 [0.3184, 0.3493] | 0.3223 [0.3184, 0.3452] | +0.08% | +0.05% |
| vliw | 0.2579 [0.2548, 0.2802] | 0.2565 [0.2556, 0.2810] | -0.55% | -0.43% |
| zebra | 0.5190 [0.5156, 0.5484] | 0.5145 [0.5139, 0.5477] | -0.87% | -0.74% |

Geometric mean of all twenty per-program median ratios: **-0.01% user CPU**, **-0.50% wall time**. All programs are included.

## Interpretation

The redundant lookups are demonstrably eliminated, but the Mac does not show an aggregate CPU improvement. Barnes–Hut improves 2.5%; KBC is unchanged. Mandelbrot's median is 6.6% slower with overlapping full sample ranges; that observed regression must remain visible rather than being dismissed as noise. Several smaller changes also have overlapping ranges. The measurements do not establish the cause of these changes, and unchanged ML function addresses do not rule out runtime or operating-system effects.

This is a bounded implementation for continued investigation, not evidence of a general performance win. Keep the change independently reviewable and corroborate it on X64 later. Do not infer that the earlier global-freelist prototype's 6–7% gains are explained by lookup elimination alone; context passing did not reproduce those gains here. Page-pool/reuse-policy changes remain a separate experiment.

Raw samples, successful-run logs, lookup counts, address checks and executable/archive hashes are in [native-performance/page-context-arm64](native-performance/page-context-arm64/). The manual run script records exact flags/order through the saved build context and uses five runs. Benchmark executables and linker-command logs remain under `/private/tmp/mlkit-page-context-bench`; original Basis preparation is documented in the preceding Mac experiment. The compiler is the preserved final PR #253 ARM64 build; its identity is included in the hashes. No ThinkPad measurements were performed.
