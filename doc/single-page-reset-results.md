# Shared single-page reset assessment

## Implementation and placement

X64 `__reset_region` now checks for a null large-object list and a null successor
of each generation's first page before entering its existing C bridge. Eligible
regions rewind their allocation pointers directly, and generational builds also
rewind the pages' colour pointers. The original descriptor and status bits are
returned intact. Only reserved `r10`, `r11` and `xmm14` are used; the eligible
path has no explicit stack storage or C call. The bridge still preserves every
C-clobbered ML register when the checks fail.

ARM64's shared reset helper has the same eligibility and state updates using
reserved `x16`, `x17`, `x30`, `d30` and `d31`. It restores both the descriptor and
link register before returning or entering the preserving slow bridge. In
particular, the colour-pointer store uses an explicit scratch address: the
negative-offset utility may borrow a scratch register holding the descriptor.
The generational sentinel fixture caught this during prototype development.

All eligibility checks precede writes, including when only the old generation
has a second page. Layout constants are covered by the existing `Layout.c`
checks for `Gen`, `Ro.lobjs`, page payload and colour-pointer offsets. Profiling
builds are included; a successful fast reset frees neither pages nor large
objects and needs no free-accounting event. Shared profiling ranges cover the
fast prefix and the preserving fallback. Reset concurrency requirements are
unchanged: parallel allocation does not authorise concurrent resets.

ARM64 retains its existing non-parallel, non-profiling site fast path. Moving
that case into the helper increased call overhead in the measured loop. The
site's ineligible branch now goes straight to `mlkit_arm64_reset_slow`, so it
does not repeat eligibility checks. Parallel and profiling sites use the shared
fast helper. X64 sites retain their existing shared-helper calls. These ABI and
placement differences are deliberate; neither backend changes finite-region or
conditional-reset decisions.

## Correctness

The new shared fixture compares generated helpers with real C `resetRegion`
using real runtime regions, page expansion and large objects. It exercises 768
cases per configuration: six allocation scenarios, four descriptor status-bit
combinations and all 32 first-page mark combinations where those marks are defined. The cases cover empty,
partially filled and full pages, multi-page regions, large objects with a single
ordinary page, and an old-generation second page. Fallback instrumentation
asserts that neither generation was modified before entering C. Integer and FP
sentinels test every allocatable register across both paths. Structural checks
reject explicit stack storage in the fast prefix.

Both native backends pass all eight configurations: no GC, GC, generational GC,
profiling, GC/profiling, generational profiling, parallelism and parallel
profiling. This tests 6,144 scenarios per backend, alongside C reference resets.
GC standalone fixtures supply reporting identities but never invoke a collector;
separate generated-ML native regressions cover GC and frame maps.

The X64 developer groups (130/130 ordinary/GC and 130/130 profiling), all 13
parallel cases, allocation/C stress and forced-CAS-failure checks, C-call
alignment/SysDB and preservation checks pass. ARM64's complete native MLKit/ReML
suite and the explicit-region regression suite pass, including reset semantics.
Only correctness checks were integrated into the existing native CI targets;
performance scripts remain manual.

## Manual measurement method

`benchmark-reset-region.sh` compares extracted generated helper bodies with the
same native C driver/runtime and a minimal bridge. It records monotonic elapsed
time for 20 million eligible resets or two million ineligible/mixed iterations.
Multi-page and large-object cases rebuild their allocation state every iteration,
so they include the common allocation/free cost. Mixed iterations expand a page
on one in ten iterations. Four runtime configurations (no GC, parallelism,
profiling, generational GC), four cases and five alternating pairs give 160
samples per backend. These are synthetic workloads, not application hit rates.

`benchmark-reset-loop.sh` compares actual generated ReML allocation/reset loops
in sequential, parallel and profiling modes. The final comparison uses 200
million resets and five alternating pairs per mode, with verified results.
A preceding 20-million-reset screen and the rejected ARM64 helper-only placement
are retained separately. Both variants are compiled before measurement; X64
runs on ThinkPad CPU 0 under a temporary idle inhibitor. No correctness builds
run concurrently with the final measurements. ARM64 uses the local native host;
its runs are not CPU-pinned. Exact identities and raw data are retained below.

## Generated ReML loop results

Median user CPU seconds [minimum, maximum], five samples per variant:

| Backend and mode | Before | Retained implementation |
| --- | --- | --- |
| X64 seq | 1.17 [1.17,1.17] | 0.42 [0.42,0.42] |
| X64 par | 1.11 [1.11,1.11] | 0.36 [0.36,0.37] |
| X64 prof | 1.41 [1.40,1.42] | 0.52 [0.52,0.52] |
| ARM64 seq | 0.35 [0.34,0.35] | 0.31 [0.31,0.31] |
| ARM64 par | 1.10 [1.10,1.10] | 0.46 [0.46,0.46] |
| ARM64 prof | 1.13 [1.13,1.13] | 0.52 [0.52,0.53] |

The ARM64 helper-only placement was rejected: its sequential loop took
0.45 [0.45,0.46] seconds versus 0.34 [0.34,0.34] for the existing site path.
The retained sequential site instruction sequence is unchanged apart from its
cold fallback target. Its final timing difference should not be attributed to a
new fast-path algorithm; binary layout and run conditions can affect this loop.
The parallel and profiling gains correspond to avoiding the preserving C bridge.

## Shared-helper tradeoffs

Median elapsed seconds; each row compares identical C driver/runtime code.
These short cases include their common allocation work as described above.

| Backend, runtime | Eligible single page: before → after | Always multi-page: before → after | Large objects: before → after | 90% eligible: before → after |
| --- | --- | --- | --- | --- |
| X64, no_gc | 0.11440 → 0.03445 | 0.01757 → 0.01856 | 0.04004 → 0.04054 | 0.01392 → 0.00758 |
| X64, par | 0.11164 → 0.03568 | 0.06133 → 0.06211 | 0.04864 → 0.04827 | 0.01605 → 0.01002 |
| X64, prof | 0.13087 → 0.03449 | 0.02105 → 0.02146 | 0.04344 → 0.04353 | 0.01398 → 0.00626 |
| X64, gengc | 0.11743 → 0.04236 | 0.02045 → 0.02148 | 0.04224 → 0.04280 | 0.01470 → 0.00665 |
| ARM64, no_gc | 0.10469 → 0.04045 | 0.01537 → 0.01661 | 0.05943 → 0.06036 | 0.01097 → 0.00540 |
| ARM64, par | 0.10358 → 0.04017 | 0.05685 → 0.05486 | 0.06668 → 0.06761 | 0.01434 → 0.00707 |
| ARM64, prof | 0.10510 → 0.03980 | 0.01801 → 0.01952 | 0.06350 → 0.06446 | 0.01092 → 0.00577 |
| ARM64, gengc | 0.12185 → 0.04105 | 0.02250 → 0.02317 | 0.06400 → 0.06532 | 0.01306 → 0.00630 |

Eligible shared resets improve by about 61–74%; the mixed cases improve by
about 38–55%. Always-ineligible cases show the cost of the extra checks: up to
about 8.3% in this screen, with some changes smaller than run variation. Retain
the optimisation with that explicit tradeoff; no general application hit rate
or overall speedup is claimed. ARM64's existing sequential fallback bypasses
these extra helper checks.

Active profiling runs of the final generated loop completed and decoded on both
hosts (523 ARM64 and 533 X64 time samples). Shared reset code metadata was present.
The GC/profiling matrix independently checks generational and parallel layouts.

Raw samples, helper/caller assembly, executable sizes, correctness logs, compiler
hashes and host metadata are under [native-performance/reset-region](native-performance/reset-region/).
Performance scripts are manual only; no performance tests or timing collection
were added to CI. Milestones 6–8 are complete.
