# UF slowdown and DLX memory investigation

Implemented and validated in [UF and DLX production fixes](arm64-uf-dlx-fixes.md).

Measured September 23, 2026, compiler source `6d61e63`, Apple M2 Max,
macOS 26.5.1. Both backends use `-gc`; X64 runs through Rosetta 2.
The freshly rebuilt Basis libraries and runtimes from the
[20-benchmark comparison](arm64-gc20-current.md) are reused. Benchmark units
are recompiled with assembly retained. Production compiler/runtime sources
are unchanged: the changes below are isolated diagnostic experiments.

Each variant receives one warmup and five measured runs in shuffled order.
Tables show medians; every execution exits successfully and matches expected
stdout byte for byte. Peak RSS is macOS `wait4`'s per-process maximum,
converted from bytes to MiB. Builds do not overlap measurement.

## UF: C-call setup dominates the observed gap

| Variant | Elapsed (s) | Relative to X64 |
|---|---:|---:|
| X64 / Rosetta 2 | 0.1337 | 1.000 |
| ARM64 current | 0.1738 | 1.300 |
| ARM64: remove modulo-call wrapper | 0.1167 | 0.873 |
| ARM64: simplify tagged multiplication only | 0.1714 | 1.282 |
| ARM64: both changes | 0.1047 | 0.783 |

UF repeatedly generates random element indices using tagged-word
multiplication and modulo 100000. There are three static copies of the
relevant sequence in `uf0.sml`. Both backends call the same runtime modulo
helper, `__mod_word63` in `src/Runtime/Math.c`.

ARM stages all four arguments through a 48-byte stack area even though they
are already in their required argument registers. It also loads, saves,
sets and restores `disable_gc` around each call. X64 directly calls the
helper. Replacing these three ARM wrappers with bare calls reduces elapsed
time by 32.8%, accounting for more than the ARM/X64 gap.

This experiment is safe for these sites: the divisor is the constant
100000, and the helper neither allocates nor calls back on that path.
It is not justification for removing GC deferral from arbitrary foreign
calls. A compiler implementation should classify suitable runtime helpers
and eliminate argument moves when source and destination registers agree;
exception and callback behavior must remain correct.

The tagged multiplication sequence also stages a temporary through the
stack and performs redundant 63-bit masks. The diagnostic folds the tagged
constant, removes the stack round trip, and eliminates masks made redundant
by logical shifts/tagging. This changes 14 instructions into six at each
site. Alone, it gives only a small effect (1.4%); combined with the call
change, elapsed time falls by 39.7%. These effects are not additive, and this
experiment does not establish a microarchitectural explanation for the
interaction. An earlier five-run batch reproduced the direction: 0.1775 s
baseline, 0.1175 s call-only, 0.1757 s multiply-only, 0.1066 s combined.

UF performs six ARM collections taking about 2.3–2.4 ms, versus seven and
3.4 ms on X64. GC does not explain its slowdown.

## DLX: allocation requests are lost while GC is deferred

DLX is the processor simulator benchmark. It allocates large arrays through
runtime C helpers such as `word_table0`, which allocates and initializes a
table without itself collecting. ARM defers GC across these calls by
setting `disable_gc`.

In `src/Runtime/Region.c`, both page allocation and large-object allocation
currently require `!disable_gc` before setting `time_to_gc` when thresholds
are exceeded. Restoring `disable_gc` after the C call does not recheck those
thresholds. Consequently, allocations inside these calls fail to request
collection, even after returning to ML and reaching safe points.

The isolated runtime experiment removes the `!disable_gc` condition from
these two **request-generation** sites. It leaves ARM's safe-point check of
`disable_gc` unchanged: actual collection remains deferred while disabled.
Only `Region.o` is replaced in a private copy of the ARM GC archive; the
same benchmark and Basis objects are relinked.

| Variant | Elapsed (s) | Peak RSS (MiB) | Collections | GC time (ms) |
|---|---:|---:|---:|---:|
| X64 / Rosetta 2 | 0.3707 | 33.4 | 683 | 161.7 |
| ARM64 current | 0.2494 | 123.7 | 8 | 1.5 |
| ARM64: restore nonallocating-function entry polls | 0.2842 | 92.3 | 8 | 1.6 |
| ARM64: retain pending allocation requests | 0.3573 | 26.2 | 683 | 129.1 |

Restoring ordinary entry polls in all functions does not restore collection:
those polls still see no request. That diagnostic was built with the normal
entry-GC generator invoked unconditionally instead of checking
`LS.allocating body`; it does not force collection at every entry.

Retaining allocation requests restores exactly the X64 collection count and
reduces peak RSS substantially. It also removes much of DLX's apparent ARM
speed advantage: ARM now spends time collecting rather than retaining dead
large objects. It remains about 3.6% faster in this batch. This is primarily
a collection-policy discrepancy, not evidence that equivalent ARM objects
intrinsically occupy substantially more memory.

A separate repeat batch gives 0.2498 s / 89.7 MiB / 8 collections for
current ARM, 0.3579 s / 26.0 MiB / 683 collections for the request change,
and 0.3719 s / 31.9 MiB / 683 collections for X64.

Absolute RSS is sensitive to executable layout and allocator behavior.
The earlier 20-program run measured 74.7 MiB ARM and 27.2 MiB X64; the
recompiled diagnostic binaries above have different peaks. Do not mix those
baselines when calculating reductions. The robust observations are the
8-to-683 collection change and the large reduction within this experiment.
Verbose GC output also shows dead large objects accumulating between ARM
collections. The runtime's final `kb rpages` label is misleading: its value
is the `rp_total` page count, not a byte measurement; it is not used as RSS.

## Recommended changes

1. Optimize calls to suitable internal runtime helpers: avoid redundant
   argument staging and unnecessary GC-deferral work. Start with UF's
   nonallocating arithmetic helpers; simplify tagged-word multiplication
   separately.
2. Separate recording a pending GC request from permission to collect.
   Allocation should retain threshold requests while collection is deferred;
   existing safe points should continue honoring deferral. Check nested
   callbacks, exceptions, and other GC runtime variants before adopting this
   shared-runtime change.

[Raw samples and diagnostic scripts](arm64-performance/uf-dlx-investigation/)
include executable hashes, CPU time, GC counters and RSS. Scripts retain
this machine's temporary paths and captured link commands; generated
assembly/objects and executables are not committed. These are targeted
experiments, not a production implementation or a full regression run.
