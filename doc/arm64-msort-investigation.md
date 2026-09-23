# msort investigation

The current GC backend does **not** reproduce an ARM slowdown. On September
23, 2026, with source `c898ef8`, ARM takes 0.463 s versus X64/Rosetta 2's
0.569 s: ARM is 18.6% faster. The remaining disadvantage is peak memory.

| Configuration | Elapsed (s) | GC time (ms) | Collections | Peak RSS (MiB) |
|---|---:|---:|---:|---:|
| Previous ARM, before UF/DLX fixes | 0.4688 | 198.5 | 26 | 146.7 |
| Current ARM | 0.4629 | 194.3 | 26 | 146.7 |
| Current X64 / Rosetta 2 | 0.5686 | 239.9 | 26 | 132.4 |

The [original milestone-11 report](arm64-runtime-performance.md) had ARM at
1.5129 s versus X64 at 0.5718 s. Those historical figures precede the later
backend optimizations. The [recent 20-program report](arm64-gc20-current.md)
already showed ARM faster (0.4629 s versus 0.5392 s). The latest pending-GC
request fix does not regress msort or change its collection count.

## Workload and measurement

This is `mlkit-bench/benchmarks/msort.sml`, revision `5bff5c9`, from the
20-program suite. It generates one million pseudo-random integers, then
sorts them with recursive list merge sort. The repository also contains a
different `src/test/msort.sml`; that is not the measured workload.

Apple M2 Max, 32 GiB, macOS 26.5.1. Both programs are compiled with `-gc`,
using the current runtimes and the complete Basis libraries freshly rebuilt
for the UF/DLX fixes. Host compilers use MLKit `-gc`; no MLton is used.
One warmup and five measured runs per configuration, shuffled order, with no
concurrent builds. Tables report medians. Peak RSS comes from macOS `wait4`,
in bytes divided by 1,048,576. All runs exit successfully and match the
benchmark's expected stdout. Its output is progress messages, not a checksum
of the sorted list.

## Neither phase is slower on ARM

A separate diagnostic adds timestamps and cumulative GC counters around
input generation and sorting. Its calls occur outside the inner loops.

| Phase | ARM elapsed (s) | X64 elapsed (s) | ARM GC (ms) | X64 GC (ms) |
|---|---:|---:|---:|---:|
| Generate input | 0.0468 | 0.0516 | 18.6 | 26.0 |
| Sort | 0.4097 | 0.4973 | 174.2 | 212.9 |

Both targets perform 13 collections during generation and 12 during sorting;
one collection occurs outside these timed phases. Roughly 42% of the
unmodified ARM program's elapsed time is GC. Verbose GC traces show matching
page/live-data sizes and collection counts between targets, unlike the DLX
request-suppression problem.

## Larger recursive frames explain the memory opportunity

The generated `merge` is not tail-recursive: it calls `merge` before
constructing each result cons cell. At the final merge, depth can approach
one million calls.

- ARM uses 32 bytes of locals and a 16-byte saved-frame-pointer/return-address
  header: **48 bytes per recursive level**.
- X64 uses 24 bytes of locals and an 8-byte return address:
  **32 bytes per recursive level**.

The extra 16 bytes over about one million levels is about 15.3 MiB, close to
the observed 14.3 MiB RSS gap. This is an explanation supported by generated
frame sizes and the workload, not a direct per-page attribution of RSS.
Heap/live-page statistics match, and the memory gap develops during sorting.

A diagnostic source rewrite selecting `(head,left,right)` before a shared
recursive call does not help: the compiler materializes that tuple on the
stack, increasing the ARM frame to 64 bytes. In a separate five-run batch it
uses 161.9 MiB versus unmodified ARM's 146.7 MiB, a further 15.3 MiB, with
elapsed time 0.4741 s versus 0.4682 s. That supports frame size as the main
memory difference, but is not a recommended source change.

The useful compiler opportunity is reducing preserved stack state across
recursive calls. Only the selected head and destination region need to
survive the recursive merge, but the current code has separate slots for
both candidate heads plus the region. Delaying stores until the branch is
known, or sharing branch-exclusive spill slots, could reduce locals from
32 to 16 bytes and the total ARM frame from 48 to 32 bytes. This would need
liveness/spill and GC-metadata changes, not just an instruction peephole.
A more general tail-modulo-cons transformation could remove the deep
recursion, but would affect both backends and is a larger project.

No compiler or runtime changes were made for this investigation.
[Raw measurements and diagnostics](arm64-performance/msort-investigation/)
retain local temporary paths and executable hashes.
