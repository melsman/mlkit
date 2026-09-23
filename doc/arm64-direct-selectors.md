# Direct operands in ARM64 switch lowering

Implements item 1 (direct-operand fixes in lowering) from the follow-up to
[the optimisation opportunities review](arm64-next-opportunities.md).
No new liveness pass, dead-write elimination, inlining or scheduling is added.

Enum and full-width numeric switches now compare an allocated register directly.
Multi-case switches keep that source through their comparison tree; jump-table
index calculation writes x16 rather than modifying the allocated selector.
Boxed-constructor/numeric selectors load directly from the allocated pointer
into scratch storage. High-tag extraction and narrow integer normalization
likewise read the original register directly and write a separate scratch result.
Spilled or materialised selectors retain the existing fallback.

Tag encodings, signed/unsigned narrow normalization, default cases, overflow
behavior and inline metadata remain unchanged. In particular, normalization
must not overwrite a register that a selected case still uses.

## Static effect

Benchmark-unit instruction counts (excluding Basis; nucleic includes its link
unit) from the same sources and cache configurations:

| Benchmark | Before | After | Removed |
| --- | ---: | ---: | ---: |
| nucleic | 66,924 | 66,922 | 2 |
| mlyacc | 213,375 | 212,671 | 704 |
| professor | 14,237 | 14,145 | 92 |

Adjacent scratch-copy/compare sequences drop from 2/511/92 to zero respectively.
These are static counts, not executed-instruction counts.

## Focused runtime check

GC enabled on the same M2 Max. Baseline is `a6d7fb9` with its rebuilt
`M12RetAfter` Basis. The candidate rebuilds the entire Basis and all three
programs into an initially absent `M12DirectFresh` cache. Both compilers are
built using MLKit `-gc` with the established 1 GiB X64 host stack; no MLton.

Each batch has one warmup plus five measured runs per variant, shuffled in a
fixed order, with no compilation or test processes running. All 72 executions
across two batches match expected stdout. Collection counts are unchanged:
3,106, 28 and 263. Values are median elapsed seconds; positive changes mean slower.

| Benchmark | Before 1 | After 1 | Change 1 | Before 2 | After 2 | Change 2 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| nucleic | 0.6475 | 0.6443 | -0.5% | 0.6419 | 0.6433 | +0.2% |
| mlyacc | 0.1482 | 0.1503 | +1.4% | 0.1475 | 0.1456 | -1.3% |
| professor | 0.1931 | 0.1927 | -0.2% | 0.1919 | 0.1928 | +0.4% |

There is **no consistent measured runtime improvement or regression** in this
small check. In particular, the first mlyacc slowdown reverses in the second
batch. Retain the simpler generated code without claiming a speedup; these
results do not isolate instruction execution from layout effects.

## Validation

The native MLKit/ReML suite and dedicated switch suite pass, covering GC,
generational GC, forced collections, profiling, callbacks, exceptions, stack
results, constructors, dense/sparse tables, defaults, numeric precision
boundaries and REPL images. New emitter checks verify direct enum comparison,
normalization from the original narrow register and absence of an unnecessary
multi-case selector copy in GC and non-GC modes. A new runtime fixture uses the
selector after dense-table dispatch, including the default arm.

[Raw timings, executable hashes, static counts, scripts and suite logs](arm64-performance/direct-selectors/)
are retained. Scripts reference local build directories and need path adaptation
for another checkout. The baseline executables are those from the return-fix
comparison; this check remeasures them alongside the new candidate.
