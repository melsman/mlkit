# ARM64 identity wrappers and self-tail loops

This implements items 2 and 3 of the frame-overhead follow-up, on top of
[`99f760e`'s live-register preservation](arm64-live-register-results.md).
The caller's argument/header layout and GC descriptors are unchanged.

## Identity wrappers

A function that only forwards its incoming register arguments to a direct
callee now emits a branch to that callee. It does not save the frame header,
set the frame pointer or adjust SP. The matcher checks argument/result shapes
and physical register placement. Reordered arguments, stack arguments/results,
indirect calls and executable work prevent this transformation.

The matcher looks through lexical scopes and finite-region declarations whose
storage is unused by the forwarding call. These declarations emit no entry or
exit code without profiling. Infinite regions remain excluded. Profiling and
forced GC checks disable the transformation, preserving their normal entries.

## Self-tail recursion

Eligible functions establish their frame header once and place an internal
loop label after frame setup. A direct self-tail call whose arguments are
already in the correct registers branches to this label. Returning still uses
the original epilogue, so the caller's frame pointer and return address are
restored normally.

The initial eligibility rules require no local frame, stack/frame-address
operands, incoming stack arguments, or stack results. The statement whitelist
allows inline arithmetic/comparisons, field access, ordinary control flow and
exiting raises; it excludes ordinary calls, allocation, nonempty region scopes,
handlers and other unrecognised operations. A self-call needing an argument
shuffle retains the ordinary tail-call path. Profiling and forced collection
also retain that path. Allocating recursion continues through the ordinary
entry and its GC poll.

Both optimisations default to enabled. `-no_arm64_tail_wrappers` and
`-no_arm64_self_loops` permit separate comparisons; use freshly compiled units
when changing these options.

## Validation

MLKit, ReML and the direct emitter were built using MLKit with `-gc`; MLton
was not used. The direct emitter checks frame-free integer and FP wrappers,
argument permutations, long self-tail loops, and fallbacks for local frames,
stack arguments and self-call argument shuffling. It asserts the conservative
code paths under forced collection and profiling. The executable prints the
expected `ABCDEFG`.

The native MLKit/ReML suite passed, including plain and GC modes,
generational GC, forced collection, profiling, exceptions, callbacks, REPL
loading, allocation/reset paths, spills and stack results. Assembly checks
confirm that the non-allocating list sum uses the loop entry, while the
allocating builder and forced-GC sum retain their normal entries and polls.

## Small GC benchmark comparison

The baseline is the previous production compiler at `99f760e`, which already
includes selective region-helper preservation. Benchmark units are rebuilt in
separate directories against the same pre-change `M12Study` Basis objects.
Measurements on the M2 Max use one warm-up plus five interleaved measured runs
per variant, checking every output. The table shows median elapsed seconds.

| Benchmark | Baseline | Wrappers only | Both | Both vs baseline |
| --- | ---: | ---: | ---: | ---: |
| mlyacc | 0.2697 | 0.2552 | 0.2545 | -5.7% |
| professor | 0.2816 | 0.2833 | 0.2718 | -3.5% |


Most of mlyacc's measured gain comes from wrappers. Professor has no matching
wrappers: its wrapper-only assembly is byte-identical to the baseline, so the
small timing difference in that column is noise. The combined version improves
professor through self-loop frame reuse. Collection counts remain 28 for
mlyacc and 263 for professor. These are a small performance check, not a broad
benchmark survey; the contemporaneous baseline differs from earlier runs.

There are 47 frame-free wrappers and 65 self-loop branch sites in mlyacc's
benchmark units, and six self-loop branch sites in professor, including
`count`. The hot `count` back edge is now a single local branch; it avoids
reloading the caller's header and immediately saving it again on re-entry.
Static instruction counts fall from 247,289 to 246,726 for mlyacc and from
15,762 to 15,744 for professor. Dynamic savings depend on execution frequency.

[Raw timings and executable hashes](arm64-performance/tail-frames/measurements.json),
[measurement runner](arm64-performance/tail-frames/measure.py), and
[static counts](arm64-performance/tail-frames/static-counts.json) are included.
