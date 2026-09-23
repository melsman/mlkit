# Compile-time register preservation at ARM64 region calls

Region entry and exit now save only the live subset of the 38 C-clobbered ML
registers. This implements the first follow-up from the
[assembly investigation](arm64-assembly-investigation.md). Frame elimination,
tail-loop conversion and instruction scheduling are separate work.

## Implementation

`CodeGenArm64` propagates physical-register liveness backwards through the
allocated `LineStmt` program. Leaf uses/definitions come from the shared
`LineStmt.def_use_var_ls` interface, including region pointers and unboxed
floating-point values. Ordinary switches union their successors' live sets.
Region entry uses the body's incoming set; region exit uses its outgoing set.
Return registers seed function-exit liveness even when no explicit subsequent
statement reads them.

The helper emitter intersects that set with `x0–x15`, `d0–d7`, and `d16–d29`.
The resulting stores and loads are fixed instructions: no live-set mask or
metadata is passed to the runtime. Save areas round up to 16-byte alignment;
argument staging and frame-relative descriptor addresses include the padding.
C-preserved `x19–x28` and the low 64 bits of `d8–d15` still rely on the C ABI.

There are deliberate conservative cases. Exception handlers retain the full
save set, both on entry to the construct and for helpers inside it. A
flow-producing operation can jump to a later switch arm past ordinary
definitions; its liveness therefore includes all relevant registers mentioned
in the function. These overestimates avoid requiring a new exception/flow CFG
in this change. Ordinary straight-line definitions still remove dead values.

Profiling's finite-region entry/exit helpers also use the computed live sets.
Shared allocation/reset slow stubs, other internal calls and the GC register
snapshot retain their existing preservation rules. The GC root map is distinct
from this analysis: non-pointer values also need preservation across C calls.

## Validation

The compilers and direct emitter harness were built using MLKit with `-gc`.
MLton was not used.

The new emitter test replaces region helpers with assembly helpers that
clobber every C-volatile ML register, assert stack alignment and write through
the region-descriptor argument. It checks entry and exit separately, empty and
odd-sized save areas, FP spill registers, mixed register banks, C-preserved
registers, nested regions, return values and a flow branch that skips a register
definition. Its expected output is `ABCDEFGHIJKLMNOP`.

The native MLKit/ReML suite passed, covering plain, GC, generational GC, profiling,
forced collection, exceptions, callbacks, REPL loading, allocation/reset paths,
spills and stack results.

## Small performance check

The comparison uses only mlyacc and professor, with GC enabled, on the same
Apple M2 Max. Each variant gets one warm-up and five measured runs, interleaved
in a seeded shuffled order. All outputs are checked. Both variants use the
same pre-change `M12Study` Basis objects; only benchmark units are rebuilt.
This isolates the change in benchmark code and does not measure the additional
effect of rebuilding the Basis with selective preservation.

The baseline is `1261087`, whose production compiler is unchanged from
`07cf1e8`. Raw timings and executable hashes are in
[measurements.json](arm64-performance/live-registers/measurements.json), with a
[reusable runner](arm64-performance/live-registers/measure.py).

| Benchmark | Before (s) | After (s) | Change |
| --- | ---: | ---: | ---: |
| mlyacc | 0.2921 | 0.2863 | -2.0% |
| professor | 0.3528 | 0.2956 | -16.2% |

Professor improves by 16.2%; mlyacc improves by 2.0% in this small run.
Collection counts remain 263 and 28 respectively. Median GC CPU time is
about 4–7 ms, so the savings are outside collection.


In professor's generated unit, the number of instructions falls from 22,364
to 15,762 (29.5% fewer). FP load/store instructions fall from 3,668 to 38;
conservative preservation and existing shared stubs account for the remaining
FP traffic. The 166 region-helper calls remain. These are static counts, not
dynamic execution counts; see [raw counts](arm64-performance/live-registers/static-counts.json).
