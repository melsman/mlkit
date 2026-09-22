# ARM64 instruction selection and peephole cleanup

Implements items 1 and 2 from the remaining-slowdown investigation. Inlining,
register allocation, the internal call ABI and instruction scheduling are
unchanged.

## Changes

`CodeGenArm64.sml` uses allocated source/destination registers for record
selection, reference loads and boxed constructor deconstruction. An unboxed
constructor with tag zero uses the ordinary assignment path. This avoids
round trips through the scratch registers.

Integer comparisons, small switches and add/subtract operations use immediate
operands for nonnegative encoded constants up to 4095. Tagged arithmetic
adjusts the encoded constant before testing the range; subtracting integer one
therefore emits `subs ..., #2`. Checked arithmetic retains its overflow branch.
Boxed constants, negative constants and values outside this immediate range
retain the existing register-materialisation path.

Single-case NIL/CONS switches use bit-zero branches on the allocated operand.
Constructor identity proves the valid list representation; other unboxed
datatypes retain the general selector. Multi-case switches retain JumpTables.

`InstsArm64.optimise` now removes round-trip integer copies, folds copies into
loads and immediate ALU operations when the copied register is overwritten,
and removes redundant ORs with known zero while keeping the zero definition.
It pairs adjacent loads/stores in either address order, including ordinary
heap addresses and signed pair offsets. Pairing rejects duplicate load
destinations, a first load that changes the base, mixed register banks/widths,
writeback, and offsets outside the encoding range.

Branch threading follows label-only blocks ending in a direct branch. Cycles
are detected; labels and directives are retained for GC/exception metadata and
address references. Three bounded local sweeps expose nearby rewrites without
an unbounded fixed-point pass. All local rewrites preserve register and NZCV
effects; no new liveness analysis is needed.

## Generated code

Professor's ordinary nonterminal list-indexing iteration now executes six
instructions rather than 19, without a function-specific optimisation:

```asm
tbnz x1, #0, empty
cmp x2, #1
b.ne recurse
// successful lookup path
recurse:
ldr x1, [x1, #16]
subs x2, x2, #2
b.vc loop
```

The frame and exception paths remain. Benchmark-unit assembly contains
213,375 instructions for mlyacc versus 246,726 before (13.5% fewer), and
14,237 for professor versus 15,744 (9.6% fewer). These are static counts,
excluding Basis/runtime objects, not cycle or executed-instruction counts.

## Runtime results

Median wall-clock seconds, GC enabled, one warmup and five measured runs per
variant in deterministic shuffled order on the same M2 Max:

| Benchmark | X64 / Rosetta 2 | ARM64 before | ARM64 after | ARM64 reduction |
| --- | ---: | ---: | ---: | ---: |
| mlyacc | 0.2021 | 0.2730 | 0.2533 | 7.2% |
| professor | 0.2458 | 0.2868 | 0.2598 | 9.4% |

Both instruction selection and peephole cleanup are enabled together. These
measurements do not attribute the improvement separately to each change.
ARM64 remains approximately 25% slower than X64 for mlyacc and 6% slower for
professor; the inlining opportunity is still deferred.

The before executables use `ad5a7a8` with its rebuilt `M12FreshFrames` Basis;
those compiler sources are also the baseline at report-only commit `dd60bef`.
The after executables and their entire Basis were freshly compiled into
`M12SelectFinal` with this implementation. X64 uses the unchanged milestone-11
GC executables. Compilation and validation had finished before timing.
All 36 warmup/measured executions matched expected stdout. Collection counts
remain 28 and 263. CPU medians also improve, from 0.2685 to 0.2486 seconds for
mlyacc and 0.2809 to 0.2542 seconds for professor.

## Validation

Compiler, ReML and emitter-test binaries are built with MLKit and `-gc`.
The final native suite, dedicated switch suite and parallel suite all passed.
The native suite includes new cases for tagged/untagged immediate boundaries,
negative-constant fallback, overflow, empty/nonempty lists, field selection
and reference loads. Emitter checks cover profitable peepholes and rejection
cases, branch cycles, metadata barriers and pair encoding boundaries.

The native suite also exercises GC/generational GC, profiling, forced GC,
exceptions, stack/register roots, foreign calls, tail frames and large frames.
Dedicated switch and parallel checks cover their existing configurations;
Argobots remains skipped because it is not configured locally.

Timing data and the generated loop are retained in
[the supporting directory](arm64-performance/instruction-selection/).
