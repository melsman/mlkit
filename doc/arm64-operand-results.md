# ARM64 call and operand optimisation

This implements items 1 and 2 of the [backend comparison](arm64-backend-comparison.md).
Resolved ML calls leave their register arguments and results in place. Only
stack arguments/results and the indirect target require staging. Tail calls
retain overlap-safe stack copying and the existing frame/GC layout. Direct
code-generator clients with unresolved register operands retain the general
staging path.

Assignments now move directly between physical registers or between registers
and stack slots. Common integer/FP arithmetic and comparisons resolve operands
and destinations to their allocated registers, materialising spills/constants
only when needed. This includes checked and tagged add/subtract, unboxed FP
binary/unary operations, and both branching and value-producing comparisons.
NaN comparison conditions, integer overflow checks, and tagging are unchanged.
Boxed/narrow numeric lowering and foreign-call argument staging retain their
existing paths; this is not a complete instruction-selection rewrite.

For example, the four instructions `fmov d30,d14; fmov d31,d16;
fadd d30,d30,d31; fmov d14,d30` become `fadd d14,d14,d16`.
An ordinary resolved one-argument call now reserves its 16-byte header and
calls directly, without saving/reloading the argument or staging its result.

MLKit `-gc` builds of the compiler, ReML and the emitter harness pass. The
native integration suite passes calls, closures, stack/FP arguments, spills,
large frames, overflow and NaN handling, forced GC, generational GC, profiling,
foreign callbacks, and REPL exception recovery. The multi-result emitter
fixtures now additionally exercise resolved calls with four through seven
results and both growing and shrinking tail-call frames. Both resolved and
unresolved variants execute successfully. No full bootstrap or broad benchmark
survey was repeated.

The runtime comparison uses the preceding `88297c5` compiler and this change,
on the same M2 Max machine. Both compilers were built using MLKit `-gc`, with
the existing X64 host compiler and its 1 GiB stack workaround. Benchmark
executables run natively on ARM64. Each version gets one warmup and three
timed runs in alternating order; all 24 executions match expected output.
The GC Basis cache is identical and reused; benchmark source units are freshly
compiled. Thus these measurements cover the changed benchmark code, not a
complete rebuild of the Basis with the new lowering.

| Benchmark, with GC | Before (s) | After (s) | Runtime change |
| --- | ---: | ---: | ---: |
| nucleic | 1.668 | 1.566 | -6.1% |
| mlyacc | 0.490 | 0.433 | -11.7% |
| professor | 0.463 | 0.426 | -7.9% |

Values are median wall-clock seconds. This is a small combined-change
comparison; it does not isolate the contribution of call staging versus
direct operands, or establish effects on other workloads or without GC.

In `nucleic.sml.s`, static instruction lines fall from 93,688 to 89,200
(-4.8%), and `fmov` instructions from 4,114 to 1,424. All 635 adjacent
four-instruction scratch FP arithmetic sequences disappear. Counts exclude
the Basis and other units and are not weighted by execution frequency.

[Runtime samples, commands and executable hashes](arm64-performance/operand-results/measurements.json)
and [static counts and assembly hashes](arm64-performance/operand-results/static-counts.json)
are retained for reproducibility.
