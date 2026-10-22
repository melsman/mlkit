# X64 C-preserved register support

## Contract and implementation

`RegAlloc.do_non_tail_call` marks values live across `CCALL` and `CCALL_AUTO`,
excluding newly defined results. Its GPR colouring already chooses from
`RI.callee_save_ccall_phregs`. X64 defines that palette as `rbx`, `rbp`, `r12`,
`r13`, `r15`; `r14` is reserved for the evaluation context. No X64 FP registers
are C-preserved in this ABI, and the FP colouring palette for C-crossing values
is empty, so those values spill.

`FetchAndFlush.F_ls` handles each `SCOPE` by consulting
`RI.is_callee_save_ccall`. ARM64 implements set membership, whereas X64 returned
false unconditionally. Consequently, X64 registered stack homes and emitted
flush/fetch operations even for values assigned to the C-preserved palette.
The unconditional false is present in the original 64-bit port, commit
`e9ceb566`; the code there does not explain a correctness requirement for it.

The implementation makes the X64 predicate recognise its existing palette.
It changes neither colouring nor argument marshalling. C calls, including
library/runtime callees, must preserve those registers under the SysV ABI.
Generated X64 callback bridges save and restore this palette before entering ML.
Generated callbacks defer GC across their dynamic extent, so eliminating these
redundant homes does not remove roots needed at an ML collection point. Ordinary
ML safe points retain their existing root maps and preservation rules.
Exception-handler liveness continues through the existing fetch/flush handling.

The same predicate also avoids redundant homes for palette values crossing
region creation/release. These are ordinary ABI-conforming C calls. This is
separate from implicit allocation/reset calls: their shared preserving bridges
are unchanged and continue to save C-clobbered ML registers, including FP values.
No live-register annotation is added to implicit calls. No ARM64 source changes
are needed.

## Correctness fixtures

`X64/tests/check-c-call-preservation.sh` compiles and runs a fixture under no GC,
ordinary GC, generational GC, profiling, GC plus profiling and parallelism.
Its assembly callee clobbers all SysV caller-save GPRs and all XMM registers,
while returning its integer argument. The fixture checks manual and converted
calls, more live integers than the preserved palette can hold, live doubles,
nested C-to-ML-to-C callbacks, and an ML handler using values from before a call.
Existing alignment tests cover excess C arguments and return conversions.
The preservation check is invoked by the existing alignment correctness suite.

`X64/tests/benchmark-c-call-preservation.sh` is a manual-only paired comparison:
it builds both variants before timing, pins runs to a CPU, checks results and
retains alternating ABBA/BAAB samples. It is never invoked by CI.

## Native validation and assessment (23 October 2026)

A clean public PR checkout was built on the ThinkPad using the installed MLKit
seed, retaining separate before/after MLKit and ReML executables. The baseline
is the plan-only revision `5a3806c0`; the only compiler source change in the
comparison is the predicate in `InstsX64.sml`. Both variants use the same built
runtime. Compiler, source and runtime hashes and host metadata are retained in
[native data](native-performance/c-call-preservation/thinkpad/host-and-hashes.txt).

The modified compiler passed:

- Six preservation modes: no GC, GC, generational GC, profiling, GC/profiling,
  parallelism, including nested callbacks and caller-save clobbering.
- The existing alignment/SysDB checks in all three GC modes, including odd/even
  stack arguments and converted results.
- Developer regression groups: 130/130 ordinary/GC cases and 130/130 profiling
  cases, zero errors.
- All 13 parallel regressions.
- Inline allocation, helper, forced CAS failure, register sentinels and generated/C
  allocation stress, including the ReML checks in that fixture.
- Detailed profiling ABI checks for C register/stack arguments, converted Int64
  results and protected GC policy.

The unchanged ARM64 compiler passed the native MLKit/ReML suite, including
foreign calls, callbacks, GC register/stack/global relocation and reserved
register checks, plus the switch/precision/REPL suite. These are reference
checks with the preserved ARM64 build from #251, not a new ARM64 implementation.

The manual ThinkPad measurements use Linux X64 on an Intel i7-1360P, CPU 0,
200 million calls per executable, ten samples per variant/workload in five
alternating ABBA/BAAB blocks. All binaries were compiled and checked before
measurement; correctness builds had finished before the final timing pass.
A temporary idle inhibitor covered the run. No persistent host settings changed.

| Workload | Before median [min,max], seconds | After median [min,max], seconds |
| --- | --- | --- |
| Manual C call | 0.37 [0.37,0.37] | 0.37 [0.36,0.37] |
| Converted C call | 0.42 [0.42,0.43] | 0.42 [0.42,0.42] |
| Five live integers across C call | 0.52 [0.52,0.53] | 0.47 [0.47,0.47] |

The pressure case improves by about 9.6%. The other medians are unchanged at
`time`'s 0.01-second reporting resolution. This establishes a benefit in a
synthetic call-heavy case, not a general application speedup.

Generated assembly confirms a removed `rbx` store/reload in the simple loop.
In the pressure case, `r13` and `r15` no longer have redundant stack homes,
removing two store/reload pairs and a 16-byte spill area. ABI stack arguments
remain. Aggregate executable text sizes are unchanged for the simple and
pressure loops and 64 bytes smaller for converted calls; alignment means these
sizes do not directly count removed instructions.

An earlier exploratory pass overlapped correctness compilation and is retained
as `exploratory-concurrent.csv` for transparency; it is not used in the table.
All 60 final samples, emitted assembly, sources and correctness logs are under
[native-performance/c-call-preservation](native-performance/c-call-preservation/).
No performance tests or timing collection were added to CI. The focused
preservation fix is retained; reset inlining remains milestones 6–8.
