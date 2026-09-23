# ARM64 record, branch and GC optimisation

This implements items 3, 4 and 5 of the
[backend comparison](arm64-backend-comparison.md), following `a0581e1`'s call
and operand changes.

- **Record construction:** keep the allocated pointer in its destination
  register when it does not alias a required source. Store physical-register
  fields directly, and materialise other fields through scratch registers.
  Spilled destinations and source aliases retain the safe stack-based path.
  Tag-free records also protect the region operand used to decide whether to
  write a header.
- **Conditional branches:** retain short branches when a conservative distance
  bound proves they fit. Track text and data separately across section switches,
  including out-of-line allocation/reset blocks, inline descriptors and jump
  tables. Count maximum alignment padding and initially assume long branches;
  subsequent shrinking cannot invalidate a proven span. Unknown directives,
  unknown sections and unresolved targets retain conservative expansion. This
  deliberately does not try to obtain the smallest possible layout at exact
  range boundaries.
- **GC entry and calls:** share one 352-byte snapshot save/restore stub per
  compilation unit, with four metadata words per entry point. Check
  `time_to_gc` first; consult `disable_gc` only for pending/forced collection,
  retaining callback deferral. The layout pass also turns local `adrp`/`add`
  pairs into one `adr` where range is proven, including GC continuation setup.

The GC stub is local to its compilation unit, so separately loaded REPL images
need no new runtime entry point. Its incoming x30 is the stub continuation;
the ML return PC remains in the existing function header, where the root walker
already reads it. The snapshot layout, inline descriptors and runtime ABI do
not change. GC-enabled ML transfers still use `b`/`br`, and returns use `ret`.
Recovering hardware `bl`/`blr` for those ML transfers would require a descriptor
layout change; this change reduces address setup without making that change
or claiming to resolve the previously suspected return-prediction cost.

MLKit `-gc` builds of the compiler, ReML and the emitter pass. The native suite
passes calls, closures, stack and FP arguments, spills, large records/frames,
overflow, NaNs, allocation/reset, forced GC, generational GC, profiling,
callbacks, and REPL exception recovery. New execution tests cover register
record destinations, a destination that aliases a field, and a spilled
destination. Range tests cover forward/backward limits, padding, inline data,
section switches, unknown directives, and near/far addresses. Existing
out-of-range branch assembly executes successfully. GC assembly checks confirm
a single snapshot stub per unit and short continuation-address setup. The
focused dense/sparse/precision-boundary and REPL switch suite also passes.

The small runtime comparison uses `a0581e1` as baseline. Both compiler versions
were built with MLKit `-gc`, using the existing X64 host compiler and 1 GiB
host-stack workaround. Executables run natively on the same M2 Max machine.
Each gets one warmup and three timed runs, alternating version order; all 24
executions match expected output. The unchanged GC Basis cache is reused for
both versions; benchmark units are compiled with their respective compiler.
The final candidate's benchmark caches were cleared after the section-tracking
refinement before recompiling. No full bootstrap or broad performance survey
was repeated.

| Benchmark, with GC | Before (s) | After (s) | Runtime change |
| --- | ---: | ---: | ---: |
| nucleic | 1.504 | 1.438 | -4.3% |
| mlyacc | 0.407 | 0.382 | -6.0% |
| professor | 0.408 | 0.386 | -5.5% |

Values are median wall-clock times for the combined changes, not isolated
measurements of each optimisation. This small comparison does not establish
effects on other workloads or without GC, and does not include a full Basis
rebuild with the candidate.

Static counts in the nucleic source unit show the code reduction:

| Property | Before | After |
| --- | ---: | ---: |
| Instruction lines | 89,200 | 72,956 (-18.2%) |
| Expanded conditional branches | 2,330 | 0 |
| Snapshot stubs calling `_gc` | 54 | 1 |
| Two-instruction x30 address setups | 60 | 0 |
| Single-instruction `adr x30` setups | 0 | 60 |

These counts exclude the Basis and other units and are not weighted by
execution frequency. Other units can still require long branches.

[Runtime samples, commands and executable hashes](arm64-performance/layout-results/measurements.json)
and [static counts and assembly hashes](arm64-performance/layout-results/static-counts.json)
are retained with the report.
