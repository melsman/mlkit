# mlyacc: GC call/return prediction mismatch

The largest measured cause of the remaining mlyacc slowdown is the return
instruction used with the GC ML calling convention. Changing only benchmark
ML returns from `ret` to `br x30` reduces runtime by 30–31% in two batches,
turning a 21–23% deficit against X64/Rosetta 2 into a 15% advantage. This is a
diagnostic assembly experiment; production compiler code is unchanged.

Follow-up: [the compiler implementation and fresh-Basis results](arm64-return-fix-results.md)
confirm the gain across nucleic, mlyacc and professor.

## Current calling convention

`CodeGenArm64.callInto` uses `adr x30, continuation` followed by `b` (or `br`)
when GC is enabled. Inline GC descriptors sit between the call and continuation,
so the continuation address is not the instruction immediately after the call.
However, `epilogueInto` emits `ret`. The non-GC path uses `bl`/`blr` with `ret`.

This is a mismatch between the GC convention and hardware return prediction.
Arm documents `bl`/`blr` as return-prediction-stack pushes and `ret` as a pop in
[the Cortex-A35 reference manual, section A6.3](https://documentation-service.arm.com/static/5e7cd0427158f500bd5c4cc0#page=92).
That manual describes an Arm core, not Apple's M2. The specific prediction
mechanism on M2 is inferred from the controlled experiments below; no hardware
misprediction counters were collected. The performance effect of the opcode
change itself is directly measured.

## Whole-program experiments

M2 Max, GC enabled, one warmup and five measured runs per variant in each
batch, deterministically shuffled. Values are median elapsed seconds. All ARM
variants share the fully rebuilt `M12SelectFinal` Basis and runtime; only copies
of benchmark assembly change. The production baseline uses the implemented
instruction-selection/peephole improvements (`ec0c186` cached objects), relinked
with the typed-instruction compiler at `fbbd01b`. The later CI-only commit
`924178a` does not change generated code. X64 uses the existing milestone-11
GC executable. No compiler or instrumentation build runs during measurements.

| Variant | Batch 1 | Batch 2 |
| --- | ---: | ---: |
| X64 / Rosetta 2 | 0.2006 | 0.2007 |
| Production ARM64 | 0.2468 | 0.2435 |
| Remove tiny leaf frame saves | 0.2394 | 0.2363 |
| Inline eligible tiny leaves | 0.2220 | 0.2198 |
| Inline only eligible comparisons | 0.2193 | 0.2188 |
| `br x30` in benchmark ML returns | **0.1699** | **0.1708** |
| `br x30` only in set-union returns | — | 0.2265 |
| `br x30` only in comparison returns | — | 0.2113 |

The full return experiment replaces 1,197 static instructions; the restricted
variants replace seven union returns and 88 comparison returns. Binary checks
compare the entire 2,818,012-byte `__text` section: every changed word is exactly
`ret` (`0xd65f03c0`) becoming `br x30` (`0xd61f03c0`). Text length, instruction
addresses, and every other instruction are identical. Thus code layout,
instruction count, register allocation and frame traffic do not explain the
return experiment's gain. Basis returns and native runtime/helper code are
unchanged. The individual experiments' gains must not be added together.

All 84 warmup/measured executions match expected stdout. All perform 28
collections. In batch 2, median GC CPU time is 6.0 ms for production ARM64,
5.9 ms for the return variant, and 8.6 ms for X64. Total CPU time falls from
238.9 to 166.0 ms. Collector work does not account for the improvement.

## Why mlyacc exposes it

A separate untimed, output-checked counter run finds 4,811,120 entries to the
dominant union instance, 4,511,040 calls to the terminal greater-than comparator,
and 2,409,740 entries to that union instance's equality wrapper. Millions of
short calls amplify a call/return prediction penalty. The comparator itself is
only a comparison plus tagged-Boolean construction and frame handling.

Inlining 111 comparison call sites improves whole-program time by about 10%,
but changing the return opcode alone improves it by 30%. Removing 37 tiny-leaf
frame saves improves it by about 3%. These results change the priority from
inlining or frame reduction to fixing the GC return convention. They do not
justify attributing the remaining call overhead solely to spills.

## Independent call microbenchmark

A standalone native assembly test performs ten million calls to a leaf that
increments a register. It checks the result, preserves the C ABI, and compares
four combinations. Five shuffled measured runs follow one warmup per variant.

| Call / return | Median seconds |
| --- | ---: |
| Manual continuation + `b` / `ret` | 0.040897 |
| Manual continuation + `b` / `br x30` | 0.008832 |
| `bl` / `ret` | 0.008742 |
| `bl` / `br x30` | 0.008711 |

This reproduces the problematic pairing independently of MLKit, GC, allocation
and register spilling. It supports the return-prediction explanation without
claiming a measured misprediction count or a portable penalty per call.

## Recommended compiler change

Use `br x30` for returns under the GC ML convention, paired with its explicit
continuation and `b`/`br` calls. Keep `ret` for ordinary non-GC `bl`/`blr` calls
and native C/runtime helper returns. Audit unit initialisation, exports/callbacks,
thread entry and exception transitions: some bridges use `bl`/`blr` even in a
GC build, so a global replacement of every `ret` is inappropriate. In particular,
callback entry/exit must keep the native call/return prediction stack balanced.

The alternative is to redesign the inline-descriptor/call layout so normal
`bl`/`blr` leaves the correct return PC and pairs with `ret`. Merely replacing
`b` with `bl` would overwrite x30 with an address inside the descriptor area.
The narrower GC-return change is the first implementation to evaluate, with
callback/exception/GC tests and a small three-benchmark check before adoption.
Inlining remains a separate, deferred opportunity.

[Supporting measurements and scripts](arm64-performance/mlyacc-returns/) include
both batches, executable hashes, rewrite counts, hot assembly, binary comparison
results and the standalone microbenchmark. The whole-program scripts refer to
local build caches; adapt those paths for another checkout. `relink.py` captures
the link command, `experiments.py` creates leaf/inline controls, `returns.py`
creates return variants, `count.py` runs untimed counters, and `measure.py`
checks outputs and records timings. `call-micro.py` is self-contained on macOS
ARM64 and rebuilds its C/assembly inputs in its own directory.
