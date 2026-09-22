# Remaining mlyacc and professor slowdown

Investigation at `ad5a7a8`, after live-register preservation, identity-wrapper
elimination, self-tail loops, and a complete GC Basis rebuild. The remaining
measured opportunities are tiny-call overhead in mlyacc and inefficient
instruction selection in professor's list-indexing loop. Broader peephole
cleanup helps both. These explain part, but not all, of the X64/Rosetta 2 gap.
This investigation changes no production compiler or runtime code.

Follow-up: [instruction selection and peephole cleanup](arm64-instruction-selection-results.md)
are implemented. The inlining opportunity remains deferred.

## Controlled experiments

Median wall-clock seconds on the same M2 Max, GC enabled. Each variant uses
identical Basis/runtime objects; only copies of benchmark assembly are changed.
Each batch has one warmup and five measured runs, shuffled deterministically.
The first batch had substantial timing variation in mlyacc, so a second batch
checks the direction and approximate size of the effects.

| Configuration | mlyacc, batch 1 | mlyacc, batch 2 | professor, batch 1 | professor, batch 2 |
| --- | ---: | ---: | ---: | ---: |
| X64 / Rosetta 2 | 0.1956 | 0.1986 | 0.2360 | 0.2388 |
| Current ARM64 | 0.2665 | 0.2701 | 0.2709 | 0.2773 |
| ARM64 + local peephole cleanup | 0.2547 | 0.2558 | 0.2595 | 0.2660 |
| ARM64 + omit tiny leaf frame saves | 0.2667 | 0.2680 | 0.2727 | 0.2761 |
| ARM64 + inline tiny leaf calls | 0.2453 | 0.2468 | — | — |
| ARM64 + compact list-indexing loop | — | — | 0.2564 | 0.2637 |

Inlining improves mlyacc by about **8–9%**; compacting the indexing loop
improves professor by about **5%**. Peephole cleanup improves mlyacc by
**4–5%** and professor by **4%**. Leaf frame-save removal alone has no clear
benefit. Do not add these improvements: combined variants were not measured,
and their opportunities overlap. CPU timings support the same conclusions.

In batch 2 the inline experiment closes about 33% of mlyacc's observed gap;
the compact loop closes about 35% of professor's. They still run about 24%
and 10% slower than X64 respectively. These are small diagnostic experiments,
not a complete performance attribution or precise long-run speed estimates.

## mlyacc: tiny calls remain expensive after wrapper removal

The hot set-union code still calls comparisons, spills values around the
calls, and tests the returned tagged Boolean. Identity wrappers now branch
directly to their targets, but the target still performs a complete ML call
and return for only a few useful operations. Earlier untimed instrumentation
counted 4.81 million entries to the dominant union instance and 6.92 million
combined entries to its greater-than/equality wrappers; these are historical
counts, not new measurements in this investigation.

The current greater-than leaf has 13 instructions: header save/setup, four
register copies, comparison, Boolean encoding, header restore, SP adjustment,
and return. The corresponding X64 leaf has eight instructions. Instruction
counts across ISAs do not establish cycle costs, but the assembly identifies
concrete unnecessary ARM64 work.

The diagnostic inlines 199 static calls to eligible tiny straight-line leaf
operations, including comparisons and field accessors, following identity
wrapper chains. It retains caller spills, return-address values and inline GC
metadata; no callee can observe the temporary frame or make a call. The
measured gain therefore demonstrates an opportunity in tiny-call sequences,
not a measured benefit from eliminating spills or proof of return-prediction
failure. It includes layout effects. Merely removing header save/setup/restore
from 37 leaves does not reproduce the gain.

Recommendation: improve small-function inlining, preferably before register
allocation so that values can also remain in registers and compare/branch
fusion becomes possible at the compiler IR level. Both backends retain such
calls, so this is not proof that inlining is intrinsically ARM-specific.
Backend call/frame specialisation is a narrower alternative.

## professor: self-recursion is a loop, but its body is still verbose

The production `count` function already reuses its frame. Its ordinary
nonterminal iteration nevertheless executes 19 instructions: scratch copies,
mask-and-compare list testing, materialised integer constants, field loading,
checked decrement, and an extra branch. An isolated rewrite reduces this to
six instructions:

```asm
tbnz x1, #0, empty
cmp x2, #1
b.eq found
ldr x1, [x1, #16]
subs x2, x2, #2
b.vc loop
```

The rewrite retains the function frame and both exception routes. It uses the
known valid list representation (CONS low bits 00, NIL 11) and tagged index
representation. It is a function-specific diagnostic, not a general licence
to discard scratch-register values or flags. The original X64 code already
uses direct field destinations and immediate comparisons, although its loop
also contains avoidable work.

The resulting approximately 5% whole-program improvement isolates a material
cost in this one function. Historical instrumentation counted 27 million
`count` entries before self-recursion became a loop, explaining why this small
body is worth improving. It is not the current function-call count.

Recommendation: generate immediate comparisons/decrements, direct field-load
destinations and list bit tests directly where representation and operand
information are known. A general peephole implementation needs register and
NZCV liveness before deleting temporary definitions or changing flag effects.

## Shared cleanup and the unexplained remainder

The local peephole trial removes round-trip copies, folds scratch-address and
ALU copies, simplifies known-zero ORs, forms adjacent load/store pairs in both
address orders, and threads jump-only blocks. It preserves labels and GC
metadata. The production pass currently has a smaller window of rules and
pairs only ascending adjacent stack accesses. Extending it is a concrete,
measured next step; no scheduling changes were included here.

GC does not explain the gap. In batch 2, median collector CPU time is 6.3 ms
ARM64 versus 8.1 ms X64 for mlyacc, and 4.0 versus 4.8 ms for professor.
Collection counts remain 28 and 263. GC CPU time and wall time are distinct
metrics, but collector cost is small and is actually lower on ARM64.

The allocating recursive `findSol` body in professor still has a 416-byte
local frame and substantial stack traffic; mlyacc union still spills around
comparisons. These remain investigation targets, not quantified explanations
of the residual gap. The previous blanket region-helper register saves and
per-iteration `count` frame setup have already been addressed. No new PMU or
usable sampling data were obtained, so cache misses, branch misprediction and
pipeline stalls are not established causes.

## Reproduction and scope

All compiler builds use MLKit with GC, never MLton. ARM64 uses `ad5a7a8` and
the freshly rebuilt `M12FreshFrames` Basis. X64 timings use the unchanged
milestone-11 GC binaries. X64 assembly was regenerated separately with the
existing MLKit X64 compiler for inspection, not substituted into timing runs.

All 120 warmup/measured executions across the two batches matched expected
stdout. Initial untimed checks also matched for all variants. This validates
the experiments on these inputs, not their general production correctness.
Compiler, assembler and instrumentation work did not run during timing.

The [supporting directory](arm64-performance/residual-slowdown/) contains both
timing batches, executable hashes, rewrite counts, assembly excerpts and the
experiment scripts. `relink.py`/`link.py` capture the unmodified link commands;
`experiments.py` builds the peephole/leaf/inline variants; `count-loop.py`
builds the indexing variant; `measure.py` runs output-checked measurements.
Paths refer to this investigation's local build caches and must be adapted
for another checkout. Cache objects were not overwritten by the experiments.
