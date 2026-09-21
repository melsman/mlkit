# ARM64 optimisation opportunities

Initial investigation for milestone 12 of issue #223 / PR #224, at `5791e21`.
This records the initial design and static-code review. See the
[implementation and small benchmark check](arm64-optimisation-results.md) for
the subsequent register-preservation and peephole changes. Instruction
scheduling is deferred.

The most promising first steps are to reduce redundant register preservation,
shorten constant materialisation, and avoid unnecessary branch expansion.
A small peephole pass is feasible. An instruction scheduler needs explicit
instruction-effect information first and should follow the simpler changes.
The [milestone 11 timings](arm64-runtime-performance.md) provide a baseline;
they do not establish which mechanisms cause the observed gaps.

## 1. Calling C and runtime helpers

There are three distinct cases in
[CodeGenArm64](../src/Compiler/Backend/Arm64/CodeGenArm64.sml):

| Call category | Current handling | Opportunity |
|---|---|---|
| Explicit `CCALL` / `CCALL_AUTO` | Register allocation and `FetchAndFlush` see the call; `scalarCallInto` stages arguments through stack slots. | Honour C-preserved registers where safe; later replace unnecessary argument staging with parallel moves. |
| Region entry/exit | `LETREGION` already informs register allocation and spill insertion, but `internalCallInto` also saves a blanket register set. | Use the existing spill contract rather than preserving the same values again. |
| Record allocation / reset slow paths | Calls are introduced by code generation after register allocation; preserving stubs keep the fast path free of spills. | First save only ABI-clobbered registers; later preserve only the live subset on the slow edge. |

The blanket `savedRegs` set has 54 registers: `x0–x15`, `x19–x26`, and `d0–d29`.
These are 52 allocatable registers plus the two FP spill temporaries `d28/d29`.
Each inline internal call emits 54 stores and 54 loads, using 432 stack bytes.
Each shared allocation/reset stub also saves LR, for 110 memory instructions
and a 448-byte aligned frame.

### Smaller preserving stubs

AAPCS64 already requires C callees to preserve `x19–x28` and the low 64 bits of
`v8–v15`. Thus `x19–x26` and `d8–d15` need not be saved again around a helper
that obeys that ABI. See the
[Arm procedure-call standard](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
and [Apple's ABI differences](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms).
The generated `mlkit_arm64_alloc` wrapper already preserves the C-preserved
registers that it uses internally.

Reducing the set from 54 to 38 saves 16 stores and 16 loads per call. Including
LR, a preserving stub would use 78 memory instructions and 320 stack bytes.
This requires no new liveness analysis. The X64 `ccall_stub` already follows
this approach: it relies on the C callee to preserve its callee-saved registers.
Pairing eligible stack accesses with `stp`/`ldp` can further reduce instruction
count, but does not reduce the amount of memory transferred.

Use this only for audited helpers that cannot collect or re-enter ML. Actual
GC entry is separate: `entryGCInto` creates a 352-byte register/argument image
used by the collector. Moving collection can update saved roots, so C's ordinary
callee-save rules do not justify dropping those root slots or their reloads.
Keep the foreign-call callback/GC-deferral protocol intact as well.

### Let the compiler spill around calls

The proposed scheme is possible, and partly exists already:

- [RegAlloc](../src/Compiler/Backend/RegAlloc.sml), in `Build` and `AssignColors`,
  classifies live ranges across C calls and prefers the C-preserved GPR palette.
  It also accounts for C calls at infinite-region entry and exit.
- [FetchAndFlush](../src/Compiler/Backend/FetchAndFlush.sml) computes the live
  variables needing stack homes and reloads across explicit C calls and
  `LETREGION` boundaries.
- Both current ARM64 and X64 `is_callee_save_ccall` predicates return `false`,
  keeping spill insertion conservative even for C-preserved GPRs. FP live
  ranges across C calls have an empty preserved-register palette in `RegAlloc`.

Start with region entry/exit, where the IR already describes the clobbering
points. Verify the existing flush/fetch contract, argument setup, and region
operands, then remove redundant blanket saves for those calls. Next make
ARM64's preservation predicate accurately recognise usable C-preserved GPRs.
Supporting `d8–d15` requires extending the shared allocator's FP palette,
not just changing that predicate. Keep reserved context/exception registers
outside the allocation palette.

For allocation/reset calls introduced late, prefer **spills only on the slow
edge**. Carry the live-across register set to code generation, save its
intersection with the helper's clobber set in the out-of-line block, call the
helper, reload, and rejoin the fast path. Preserve operands needed after the
allocation as well as ordinary live-out values. A physical-register analysis
must include the FP spill temporaries and implicit scratch-register uses.
Shared stubs specialised by live-register mask are another option, but may
increase code size as masks multiply.

Marking every allocation as an unconditional C clobber before register
allocation would be simpler, but can force spills or constrained register
choices on every successful inline allocation. That undermines the fast path.
For calls that are unconditional already, allocator-visible clobbers are a good
fit. For conditional slow calls, use edge-specific preservation and measure
whether its smaller save set outweighs additional out-of-line code.

Argument staging is a separate opportunity: `scalarCallInto` stores every
argument, then reloads ABI locations. A parallel-move resolver could use direct
moves for acyclic assignments and scratch space only for cycles, stack arguments,
and required conversions. This needs Darwin variadic/narrow-argument and callback
checks; it is not a prerequisite for reducing allocation-helper saves.

## 2. Peephole optimisation and scheduling

[X64 `InstsX64.optimise`](../src/Compiler/Backend/X64/InstsX64.sml) applies
`elim_jmp_jmp`, `peep`, and label alignment to function bodies. Its rules remove
self-moves, redundant constants, some store/load pairs, branches to the next
label, and dead instructions; they also invert branch-over-branch sequences
and remove unused labels. `CodeGenX64.CG` invokes the pass. There is no general
instruction scheduler in this X64 pipeline to port.

ARM64 currently has no corresponding pass. Its instruction representation is
`Label | Directive of string | Op of string * string list`; register and memory
effects are not explicitly represented. `moveInto` already suppresses direct
self-moves, so merely copying X64's self-move rule is unlikely to help much.

A practical first pass would handle:

1. **Constants.** `constantInto` always emits one `movz` and three `movk`
   instructions. Omit writes of already-zero upper chunks; subsequently choose
   a shorter `movz`/`movn` seed and only the necessary `movk` patches. This can
   be fixed directly in the builder without waiting for a general optimiser.
2. **Branches.** Remove branches to the immediately following label and simplify
   conditional-branch/unconditional-branch patterns. `InstsArm64.emit` currently
   expands every conditional branch into an inverted condition plus `b`, even
   when the original target is near. Add layout-aware range checking and widen
   only out-of-range branches, accounting for alignment and iterating to a
   stable layout. Retain a conservative fallback for unknown layout directives.
3. **Memory and moves.** Forward an adjacent store to a matching load where
   widths, aliases, and address dependencies allow it; combine eligible adjacent
   stack loads/stores into `ldp`/`stp`. Avoid transformations of unknown/volatile
   memory. Register-copy propagation needs liveness when deleting the temporary
   copy; a use of `wN` also changes `xN` through zero extension.

Place semantic peephole cleanup before branch layout/emission. Apply it to
ordinary function code, init/link code, and executable slow blocks currently
mixed into `staticChunks`, without treating data directives as instructions.
Preserve GC frame/return labels, exception continuations, address-taken labels,
relocations, and function boundaries. ARM64 metadata embeds label references in
strings, so full X64-style label coalescing/deletion needs a reliable inventory
of those references first. Branch-to-next-label removal can retain the label.

### Static evidence from nucleic

The retained milestone 10 GC assembly for the short `test/nucleic.mlb` test
contains 102,765 instructions in its two program units, excluding Basis and
link code. The current compiler source is unchanged since that assembly was
validated; this is not the larger milestone 11 paper workload.

| Syntactic opportunity | Count |
|---|---:|
| Four-instruction constant sequences | 1,679 |
| Zero upper-halfword `movk` instructions after `movz` | 3,877 |
| Unconditionally expanded conditional branches | 2,356 |
| Unconditional branches to the next label | 32 |

Omitting those zero `movk` writes alone would remove about 3.8% of these static
instructions. The branch count is a set of candidates, not a count of branches
proved to be in range. None of these counts predicts dynamic speedup.
[Counts and assembly hashes](arm64-performance/optimisation-opportunities/nucleic.json)
were produced with the [inspection script](arm64-performance/optimisation-opportunities/inspect.py):

```sh
python3 doc/arm64-performance/optimisation-opportunities/inspect.py \
  /path/to/retained-nucleic-assembly
```

### A small scheduler

Introduce typed operands or a validated semantic view of supported operations:
register reads/writes (including overlapping X/W and FP views), NZCV flags,
memory effects, stack changes, potentially trapping operations, FPCR/FPSR,
and control-flow effects. Unknown operations and
directives must be barriers.

Then try a bounded, basic-block-local scheduler that moves independent arithmetic
between a load and its consumer. Honour RAW, WAR, and WAW dependencies; initially
retain memory-operation order and forbid motion across calls, branches, labels,
GC points, exception transitions, and stack adjustments. Run it after allocation
and peephole cleanup but before final branch layout. It should be optional for
A/B measurements: a different instruction order can help, do nothing, or hurt,
and should not be retained on intuition alone.

## Suggested implementation order

First narrow the preserving save set and shorten constants. Then remove the
redundant region-entry/exit preservation and enable safe use of C-preserved
registers. Add conservative peephole rules and selective branch expansion next.
Evaluate live-register masks for cold allocation edges, then a small scheduler
once instruction effects are available.

For each isolated change, begin with output-checked nucleic GC/no-GC runs and
static instruction counts. Check C-call/FP preservation, GC roots, spills,
callbacks, and branch-range stress as appropriate to the change. Compare the
milestone 11 suite after promising results; retain compile-time checks so the
extra passes do not undo the code-construction improvements. Build host
compilers with MLKit `-gc` throughout.
