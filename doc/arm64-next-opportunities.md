# ARM64 opportunities after the GC return fix

Review at `a6d7fb9`, using the freshly compiled `M12RetAfter` Basis and benchmark
assembly. The return-prediction issue is fixed; older measurements of tiny-call
optimisations should not be treated as current expected gains. No production
compiler or runtime change is made by this investigation.

## Small experiments on the current compiler

The same mlyacc benchmark, GC enabled, on the M2 Max. Each of two batches has
one warmup and five measured runs per variant, deterministically shuffled.
Only copies of benchmark assembly change; Basis and runtime objects are identical.
All 60 executions match expected stdout and perform 28 collections. No builds
run during timing. These measurements are comparisons within each batch, not
updated X64 comparisons.

| Variant | Batch 1 seconds | Batch 2 seconds |
| --- | ---: | ---: |
| Current compiler | 0.15468 | 0.15504 |
| Omit eligible tiny-leaf frame saves | 0.15284 | 0.15591 |
| Inline eligible tiny leaves | 0.14906 | 0.15137 |
| Inline eligible comparisons only | 0.14995 | 0.15163 |
| Shorter constant construction | 0.15389 | 0.15430 |

The inline experiments change 199 call sites, or 111 for comparisons only.
They preserve caller spills and inline GC metadata. All-leaf inlining improves
runtime by about 2.4–3.6%; comparison-only inlining by 2.2–3.1%. This remains an
opportunity, but the old roughly 10% gain is no longer representative after the
return fix. Removing 37 leaf-frame headers has no consistent benefit.

The constant experiment chooses a shifted MOVZ or MOVN seed and subsequent
MOVKs. It shortens 96 sites by 146 instructions in benchmark units, but its
roughly 0.5% elapsed difference is not supported by a CPU-time reduction.
Treat it as a code-size improvement with unestablished runtime benefit.
These isolated gains must not be added together.

## Recommended order

1. **Improve direct operand selection and eliminate dead temporaries.**
   The current peephole pass deliberately keeps register/flag effects unchanged.
   It therefore cannot generally delete a scratch copy before a comparison,
   remove a dead constant definition, or propagate a value through several
   instructions. Prefer direct operands in lowering where already available;
   add instruction use/def information and backward register/NZCV liveness for
   additional local cleanup. The typed instruction representation makes this
   practical. Start within basic blocks, conservatively treating calls, opaque
   directives and metadata boundaries. Do not reuse `liveStmts` as though it
   described emitted instructions: it operates on the higher-level statements.

   A static scan of current benchmark units finds 511 adjacent scratch-copy/
   comparison sequences in mlyacc and 92 in professor. It also finds 2,269 and
   120 definitions of zero in x16/x17 respectively; some are useful, so these
   are candidates for analysis, not counts of removable instructions. This is
   the most attractive bounded next step, but its runtime benefit is unmeasured.

2. **Reduce spills around known small calls.**
   All allocatable integer registers are currently ML caller-save in
   `InstsArm64.RI.caller_save_phregs`. mlyacc's hot union still has a 64-byte
   local frame and repeatedly stores/reloads list heads and tails around tiny
   comparisons and recursive calls. A pre-register-allocation clobber summary
   for known callees could avoid some of these spills; unknown and indirect
   calls would retain the conservative convention. Summaries must account for
   GC, native helpers, tail transfers and transitive calls. Changing only the
   late code generator cannot undo allocator-inserted spills safely.

   Small-function inlining before allocation is another route and can expose
   direct compare/branch generation rather than a tagged Boolean round trip.
   It is shared compiler work and may benefit X64 too. The assembly experiment
   establishes a small residual call opportunity, not the benefit of removing
   spills. Inlining remains deferred under the existing project scope.

3. **Extend constant, immediate and addressing-mode selection.**
   `constantInto` always starts with a low-halfword MOVZ; `smallImmediate`
   accepts only 0..4095; `loadInto`/`storeInto` otherwise build an address when
   the offset is not a nonnegative aligned scaled offset. Add shifted MOVZ/MOVN
   seeds, shifted arithmetic immediates, suitable logical immediates, signed
   unscaled loads/stores, and register-indexed addressing where the IR provides
   an index. Arm's [A64 ISA overview](https://developer.arm.com/-/media/Files/pdf/graphics-and-multimedia/ARMv8_InstructionSetOverview.pdf)
   and [instruction reference](https://documentation-service.arm.com/static/6245c734b059dc5ff9a8bdab)
   document these forms. Extend the typed datatype/printer as needed; do not
   reintroduce instruction-string matching into the compiler.

   Preserve tagged arithmetic and overflow behavior. In particular, rewriting
   flag-setting operations with negated immediates needs a proof for the flags
   actually consumed. The measured constant-only gain is too small to make
   this the leading runtime project, although implementation scope is modest.

4. **Reduce repeated allocation-accounting and polling address work.**
   Allocation fast paths repeatedly materialise `alloc_period` and update it;
   GC entry polls similarly address `time_to_gc`. The benchmark assembly has
   1,082 allocation-accounting sites in nucleic, 2,670 in mlyacc and 99 in
   professor. These are static sites, not executed counts. Investigate cheaper
   access through the existing context pointer or carefully scoped address
   reuse before reserving another register. Moving state into the context is
   coordinated compiler/runtime/ABI work, including GC deferral and foreign
   callbacks. Do not simply omit accounting, hoist a mutable poll value across
   a call, or assume all allocations can share one update. No runtime saving
   is established yet.

5. **Finish control-flow cleanup, preserving metadata references.**
   ARM64 threads branch chains but does not yet have X64's broader dead-code/
   label cleanup. Its JumpTables hook still supplies `fn _ => NONE` for small
   continuation inlining. A typed control-flow pass could remove redundant
   join branches and duplicate short continuations. Treat descriptor anchors,
   exception continuations and jump-table labels as explicit references;
   ordinary branch reachability alone is insufficient. Measure code size and
   runtime before adding alignment changes. This can share use/def and block
   infrastructure with item 1.

Instruction scheduling remains deferred. Likewise, I would not redesign the
now-working GC call layout merely to recover BL/RET, enlarge the register
palette, or prioritise general leaf-frame elimination without new evidence.

[Supporting scripts and measurements](arm64-performance/next-opportunities/)
retain executable hashes, both batches and rewrite counts. Paths refer to local
build caches and need adaptation for another checkout. Diagnostic assembly
rewrites validate only these benchmark inputs, not general compiler correctness.
The static scan excludes Basis code; the nucleic count includes its link unit.
