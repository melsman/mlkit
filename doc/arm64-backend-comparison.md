# ARM64 and X64 code-generation comparison

Source review at `88297c5`, after milestone 12's JumpTables implementation.
This is an investigation, with a static check of the existing generated
`nucleic.sml.s`; no compiler changes or new runtime measurements were made.

Items 1 and 2 are subsequently implemented in the
[call and operand optimisation](arm64-operand-results.md). Items 3, 4 and 5
are addressed by the [record, branch and GC optimisation](arm64-layout-results.md).

For the current priorities after the return fix, see
[the follow-up investigation](arm64-next-opportunities.md). The analysis below
describes the earlier revision.

The strongest remaining opportunities are to use the register allocator's
results directly and to reduce unnecessary stack traffic. ARM64 now shares
the broad strategies of X64: code-suffix construction, register allocation,
inline allocation/reset fast paths with shared allocation helpers,
return-address-relative GC descriptors, and the shared JumpTables selection
algorithm. Their absence should no longer be used to explain performance.

1. **Remove redundant ML argument/result staging — highest priority.**

   [RegAlloc](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/RegAlloc.sml#L83) resolves the calling
   convention before allocation, inserting assignments to argument registers
   and from result registers. [X64 calls](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/CodeGenX64.sml#L550)
   subsequently move only the stack-passed arguments/results.
   [ARM64 calls](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L117)
   nevertheless stage all arguments, including register arguments, on the
   stack and reload them. Register results similarly go through a temporary
   stack area before the already-inserted result assignments. Tail calls also
   stage everything.

   Keep register arguments/results in place and handle only actual stack
   arguments/results. Preserve overlap-safe stack copying for tail calls and
   the indirect closure target. A general parallel-move resolver is useful
   for unresolved helper/C calls, but ordinary resolved ML calls should not
   require a second full register shuffle. This reduces work at every affected
   call without changing the number of argument registers or the GC ABI.

2. **Use physical operands/destinations directly — highest priority.**

   [ARM64 primitives](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L534)
   routinely copy both operands into x16/x17 or d30/d31, perform the operation,
   and copy the result out. Even [ordinary assignments](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L1469)
   go through x16; FP-to-FP assignments take a round trip through an integer
   register. [X64's FP lowering](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/CodeGenUtilX64.sml#L1318)
   resolves physical operands and destinations directly and introduces
   temporaries only where needed.

   An actual sequence in the current nucleic assembly is:

   ```asm
   fmov d30, d14
   fmov d31, d16
   fadd d30, d30, d31
   fmov d14, d30
   ```

   The scalar arithmetic can instead be `fadd d14, d14, d16`. Use direct
   register moves for assignments and operand-resolution helpers for arithmetic,
   loading spills only when necessary. This makes use of ARM64's three-register
   arithmetic, rather than adding scratch copies around it. Check FP upper-lane
   semantics when extending copy optimisations; the current ML values are
   scalar 64-bit values, not arbitrary vectors.

3. **Keep record destinations in registers — high priority.**

   [ARM64 record construction](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L481)
   reserves 16 stack bytes, saves the allocated pointer, reloads it for every
   header/field store, then reloads it again for the result. This protects it
   from address-materialisation helpers that use both scratch registers.
   [X64](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/CodeGenX64.sml#L187) resolves a result
   register and stores fields through that register.

   Add a register-resident construction path for common cases, with explicit
   scratch-clobber contracts and a fallback for conflicting operands or large
   offsets. Avoid overwriting a result register if it still holds a source
   field. This opportunity remains even though allocation itself is now inline.

4. **Expand only out-of-range conditional branches — high priority.**

   [ARM64 emission](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/InstsArm64.sml#L99)
   unconditionally replaces every supported conditional branch with an inverted
   test and unconditional branch. This solves large generated parser functions,
   but nearby branches pay the same code-size cost and some paths execute two
   branches. The expansion happens after the peephole pass.

   Add range-aware relaxation, accounting for instruction sizes, alignment,
   inline descriptors and jump-table data. Keep direct conditional branches
   where they fit and expand the rest. This requires correct layout handling;
   simply removing expansion would reintroduce known long-branch failures.

5. **Share GC slow-path code and investigate GC call overhead.**

   [X64's entry check](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/CodeGenUtilX64.sml#L291)
   branches to out-of-line setup and a shared GC stub.
   [ARM64](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L1851) emits the
   352-byte snapshot save/restore sequence separately in each function, behind
   checks of both `disable_gc` and `time_to_gc`. A shared snapshot stub, with
   small per-site metadata setup, would reduce duplicated cold code. A compact
   polling protocol is another opportunity, provided foreign-callback GC
   deferral remains correct. ARM64 also materialises addresses for polling
   and allocation-accounting globals; some load/store overhead is architectural,
   but repeatedly computing these addresses is worth examining.

   Both backends use explicit continuations for GC-enabled ML calls: X64
   pushes the return label and jumps; ARM64 materialises x30 and uses `b`/`br`.
   Thus it would be misleading to say that X64 always uses hardware `call`
   while ARM64 does not. No-GC ARM64 calls already use `bl`/`blr`.

   The [inline-descriptor experiment](arm64-inline-gc-results.md) recorded
   +2.2%, +18.4%, and +11.8% runtime changes for nucleic, mlyacc, and professor.
   These are measurements of that change, not a current X64/ARM64 comparison
   or proof of a predictor effect. They justify a focused experiment on call
   layout/address setup while retaining static PC-relative descriptors.
   Recovering `bl`/`blr` would require a carefully designed descriptor layout;
   it is not a drop-in opcode replacement.

6. **Finish control-flow cleanup and instruction selection.**

   [X64 optimisation](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/InstsX64.sml#L901) redirects
   jump chains, coalesces labels, removes dead code and unused labels, and
   aligns eligible labels. ARM64 has a useful local peephole pass, but not
   these broader control-flow transformations. Its [JumpTables integration](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L1842)
   also passes `NONE` for continuation inlining, whereas
   [X64](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/X64/CodeGenUtilX64.sml#L632) recognises small
   jump/return continuations. Port these ideas with explicit protection for
   descriptor anchors, exception continuations and table references. Do not
   copy X64's label-alignment policy without measuring it on ARM64.

   [ARM64 constant/address helpers](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenUtilArm64.sml#L33)
   still have limited instruction selection: constants start with a low-halfword
   `movz`, arithmetic/comparisons often materialise constants in registers,
   and negative memory offsets use address-building fallbacks. Candidates are
   better `movz`/`movn` seeds, immediate arithmetic/comparisons, logical
   immediates, and suitable unscaled or indexed loads/stores. For example,
   all-ones currently needs four constant-building instructions; a `movn`
   form can express it in one. Preserve tagging, overflow and width semantics.

7. **Refine preservation after eliminating the avoidable traffic above.**

   ARM64 currently offers 24 allocatable GPRs and 28 allocatable FP registers,
   versus X64's five ordinary ML caller-save GPRs and 12 allocatable FP
   registers; X64 additionally has its C-preserved GPR allocation palette.
   ARM64 also passes eight integer arguments in registers versus three for
   X64. More registers are useful, but they make blanket helper preservation
   more expensive. [ARM64 helpers](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/Arm64/CodeGenArm64.sml#L201)
   still save 38 C-clobbered registers, regardless of the individual call's
   live set. Pair instructions reduce instruction count, not bytes saved.

   Calls already represented as clobbering points in the IR, such as region
   creation, should exploit existing allocation/flush information. Late
   allocation/reset calls need explicit live sets or specialised preserving
   stubs; moving every spill onto the successful allocation path would lose
   the benefit of the current design. Separately,
   [RegAlloc](https://github.com/melsman/mlkit/blob/88297c5/src/Compiler/Backend/RegAlloc.sml#L859) uses an empty C-preserved
   FP palette, despite ARM64's d8-d15 scalar preservation contract. Supporting
   that palette requires coordinated allocator and fetch/flush changes.

   Every ARM64 function also saves/restores x29/x30 and sets up x29. Leaf
   frame simplification is a later opportunity, especially without GC, but
   must respect use of x30 as scratch by allocation/reset and the frame
   contracts for GC, exceptions, callbacks and profiling.

The static nucleic check used the existing JumpTables benchmark artifact at
`/private/tmp/mlkit-m12-jump/artifact/nucleic/MLB/ARM64_RI_GC_M12Inline/nucleic.sml.s`.
That source unit contains 93,688 instruction lines, 18,803 `mov` instructions,
4,114 `fmov` instructions, 635 adjacent four-instruction scratch FP arithmetic
sequences, 2,330 expanded conditional sites, and 54 inline calls to `_gc`.
These are static counts for one unit, excluding the Basis and other program
units. They establish that the patterns occur in generated code; they do not
measure execution frequency or predict a runtime speedup.

Recommended order: eliminate resolved ML-call staging; introduce direct
assignment/arithmetic operands and register-resident record construction;
then implement selective branch expansion and shared GC slow paths. Measure
each change on the same small benchmark set. The older 20-benchmark
[runtime comparison](arm64-runtime-performance.md) predates milestone 12 and
should not be presented as the current performance gap. Instruction scheduling
remains deferred; X64 has no general scheduler to port, and unnecessary
instructions should be removed before considering one.
