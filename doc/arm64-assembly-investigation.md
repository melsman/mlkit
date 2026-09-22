# ARM64 assembly and residual slowdown investigation

Investigation at `07cf1e8`, concentrating on mlyacc and professor. The best
measured opportunities are unnecessary register preservation and frame work,
followed by conservative peephole cleanup. A simple load-rescheduling trial
does not improve these benchmarks. No production compiler or runtime code is
changed by this investigation.

## Measurements

The earlier runs reused an older Basis. Rebuilding the entire GC Basis with
the current MLKit compiler reduces both runtimes by about 4–5%, without any
new backend change. Freshly built benchmark units and Basis then form the
baseline for isolated assembly experiments. All figures below are median
wall-clock seconds, with GC enabled, on the same M2 Max.

| Configuration | mlyacc | professor |
| --- | ---: | ---: |
| X64 / Rosetta 2 | 0.2040 | 0.2525 |
| Current ARM64, older Basis | 0.3053 | 0.3741 |
| Current ARM64, rebuilt Basis | 0.2919 | 0.3573 |
| Rebuilt Basis + peephole trial | 0.2774 | 0.3517 |
| Rebuilt Basis + load-rescheduling trial | 0.2930 | 0.3579 |
| Rebuilt Basis + omit unused FP saves at region calls | — | 0.3308 |

The isolated peephole changes improve mlyacc by 5.0% and professor by 1.6%.
Omitting unnecessary FP preservation improves professor by 7.4%. Scheduling
changes are within the observed timing spread and show no clear benefit.
Rebuilt ARM64 is still about 43% slower than X64 for mlyacc and 42% slower
for professor; the experiments explain portions of that gap, not all of it.

Two subsequent, separately interleaved experiments isolate frame overhead:

| Experiment | Contemporary ARM64 baseline | Experimental | Change |
| --- | ---: | ---: | ---: |
| mlyacc: simplify identity tail wrappers | 0.2858 | 0.2713 | -5.1% |
| professor: reuse `count` frame on self-tail recursion | 0.3523 | 0.3391 | -3.7% |

These effects must not be added together: combined variants were not measured.
The different baselines reflect separate measurement batches.

GC CPU time with the rebuilt Basis is only 0.0065 seconds for mlyacc and
0.0048 seconds for professor. Collection counts remain 28 and 263 throughout.
The residual slowdown is overwhelmingly outside collection.

## What the hot code does

Fresh sampling attempts failed with `sample_remote_thread_with_frame_filter
failed to get thread state`; empty profiles were discarded. Earlier successful
profiles identified set union and professor's search/list traversal as targets.
For this investigation, separate untimed executables count selected function
entries directly. Their outputs match the benchmarks; instrumented binaries
are not used for timing.

### mlyacc: small operations repeated millions of times

The dominant set-union instance runs 4,811,120 times. Its greater-than wrapper
runs 4,511,040 times and its equality wrapper 2,409,740 times. Together the two
wrappers execute 6,920,780 times. The [actual wrapper](arm64-performance/assembly-investigation/mlyacc-wrappers.s.txt)
performs this sequence before reaching the comparator:

```asm
stp x29, x30, [sp, #0]
add x29, sp, #0
sub sp, sp, #16
ldp x29, x30, [sp, #16]
add sp, sp, #16
b   _F.gtTerm5_...
```

For this identity tail wrapper, the first five instructions leave the incoming
registers and SP unchanged. The callee writes the same header. Keeping the
branch and removing that work from 46 matching wrappers produces the measured
5.1% improvement. These two wrappers alone account for about 34.6 million
redundant dynamic instructions. The corresponding X64 wrapper has two stack
address adjustments and a jump, without the frame save/reload.

The [union body](arm64-performance/assembly-investigation/mlyacc-union.s.txt)
also spills list heads/tails across comparator calls, then reloads them and
constructs the output list. ARM64 repeatedly routes field addresses through
`x16`. X64 can express several corresponding accesses directly with its base
register and uses immediate comparisons. This is instruction-count and
dependency overhead in very frequently executed paths; cache misses have not
been measured and should not be inferred merely from the spills.

### professor: frame churn, list operations and region helpers

Entry counts are 4,318,580 for `findSol`, 27,023,610 for `count` (list indexing),
and 6,995,460 for Basis list append. The previously prominent symbol `_F._17`
is list append, not integer division. Updating the old Basis therefore matters.

The [list-indexing function](arm64-performance/assembly-investigation/professor-count.s.txt)
is non-allocating and now has no GC poll, but every recursive iteration still
saves/restores its frame header. The diagnostic loop variant retains the entry
header once and redirects self-tail calls to a label after that setup. It
removes five executed instructions per recursive iteration, preserves argument
updates and overflow handling, and produces the 3.7% improvement above.

The [search assembly](arm64-performance/assembly-investigation/professor-search.s.txt)
contains region helpers that reserve 304 bytes and save 16 integer plus 22 FP
registers. The professor ML unit performs no FP calculations; these FP saves
are generic preservation, not live real values. Removing only FP save/restore
instructions at allocation/deallocation calls removes 22 instructions and
352 bytes of stack traffic per affected call. Stack reservation and integer
saves remain unchanged. There are 166 matching static sites in the whole unit;
the measured improvement is 7.4%.

Professor also constructs debugging strings even with `debug_flag = false`:
the strict argument expression is evaluated before `debug` returns. Assembly
still calls `int_to_string` and `concatStringML` in those paths. This explains
why string/region machinery appears in a puzzle-search benchmark. The benchmark
was not rewritten to remove this work. A compiler must consider effects and
exceptions before eliminating those argument expressions.

## Peephole opportunities and literature

The existing `InstsArm64.optimise` already handles self-moves, nearby branches,
adjacent stack store/load forwarding, and ascending adjacent stack pairs.
The experimental extension adds register-preserving local rewrites such as:

```asm
// Fold a scratch copy into a field load.
mov x16, x1                 // becomes: ldr x16, [x1, #8]
ldr x16, [x16, #8]

// Pair descending stack loads, with registers in address order.
ldr x1, [sp, #24]           // becomes: ldp x0, x1, [sp, #16]
ldr x0, [sp, #16]
```

It also folds a scratch copy into an immediate ALU operation, removes a
redundant round-trip copy or OR with known zero, and redirects direct branches
through blocks containing only labels and another branch. Labels, GC data and
exception continuations are retained. Across changed mlyacc units it removes
18,771 of 275,075 static instructions (6.8%); in professor it removes 770 of
22,364 (3.4%). These counts do not include unchanged Basis/runtime code.

LLVM's AArch64 load/store optimiser runs after register allocation and includes
pair formation, store-to-load forwarding and address/update folding. It uses
register effects and bounded searches rather than matching arbitrary distant
text. That is a useful implementation model for extending MLKit's pass.
[LLVM source](https://llvm.org/doxygen/AArch64LoadStoreOptimizer_8cpp_source.html)

Other promising, unmeasured rules are immediate comparisons instead of
materialising constants in `x17`, and `CBZ`/`TST`/bit branches for suitable tag
tests. Deleting the temporary definition or replacing flag-setting operations
requires liveness checks for both registers and NZCV. The measured trial
deliberately limits itself to simpler rewrites.

## Scheduling: where it may help, and where it did not

The trial moves a stack load one instruction earlier across an independent
integer move/ALU operation. It preserves all registers and never crosses SP
updates, calls, branches, labels, flags, or heap accesses. It changes 2,390
positions in mlyacc and 170 in professor, with no clear speedup. This is a
limited scheduling experiment, not evidence that every scheduler is useless.

Apple documents out-of-order execution on its CPUs: hardware can already
execute independent instructions ahead of earlier work. Removing instructions
and dependency chains is consequently a stronger starting point here than
blindly increasing load/use distance. Apple's detailed CPU guide requires
developer-account access and an additional agreement; it was not used as
evidence for specific M2 cycle counts.
[Apple overview](https://developer.apple.com/documentation/xcode/addressing-cpu-bottlenecks),
[guide access](https://developer.apple.com/documentation/apple-silicon/cpu-optimization-guide)

Arm's Cortex-A77 guide recommends selected paired memory sequences and
documents adjacent compare/test-plus-branch fusion. Its throughput tables also
show why a pair is not automatically twice as fast as two scalar accesses.
Those are useful principles, but Cortex-A77 latencies and bandwidth are not M2
measurements. LLVM likewise models fusion as a target-dependent adjacency
constraint. A scheduler should preserve profitable compare/branch and address
pairs, not insert unrelated work between them.
[Arm guide, sections 3.9–3.10 and 4.4–4.14](https://documentation-service.arm.com/static/64adbc3e38511951cb79e6d4),
[LLVM macro-fusion logic](https://llvm.org/doxygen/AArch64MacroFusion_8cpp_source.html)

If scheduling is added, start with bounded, dependency-aware movement that
enables useful load/store pairs. It needs explicit register, NZCV and memory
effects; unknown operations should stop the search. Keep it before branch
relaxation, with hard barriers at calls, GC/exception boundaries, metadata
directives and SP changes. General latency scheduling should wait for stronger
hardware evidence. The installed command-line tools do not provide
`llvm-mca`, and `xctrace` requires a full Xcode installation here; no PMU
cache-miss, branch-misprediction or stall measurements were collected.

## Recommended implementation order

1. Propagate live-register information to region helpers, particularly FP
   liveness, instead of preserving every possible register.
2. Eliminate redundant identity-tail-wrapper frames; add a safe loop entry for
   eligible self-tail recursion. Preserve GC checks in allocating loops and
   account for stack arguments, regions and handlers. LLVM's tail-recursion
   pass provides an established example of converting recursion into a loop,
   with explicit attention to frame/stack-object legality.
   [LLVM tail-recursion pass](https://llvm.org/doxygen/TailRecursionElimination_8cpp_source.html)
3. Extend conservative copy/address/zero cleanup, branch threading and pair
   formation, then separately evaluate immediate/bit-test rules.
4. Evaluate pairing-oriented scheduling before undertaking a general scheduler.

There is still unexplained overhead after these isolated improvements.
In particular, return prediction remains a hypothesis: GC-enabled X64 also
constructs return addresses and jumps, so the ARM64 `ADR`/`B` sequence alone
does not establish an ARM-specific disadvantage. A call-ABI experiment or
hardware branch measurements are needed to attribute that cost.

## Reproduction and limits

All builds use MLKit, never MLton. The compiler is the MLKit `-gc` host build
from `07cf1e8`; `M12Study` is a fresh Basis cache. X64 uses the existing milestone
11 binaries. The assembly variants reassemble copies of benchmark units and
relink with identical fresh Basis and runtime objects. Shared cache objects
are not overwritten. None of the candidate passes is installed in the compiler.

Each timing batch uses one warmup and five measured executions per variant,
with deterministic shuffled order. All 90 timing/warmup executions and both
untimed counter executions match expected output. The timing batches are
separate from compilation, sampling and instrumentation. Output equivalence
on these two programs is diagnostic validation, not general correctness
validation for production transformations. In particular, the FP experiment
depends on professor's absence of live FP values.

[Primary timings](arm64-performance/assembly-investigation/measurements.json),
[wrapper timings](arm64-performance/assembly-investigation/wrapper-measurements.json),
[loop timings](arm64-performance/assembly-investigation/loop-measurements.json),
[call counts](arm64-performance/assembly-investigation/call-counts.json), and
[static rewrite counts](arm64-performance/assembly-investigation/experiments.json)
are retained with executable/output hashes. The neighbouring scripts and
captured link commands record the experiment, with local paths that must be
adapted for another checkout. Assembly excerpts have a
[source/hash manifest](arm64-performance/assembly-investigation/assembly-sources.json).
