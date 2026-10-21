# ARM64 atomic allocation fast path: development plan

Status: implemented; validation and measurements are recorded in
`arm64-atomic-allocation-results.md`.

Parallel defaults are aligned across X64 and ARM64: ReML enables `-par -par0`
by default and accepts `-no_par0`; MLKit does not expose `par0` and retains
parallelism off by default. To measure forced protection in ReML, combine
`-no_par0` with `--alloc_protect_always`.

## Goal

Give ordinary protected parallel allocations on ARM64 a generated assembly fast
path comparable to X64's `allocinreg`. Successful allocations should avoid the
full preserving stub and C allocator call. Keep the existing runtime fallback
for contention, page expansion, and large objects.

## Findings

The source review used master at `deffec47`, together with the local X64
`allocinreg` comment/alignment cleanup. That local cleanup is not included here.

- `src/Compiler/Backend/X64/CodeGenX64.sml`, `allocinreg`, clears region status
  bits and checks the region mutex pointer. Regions without a mutex use an
  ordinary bump-pointer path. Protected regions check available page space and
  attempt one `lock cmpxchgq`; failure or insufficient space transfers to
  `__allocate`. Profiling allocation sites use the runtime path.
- `src/Runtime/Region.c`, `allocGen`, already implements the shared atomic
  allocation protocol used by ARM64 as well as X64. Ordinary protected
  allocations use an acquire load and a CAS loop. Page expansion takes the
  mutex, rechecks the pointer, atomically reserves the old page remainder by
  advancing to its boundary, initializes the next page, then publishes its
  allocation pointer with a release store. Thus ARM64 does not currently take a
  mutex for every protected allocation; the missing optimization is the
  generated fast path.
- `src/Compiler/Backend/Arm64/CodeGenArm64.sml`,
  `allocateInRegionInto`, currently sends protected parallel allocation through
  `allocSlowInto`. The shared preserving stubs save 16 integer registers, 22
  floating-point registers, and LR in a 320-byte frame before calling the
  existing helper. Avoiding this fixed cost is the primary expected benefit.
- The private ARM64 allocation ABI passes region/result in `x16` and word count
  in `x17`; `x16`, `x17`, and `x30` are scratch. Incoming ML LR is already saved
  in the ML frame, but a called shared helper must preserve its own return
  address. Scratch-register pressure and fallback argument restoration need an
  explicit design.
- The ARM64 instruction datatype and printer already support `ldaxr` and
  `stlxr`; the backend uses them to increment the exception-name counter.
  These instructions can implement an atomic reservation without requiring LSE
  CAS support. See [Arm's synchronization documentation](https://documentation-service.arm.com/static/68c223238a337a2bc6645c0a?token=).
- Existing ARM64 parallel validation covers pthreads, optional Argobots, inferred
  protection, forced protection, private allocation, callbacks, C allocation,
  and page publication. Its current exclusive-instruction checks also match
  exception-name generation, so allocation-specific coverage must be added.
  The script explicitly rejects parallel GC and generational GC combinations;
  this work should not imply support for those configurations.

## Implementation

A shared assembly fast helper precedes the full preserving fallback. Allocation
sites keep the existing private ABI; the helper saves one working register
and LR in a 16-byte frame. The resolved protocol is described below.

For an ordinary page-sized allocation:

1. Clear region status bits after the existing reset/finite-region handling.
2. Inspect the region mutex pointer using the backend layout information.
   Regions without a mutex may use ordinary bump allocation.
3. For a protected region, acquire-load the allocation pointer with `ldaxr`,
   check space using the existing `a - 1` boundary calculation, and reserve the
   object with `stlxr`.
4. On success return the old allocation pointer, applying the existing tag-free
   adjustment as appropriate. On insufficient space or exclusive-store failure,
   restore the original arguments and transfer to the preserving runtime path.

The helper uses a single exclusive-store attempt, following X64's bounded fast
path. ARM64 exclusive stores can fail without a competing pointer update; the
runtime fallback already retries. Consider a small bounded retry only if
measurements justify it. Keep large objects, profiling, and parallel resets on
existing runtime paths initially.

Acquire ordering must pair with the runtime's release publication of a new
page. An ordinary load/store substitution is insufficient. Review exclusive
monitor cleanup when abandoning a reservation, instruction/register overlap
constraints, aligned stack saves outside the exclusive sequence, and interaction
with the C allocator's sequentially consistent CAS operations.

## Milestones and acceptance criteria

### 1. Specify helper ABI and concurrency protocol

- [x] Choose helper placement, scratch registers, minimal saves, and LR handling.
- [x] Specify protected/unprotected dispatch, original-argument restoration, and
      the handoff to both allocation stubs.
- [x] Review acquire/release ordering and the page-boundary reservation protocol
      against `allocGen`; decide whether an instruction/printer extension for
      exclusive-monitor cleanup is needed.
- [x] Confirm finite regions, at-bottom reset, exact page fills, tag-free values,
      large objects, profiling, and supported parallel option combinations.

Acceptance: document the final instruction sequence and invariants before
changing allocation behavior.

### 2. Implement the generated fast path

- [x] Emit the shared helper and route eligible protected parallel allocations
      through it.
- [x] Preserve ordinary allocation for regions without a mutex and existing
      explicitly unprotected behavior.
- [x] Retain the full preserving fallback on insufficient space or failed
      reservation, without changing runtime layout or allocation semantics.
- [x] Check standalone initialization and incremental REPL helper linkage.

Acceptance: inspect generated assembly to establish that success avoids the
320-byte save frame and C allocation call, and failure preserves all required
registers and arguments.

### 3. Validate correctness and interoperability

- [x] Add allocation-specific emitted-code checks rather than relying on the
      existing exception-counter `ldaxr`/`stlxr` matches.
- [x] Exercise uncontended allocation, exact fills, repeated expansion, large
      objects, finite regions, reset modes, and tag-free pointer adjustment.
- [x] Verify integer/FP register sentinels on successful and fallback paths.
- [x] Stress multiple generated-code allocators sharing one protected region;
      verify disjoint storage and retained contents.
- [x] Mix generated-code and C allocation into the same region to exercise page
      publication and contention across both implementations.
- [x] Run pthread and available Argobots coverage (including multiple execution
      streams), MLKit/ReML, callbacks, protection policies, and REPL linkage.
- [x] Run the relevant native/regression suites and confirm existing profiling
      behavior and unsupported-option guards remain correct.

Acceptance: record commands, configurations, results, and any skipped coverage.
Source inspection alone is not correctness validation.

### 4. Measure and document results

- [x] Compare the baseline and implementation for uncontended allocation,
      shared-region contention, frequent expansion, and a private-region control.
- [x] Measure representative parallel programs with repeated runs, identical
      flags and caches, and verified output.
- [x] Record generated code size and the fast/fallback register-save costs;
      report timing distributions and any regressions.
- [x] Update `doc/arm64-allocation-paths.md` with the final protocol and results.

Acceptance: substantiate the benefit with measurements; do not assume X64-like
speedups or make a lock-free claim for the complete allocator. Expansion and
large-object handling retain mutexes.

## Validation

The results document records executed checks and measurements. The parallel
script tests allocation-specific exclusives, deterministic reservation failure,
all helper working-register sentinels, and mixed generated/C allocation with
pthreads and Argobots. Profiling remains on the existing runtime path.

## Resolved design (milestone 1)

The resolved helper ABI preserves x0 and its return address in a 16-byte
aligned frame; x16 retains the region until reservation succeeds and x17 retains
the original word count throughout. Allocation size is checked before shifting
or entering the exclusive sequence. The maximum ordinary request is 1022 words
in the supported non-GC parallel layout. A mutex-null region uses an ordinary
load/store. x30 is reused for the limit and exclusive-store status after its
return address is saved. Protected allocation uses ldaxr/stlxr without intervening memory
accesses; only register arithmetic and branches separate the pair. On no space,
clrex abandons the reservation. On failed stlxr, the reservation is already
cleared. Both paths restore the small frame, reestablish the infinite-region bit,
and tail-branch to the existing full preserving stub. Successful allocation
reconstructs the old pointer by subtracting the byte size from the reserved new
pointer, with the existing optional -8 adjustment. Scaled add/subtract operands
avoid a separate byte-count register, mirroring X64's scaled LEA strategy.

The acquire load pairs with allocGen's release page publication. An allocator
that races the runtime's boundary-reservation CAS cannot successfully store a
stale pointer: its exclusive store fails. The page-boundary test is algebraically
the existing (a-1+bytes) <= ((a-1)|8191) test, expressed as
(a+bytes) <= (((a-1)|8191)+1), and handles exact fills and a already at the end.
No new runtime layout or concurrent-reset guarantee is introduced. Profiling,
large objects, finite-region handling and reset semantics retain their existing
paths; unsupported parallel GC/tagged combinations remain rejected. The new
clrex instruction is represented in the shared datatype, signature, and printer.

The source's current `savedRegs` palette is 16 integer registers and 22
floating-point registers plus LR: a 320-byte preserving frame. The earlier
allocation-path document described an older 448-byte frame. The fast helper
uses 16 bytes on success; fallback restores that frame before entering the
320-byte stub, whose C-ABI wrapper has its own 48-byte frame.

## Comparison with X64

| Detail | X64 `allocinreg` | ARM64 allocation helper |
| --- | --- | --- |
| Region and word-count arguments | `r11`, `r10` | `x16`, `x17` |
| Unprotected explicit saves | Region and size: 16 bytes | `x0` and LR: 16 bytes |
| Protected explicit saves | Region, size, and `rax`: 24 bytes | `x0` and LR: 16 bytes |
| Return address | Additional 8 bytes pushed by `call` | Included in the 16-byte frame |
| Total fast-path stack use | 24 bytes unprotected; 32 bytes protected | 16 bytes on either path |
| Atomic reservation | One `lock cmpxchgq`; `rax` holds expected old pointer | One `ldaxr`/`stlxr`; exclusive monitor detects interference |
| Result calculation | New pointer minus scaled size | New pointer minus scaled size |
| No space / contention | Restore saves; tail-jump to `__allocate` | Clear abandoned reservation if needed; restore saves; tail-branch to preserving stub |
| Retry / expansion | Shared C allocator | Same shared C allocator |

Both backends use a small successful path and enter their broader C-call
register-save stubs only on failure. Neither takes a mutex simply because a
region is protected. The mutex-null test also lets private regions avoid atomic
updates without globally disabling allocation protection.

X64 can consume its two argument registers as arithmetic temporaries after
stacking the inputs. Its CAS has the fixed `rax` operand, so `rax` must survive
the operation. ARM64 keeps the inputs in registers and needs no fixed expected
value register: `stlxr` validates the exclusive reservation. Reusing the saved
LR as a temporary and reconstructing the result keeps the ARM64 frame smaller
than the initial 48-byte implementation. Both helpers keep the full allocator's
mutex-based expansion behavior; neither makes the complete allocator lock-free.

## Shared controls

| Compiler / flags | Allocation behavior |
| --- | --- |
| MLKit default | Sequential; `par0` options are blocked |
| MLKit `-no_gc -par` | Parallel allocation with normal protection decisions |
| ReML default | `-par -par0`: parallel execution using region separation |
| ReML `-no_par0` | Parallel execution with normal protection decisions |
| Either compiler `--no_inline_allocation` | Infinite-region allocations use the existing C allocator |

`--inline_allocation` defaults to enabled in both backends. It controls generated
fast paths, including shared assembly helpers; disabling it does not select a
mutex-only allocator. C allocations retain atomics for protected regions and
use `alloc_unprotected` when ReML's `par0` is active. Finite regions and reset
policy are unchanged. `_NOINLINEALLOC` separates caches for the disabled mode,
and the flag is fixed when a REPL session starts.

For example, the following ReML pairs measure separation and generated fast
paths independently, with no additional strategy enumeration:

```sh
reml -o separated program.mlb
reml -no_par0 -o protected program.mlb
reml --no_inline_allocation -o separated_c program.mlb
reml -no_par0 --no_inline_allocation -o protected_c program.mlb
```

Add `--alloc_protect_always` to the protected commands when the comparison
should also disregard protection analysis.
