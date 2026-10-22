# ARM64 atomic allocation fast path: development plan

Status: milestones 1–4 are implemented; validation and measurements are recorded
in `arm64-atomic-allocation-results.md`. Follow-up milestones 5–7 are implemented.
Milestone 8 has ARM64 measurements and code-size assessment; native X64 paired
performance measurements remain outstanding. See `native-inline-allocation-results.md`.

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

## Initial implementation (milestones 1–4)

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

## Initial comparison with X64

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

## Agreed follow-up: inline allocation-local dispatch in both backends

For MLKit with `-par` and ReML with `-no_par0`, eligible ordinary infinite-region
allocation sites will check the existing region mutex field. A NULL mutex selects
an inline ordinary bump-pointer update when the current page has room. A non-NULL
mutex selects a shared protected allocation helper containing the CAS sequence.
CAS instructions will not be duplicated at allocation sites. Consider a protected
helper entry that skips the redundant mutex check; its ABI and fallback arguments
must first be specified and verified.

This differs from the current implementation, where these parallel modes call a
shared helper even for allocation-local regions. Both backends already dispatch
on the mutex inside that helper. The follow-up moves the allocation-local decision
and ordinary successful allocation to the allocation site, avoiding the helper
call and its saves for that case. No descriptor layout change is required.

ReML's default `-par -par0` keeps its existing inline allocation without a
protection check. This is not adoption of the earlier proposal to make `par0` a
dynamic descriptor policy. MLKit continues to reject `par0` options. Disabling
`--inline_allocation` continues to use the existing C allocator, including atomics
for protected regions. Profiling retains its runtime path. Finite regions, reset
semantics, large objects and page expansion must preserve their existing behavior;
ordinary no-space fallback must retain the region's correct protection policy.
No additional standard library variants or ReML `-no_par0` prebuilds are planned.

### 5. Specify the common dispatch and backend ABIs

- [x] Specify the eligible sites, mutex-null test, page-space test, result/tag
      adjustment and no-space/protected transfers for X64 and ARM64.
- [x] Specify scratch registers, live-register preservation and original arguments
      at each helper entry; decide whether to add a protected-only entry.
- [x] Document unchanged ReML defaults, C/profiling paths, reset/finite behavior,
      and acquire/release requirements for protected allocation.

Acceptance: review instruction sketches for both backends, including exact page
fills and all fallback edges, before implementing the change.

### 6. Implement aligned inline allocation-local paths

- [x] Implement the descriptor check and ordinary inline success path in X64 and
      ARM64; keep atomic reservation in shared helpers.
- [x] Aim to use no stack storage or frames on successful fast allocation paths,
      both the inline allocation-local path and the shared protected CAS path.
      Prefer scratch-register allocation and result reconstruction over stack
      saves; document any unavoidable exception and its ABI constraint. Stack
      saves remain permitted in the runtime fallback.
- [x] Preserve standalone and incremental REPL helper linkage, flag behavior,
      existing runtime fallback and standard cache preparation policy.

Acceptance: inspect emitted code from both backends: allocation-local success
makes no helper call or stack saves, protected allocation transfers to shared CAS,
and default ReML gains no protection check. Inspect successful paths for stack
accesses and frame creation, aiming to eliminate both; document any remaining
requirement. Record added bytes per allocation site.

### 7. Validate allocation and fallback correctness

- [x] Extend allocation-specific assembly checks for both backends and flag modes.
- [x] Exercise allocation-local success, protected success/contention, exact fills,
      expansion, large objects, finite/reset cases and tag-free adjustments.
- [x] Verify live integer/FP registers and fallback arguments; stress mixed C and
      generated allocation with pthreads and available Argobots configurations.
- [x] Run relevant MLKit/ReML native, profiling, callback, REPL and regression
      coverage, plus the read-only installed-library and build-order checks.

Acceptance: execute on each architecture, or explicitly record unavailable native
coverage as a review limitation; assembly inspection alone is not execution proof.

### 8. Assess performance and code-size tradeoffs

- [ ] Compare against the current shared-helper implementation with repeated
      paired runs, matching flags/caches and verified results on each architecture.
      ARM64 is complete; native X64 timings are deferred until the ThinkPad is
      available. CI must run correctness checks only, not performance tests.
      X64 assembly size and translated correctness checks are recorded separately.
- [x] Measure ARM64 allocation-local workloads, uncontended protected allocation,
      shared contention, mixed C/generated allocation and frequent expansion.
- [x] Measure ARM64 representative parallel programs, retain default ReML as a control,
      and compare C allocation with `--no_inline_allocation` where useful.
- [x] Report available timing distributions, allocation-site and total text size, and any
      regressions, particularly the added check on protected-region allocations.

Acceptance: document whether allocation-local gains justify code growth and
protected-path costs on each backend; investigate material regressions before
recommending the design for merge. Record commands, raw samples and limitations.

## Resolved follow-up ABI (milestone 5)

Both backends test the existing mutex field only for protected parallel mode.
Allocation-local success updates the pointer inline without stack storage.
Exact fills are accepted; an allocation pointer already at a page end fails the
next nonzero request. Large requests and profiling retain runtime handling.

ARM64 keeps region/result in x16, size in x17 and scratch x30 at allocation
sites. The inline mutex check precedes a page-offset test. For bounded ordinary
requests, `((a-1) & 8191) + bytes` is below 16384; bit 13 detects insufficient
space, including a pointer already at the page end. x17 retains the old pointer
for the result, and x30 computes the new pointer for the store. Protected sites
call a separate helper entry which skips the repeated mutex/size checks; its
preconditions are a protected descriptor and a compile-time bounded request.
No-space edges restore the infinite-region bit and use the compatible shared
helper. That helper retains its mutex/size checks for existing private-ABI
callers (including C bridges), but saves x0 in d30 and its own
LR in d31 instead of creating a frame. d30/d31 are reserved FP scratches, outside
the ML allocation palette. They are restored to x0/LR on both success and fallback;
the original size and region remain available on failure. The exclusive sequence
and acquire/release protocol remain unchanged. The helper needs no stack storage.

X64 keeps the descriptor in r11 and uses r10 for the inline space test and
pointer update. For a local region, two pointer loads are safe because no other
thread allocates into it. The offset test is `(a-1) & (page_size-1) <=
page_size-1-bytes`; the compile-time request bound makes the right side
nonnegative. Protected sites call a new `allocinreg_protected` entry, bypassing
the helper's mutex dispatch; local expansion calls `__allocate` (or
`__allocate_unprotected` under par0). Out-of-line calls return to the common
result continuation without repeating inline allocation or GC accounting.

The X64 shared helper saves rax in xmm14 and size in xmm15; both are reserved
FP scratches. It keeps region in r11, computes new pointer in r10 and page limit
in rax, then reconstructs the original expected pointer as `new-8*size` before
CAS. No second shared-pointer load is used to reconstruct that expectation.
Success returns the old pointer and restores rax. Failure restores rax and size
before tail-transferring to the existing runtime stub. No explicit stack saves
or frame are needed; the X64 call instruction still necessarily pushes its
8-byte return address. That is the remaining architecture-specific stack cost.
The compatible mutex-dispatch entry remains for existing private-ABI callers.

Allocation may clobber the two reserved FP scratches, but no allocatable integer
or FP register. Register preservation and real-valued allocation users require
execution coverage. No runtime layout, protection inference or library-cache
policy changes are part of this follow-up.

## Planned follow-up: standalone programs without spawning

Each native compilation unit's link-object will carry a `may_spawn` boolean,
recording whether its retained generated code contains a spawning operation.
The compiler's final standalone link step ORs the booleans of all included units.
This is compiler link metadata, not an `ld` feature or a scan of object symbols.
A present but never executed spawning operation conservatively counts as spawning;
this does not require whole-program call-graph reachability analysis.

Before any global or other region is created, startup establishes a fixed runtime
policy. If no linked unit may spawn, `allocateRegion` and related constructors
initialize the existing mutex pointer to NULL even where protection was requested.
Otherwise, constructors honor existing protection decisions. The descriptor layout
and generated allocation checks stay unchanged; NULL selects ordinary allocation
rather than CAS, including for global regions and C allocation fallback.

The REPL always retains normal protection from startup: later input may spawn,
and existing regions must already have suitable protection. There is no transition
from unprotected to protected regions after the first spawn. Foreign-code analysis
is explicitly out of scope. This work does not enable parallelism by default,
change ReML's separation controls, or claim to remove all parallel-runtime overhead.

### 9. Specify and record spawning in link-objects

- [ ] Define `may_spawn` for retained generated spawning operations in both native
      backends, following the lowering of `spawnone` to thread creation.
- [ ] Add the boolean to native link metadata and its serialization; ensure it is
      reset per compilation unit and survives separate compilation and cache reuse.
- [ ] Choose and document cache compatibility: old/unknown metadata must either
      conservatively mean may-spawn or be invalidated and rebuilt. It must never
      silently imply no spawning. Keep the standard library variant count unchanged.

Acceptance: units with and without spawning produce the expected link-object
values on both backends, including unused-but-retained spawning code, fresh
compilation and cached reuse. No linker-specific symbol mechanism is used.

### 10. Aggregate the policy and initialize it before region creation

- [ ] OR `may_spawn` across every included link-object, including cached library
      units, and convey the result through native standalone startup code.
- [ ] Establish a conservative runtime default and set the standalone policy before
      global region construction on both X64 and ARM64.
- [ ] Keep REPL startup and incremental linking permanently conservative, including
      a session that first creates regions and only later evaluates spawning code.

Acceptance: inspect and execute startup paths to show that the policy is set before
any relevant region constructor, that one spawning unit preserves normal protection,
and that the absence of spawning removes protection requests program-wide. Missing
metadata or startup-policy information cannot accidentally disable protection.

### 11. Apply the policy consistently to region constructors

- [ ] Route global and local region constructors, including specialized constructors
      and profiling configurations, through the common effective protection policy.
- [ ] In proven no-spawn programs, initialize mutex pointers to NULL without obtaining
      mutexes; otherwise retain existing inferred/forced protection decisions.
- [ ] Preserve deallocation, reset, expansion and large-object invariants, the shared
      C allocator's descriptor-based dispatch, and ReML's `par0` behavior.

Acceptance: no-spawn global and local regions take ordinary generated/C allocation
paths without CAS or region-mutex allocation. Programs containing spawning and REPLs
retain their established protection behavior. No descriptor layout change, new
strategy flag or additional prepared library variant is introduced.

### 12. Validate correctness and assess remaining overhead

- [ ] Add correctness checks for no-spawn/spawn units, mixed separate compilation,
      cached Basis/library reuse and conservative cache compatibility.
- [ ] Exercise global and local allocation, forced protection, generated paths on/off,
      page fills/expansion, large objects, reset/deallocation and profiling.
- [ ] Verify REPL protection both before and after evaluating a later spawning phrase;
      run pthread/available Argobots coverage and read-only installation checks.
- [ ] Run the relevant correctness suites on both backends and record unavailable
      native execution coverage explicitly.
- [ ] Manually compare standalone no-spawn programs with sequential compilation and
      existing protected parallel allocation; record code size, remaining descriptor
      checks, region creation and runtime startup costs. Native X64 performance work
      waits for the ThinkPad. Do not add performance tests or timing collection to CI.

Acceptance: publish commands, correctness results, manual measurements and limits.
Separate the removal of region synchronization from other costs of parallel-enabled
execution. Retain existing defaults until a separate decision enables MLKit
parallelism by default. Foreign-code analysis is not part of these milestones.
