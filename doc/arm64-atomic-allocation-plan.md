# ARM64 atomic allocation fast path: development plan

Status: planning only; no compiler or runtime implementation changes yet.

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
  `allocSlowInto`. The shared preserving stubs save 24 integer registers, 30
  floating-point registers, and LR in a 448-byte frame before calling the
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

## Proposed approach

Add a shared assembly fast helper ahead of the full preserving fallback. Keep
allocation sites short and preserve the existing private ABI. Determine the
minimum additional register saves needed before choosing the final placement.

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

Initially use a single exclusive-store attempt, following X64's bounded fast
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

- [ ] Choose helper placement, scratch registers, minimal saves, and LR handling.
- [ ] Specify protected/unprotected dispatch, original-argument restoration, and
      the handoff to both allocation stubs.
- [ ] Review acquire/release ordering and the page-boundary reservation protocol
      against `allocGen`; decide whether an instruction/printer extension for
      exclusive-monitor cleanup is needed.
- [ ] Confirm finite regions, at-bottom reset, exact page fills, tag-free values,
      large objects, profiling, and supported parallel option combinations.

Acceptance: document the final instruction sequence and invariants before
changing allocation behavior.

### 2. Implement the generated fast path

- [ ] Emit the shared helper and route eligible protected parallel allocations
      through it.
- [ ] Preserve ordinary allocation for regions without a mutex and existing
      explicitly unprotected behavior.
- [ ] Retain the full preserving fallback on insufficient space or failed
      reservation, without changing runtime layout or allocation semantics.
- [ ] Check standalone initialization and incremental REPL helper linkage.

Acceptance: inspect generated assembly to establish that success avoids the
448-byte save frame and C allocation call, and failure preserves all required
registers and arguments.

### 3. Validate correctness and interoperability

- [ ] Add allocation-specific emitted-code checks rather than relying on the
      existing exception-counter `ldaxr`/`stlxr` matches.
- [ ] Exercise uncontended allocation, exact fills, repeated expansion, large
      objects, finite regions, reset modes, and tag-free pointer adjustment.
- [ ] Verify integer/FP register sentinels on successful and fallback paths.
- [ ] Stress multiple generated-code allocators sharing one protected region;
      verify disjoint storage and retained contents.
- [ ] Mix generated-code and C allocation into the same region to exercise page
      publication and contention across both implementations.
- [ ] Run pthread and available Argobots coverage (including multiple execution
      streams), MLKit/ReML, callbacks, protection policies, and REPL linkage.
- [ ] Run the relevant native/regression suites and confirm existing profiling
      behavior and unsupported-option guards remain correct.

Acceptance: record commands, configurations, results, and any skipped coverage.
Source inspection alone is not correctness validation.

### 4. Measure and document results

- [ ] Compare the baseline and implementation for uncontended allocation,
      shared-region contention, frequent expansion, and a private-region control.
- [ ] Measure representative parallel programs with repeated runs, identical
      flags and caches, and verified output.
- [ ] Record generated code size and the fast/fallback register-save costs;
      report timing distributions and any regressions.
- [ ] Update `doc/arm64-allocation-paths.md` with the final protocol and results.

Acceptance: substantiate the benefit with measurements; do not assume X64-like
speedups or make a lock-free claim for the complete allocator. Expansion and
large-object handling retain mutexes.

## Current validation

This planning change was checked by source inspection only. No builds,
correctness tests, or benchmarks were run, and no speedup has been measured.
