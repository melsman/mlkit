# Align C-call preservation, then assess single-page reset

PR #253 first addresses issue #252 by aligning preservation across explicit
foreign calls and implicit C calls in X64 and ARM64. Only after that work is
validated does it investigate a frame-free single-page reset fast path in the
shared helper. It remains separate from allocation PR #251; use the ARM64
implementation from that work once available and avoid duplicating its changes.
This is an implementation plan: no emitted code changes or measured benefit yet.

## Eligibility and semantics

C `resetRegion` clears the descriptor's low two status bits for addressing,
resets each generation, releases large objects, clears the large-object list,
and returns the original descriptor with its status bits intact.

A reset can avoid C when every generation's first page has a null successor
and the region's large-object list is null. Checking only the page chain is
insufficient: a one-page region can also own separately allocated large objects.
With generational GC, each generation must have one page; this is not literally
one page for the entire region.

For each eligible generation, set its allocation pointer to the first page's
payload. With generational GC, also set that page's colour pointer to the same
address. Preserve the first-page pointer and its GC mark bits. `clear_fp` masks
five low bits when required; descriptor status bits use a different mask.
No page is released, so the page-free accounting in `resetGen` does not apply.
Use the runtime layout definitions to verify offsets and header sizes.

Check every eligibility condition before changing any generation. On failure,
branch to the existing preserving C bridge with the original descriptor intact.
Keep finite-region and conditional-reset decisions at their existing boundary.
Do not infer that parallel allocation makes a concurrent reset safe; retain the
existing language/runtime ownership requirements for reset.

## Backend comparison

X64 `__reset_region` currently enters a preserving bridge to C `resetRegion`.
The proposed fast prefix uses reserved scratch registers and returns directly,
without explicit stack storage or touching allocatable integer/FP registers.
The slow bridge continues to preserve C-clobbered ML registers. The helper's
normal call/return still uses its architectural return-address mechanism.

The ARM64 development in PR #251 already emits a simple reset fast path at
reset sites when parallelism and profiling are disabled. Other configurations
reach a preserving C helper. Compare moving or sharing this logic inside the
reset helper, including parallel builds, rather than duplicating it at sites.
ARM64 must preserve the caller's link register while using its scratch registers.
Do not generalise the X64 return-address convention to ARM64.

Ordinary GC, generational GC, profiling and parallel layouts must be considered
separately. Start by retaining the existing C path for profiling until its
accounting and profiler-visible helper ranges have been checked explicitly.

## Implementation and assessment milestones

Complete milestones 1–5 before beginning the reset optimisation in 6–8.
Keep changes separately reviewable so preservation fixes can stand on their own
if the reset optimisation does not prove worthwhile.

1. **Establish call contracts and baseline code.** Inventory explicit
   `CCALL`/`CCALL_AUTO`, implicit allocation/reset calls, region creation/release,
   GC, profiling and callback bridges in both backends. Record allocatable and
   reserved registers, C clobbers, FP preservation widths, argument staging,
   result placement and exception transfers. Identify where physical liveness
   is available and where it is not. Capture representative emitted code.
2. **Align explicit foreign-call preservation.** Trace register allocation and
   fetch/flush handling of values live across calls. Determine why X64's
   `is_callee_save_ccall` returns false despite its C-preserved palette, and
   correct it where sound. Align both backends around saving live C-clobbered
   values and retaining values in valid C-preserved registers. Keep argument
   staging, conversions and ABI stack arguments distinct from preservation.
   Check integer and FP cases and exclude newly defined call results from saves.
3. **Align implicit C-call bridges.** Audit allocation, reset and other late
   runtime calls. Preserve all C-clobbered ML registers whenever no sound
   call-site live set is supplied, including spill temporaries that may be live.
   Use the existing liveness-aware region creation/release design as a reference
   where applicable. Do not reduce saves using only GC-root liveness. Check
   return values, reserved scratch registers, ARM64 link-register handling and
   ABI stack alignment; retain collector and callback-specific save contracts.
4. **Validate preservation correctness.** Add focused tests for live/dead integer
   and FP values, C-preserved registers, result-register overlap, register
   pressure/spills, more arguments than ABI registers, conversions, callbacks
   and exception paths. Exercise explicit calls and implicit allocation/reset
   fallbacks with register sentinels, GC and supported profiling/parallel modes.
   Verify emitted code removes redundant saves without losing required saves.
   Correctness tests may run in CI; performance tests and timing collection may
   not.
5. **Assess and complete #252.** Build both native backends and run relevant
   regressions. Manually compare before/after code size, stack traffic and
   representative foreign/runtime-call workloads on X64 and ARM64. Publish
   results and document justified backend-specific ABI differences. Assess full
   reset inlining requested by #252, including unavoidable nested page/object
   release calls; record why the bounded single-page case below is the first
   candidate rather than assuming full inlining avoids preservation. Resolve
   the call-preservation work before proceeding to milestone 6.
6. **Prototype the shared-helper single-page reset fast path.** Start on X64;
   check the large-object list and every generation's page successor before
   modifying state. Rewind allocation and colour pointers using reserved
   scratch registers, preserve descriptor status/first-page marks, and return
   without an explicit frame. Verify runtime offsets. Retain the validated
   preserving C fallback for ineligible cases and initially for profiling.
   Compare ARM64's existing site fast path with placement inside its shared
   helper, avoiding unnecessary duplicated checks and respecting its return ABI.
7. **Validate reset semantics.** Compare emitted helpers with C `resetRegion`
   for empty, partially filled and full pages, multiple pages, large objects
   with a single ordinary page, descriptor status bits, first-page marks and
   both generations. Check integer/FP preservation on fast and fallback paths,
   conditional/finite-region behaviour, supported GC/parallel configurations
   and absence of explicit stack storage on the eligible path. Keep profiling
   on the C path until its accounting is explicitly validated.
8. **Measure and decide on reset inlining.** Measure native X64 and ARM64
   manually: repeated eligible resets, mostly ineligible resets, and realistic
   allocation/reset workloads. Hold caller code constant for helper-only
   comparisons; assess ARM64 site-versus-helper placement separately. Record
   hit rates separately from timings, code size, fallback overhead and all raw
   samples. Retain or reject the optimisation from this evidence and document
   remaining configuration differences. Add no performance tests to CI.

## Sources

- `src/Runtime/Region.c`: `resetGen`, `resetRegion`, `resetRegionProf`.
- `src/Runtime/Region.h`: `Gen`, `Ro`, `Rp`, `clear_fp`, `clearStatusBits`.
- `src/Compiler/Backend/X64/CodeGenX64.sml`: `ccall_stub`, `resetregion`.
- `src/Compiler/Backend/X64/CodeGenUtilX64.sml`: `reset_region`.
- ARM64 development in PR #251: `resetLoadedInto` and the preserving reset stub.
