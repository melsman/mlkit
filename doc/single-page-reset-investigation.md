# Align C-call preservation, then assess single-page reset

PR #253 first addresses #252 through a focused change: support C-preserved
registers consistently in the X64 backend, using the existing allocator and
fetch-and-flush machinery. ARM64 supplies the reference behaviour. Implicit
allocation/reset C-call bridges retain their required preservation. The second
part investigates a frame-free single-page reset fast path in the shared helper.
Allocation PR #251 is merged into master; build on its ARM64 implementation.
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

ARM64 already emits a simple reset fast path at
reset sites when parallelism and profiling are disabled. Other configurations
reach a preserving C helper. Compare moving or sharing this logic inside the
reset helper, including parallel builds, rather than duplicating it at sites.
ARM64 must preserve the caller's link register while using its scratch registers.
Do not generalise the X64 return-address convention to ARM64.

Cover ordinary GC, generational GC, profiling and parallel layouts. Profiling
is included in the fast-path scope: an eligible reset releases no pages or large
objects. Verify the applicable profiling entry point, layout and metadata as
part of implementation and correctness checks.

## Implementation and assessment milestones

Complete milestones 1–5 before beginning the reset optimisation in 6–8.
Keep changes separately reviewable so preservation fixes can stand on their own
if the reset optimisation does not prove worthwhile.

1. **Confirm the X64 mismatch.** Trace the existing C-preserved allocation
   palette through `RegAlloc` and `FetchAndFlush`. Check why
   `is_callee_save_ccall` returns false and verify which palette registers can
   safely hold ML values across an explicit C call, including runtime-reserved
   uses and callbacks. Use ARM64 as a reference; no general call redesign.
2. **Enable X64 C-preserved registers.** Make the predicate agree with the
   verified palette so existing liveness and fetch/flush logic avoid redundant
   stores/reloads. Adjust the palette only if the audit finds an unsafe member.
   Leave ARM64 behaviour and argument staging unchanged. X64 FP registers remain
   C-clobbered; implicit C-call bridges continue to preserve all required ML
   registers rather than relying on this explicit-call optimisation.
3. **Add focused correctness tests.** Keep values live across explicit manual
   and automatically converted C calls, including register pressure, returned
   values, callbacks and exception paths. Verify that C-preserved integer values
   survive and C-clobbered integer/FP values still receive required protection.
   Check emitted code to establish that redundant flush/fetch pairs disappear.
4. **Run relevant regressions.** Build X64 and exercise C-call, allocation/reset,
   GC, profiling and parallel correctness checks as applicable. Confirm implicit
   bridges retain their preservation contract. Run relevant ARM64 checks as a
   reference without expanding this milestone into backend restructuring.
5. **Assess and document the focused fix.** Manually compare X64 generated code,
   stack traffic and representative foreign-call workloads before/after. Record
   results and any remaining ABI differences from ARM64. Resolve #252's
   preservation mismatch through this bounded change; investigate reset inlining
   separately in milestones 6–8. No performance tests or timing collection in CI.
6. **Prototype the shared-helper single-page reset fast path.** Start on X64;
   check the large-object list and every generation's page successor before
   modifying state. Rewind allocation and colour pointers using reserved
   scratch registers, preserve descriptor status/first-page marks, and return
   without an explicit frame. Verify runtime offsets. Retain the validated
   preserving C fallback for ineligible cases. Include profiling configurations
   in the prototype, using their applicable layouts and entry points.
   Compare ARM64's existing site fast path with placement inside its shared
   helper, avoiding unnecessary duplicated checks and respecting its return ABI.
7. **Validate reset semantics.** Compare emitted helpers with C `resetRegion`
   for empty, partially filled and full pages, multiple pages, large objects
   with a single ordinary page, descriptor status bits, first-page marks and
   both generations. Check integer/FP preservation on fast and fallback paths,
   conditional/finite-region behaviour, supported GC/parallel configurations
   and absence of explicit stack storage on the eligible path. Include profiling
   configurations and verify their accounting and helper metadata.
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
- `src/Compiler/Backend/Arm64/CodeGenArm64.sml`: `resetLoadedInto` and the preserving reset stub.
