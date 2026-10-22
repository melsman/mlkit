# Single-page region reset in the shared helper

Investigate a frame-free fast path inside the shared reset helper, independently
of allocation PR #251 and the broader foreign-call alignment issue #252.
This document records source-level findings and the experiment to run; it does
not establish a performance benefit or change emitted code.

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

1. Prototype the shared-helper fast prefix on X64. Check the large-object list
   and page successor(s), then rewind pointers using only reserved scratch
   registers. Retain the existing C fallback unchanged. Verify runtime layout
   offsets rather than copying unchecked constants.
2. Test emitted code against C `resetRegion`: empty/partially filled/full first
   page, multiple pages, large objects with a single ordinary page, descriptor
   status bits, first-page marks, and both generations. Use integer and FP
   sentinels to check preservation on fast and fallback paths. Assert no explicit
   frame on the eligible path. Exercise the supported GC and parallel modes.
3. Measure manually on native X64: repeated eligible resets, mostly ineligible
   resets, and allocation/reset workloads representative of generated ML code.
   Compare identical caller code with only helper bodies changed. Record hit
   rates separately from timings; inspect code size and all retained samples.
   Measure whether the extra checks penalise the slow cases. No performance
   tests or timing collection in CI.
4. Compare ARM64 placement in the helper versus its existing site fast path.
   Preserve link-register and GC/profiling contracts; measure native ARM64 before
   deciding whether to move existing logic or extend its configuration coverage.
5. Retain or reject the optimisation based on correctness, hit frequency,
   eligible-path savings and fallback overhead. Publish raw manual results and
   explain any backend differences left in place.

## Sources

- `src/Runtime/Region.c`: `resetGen`, `resetRegion`, `resetRegionProf`.
- `src/Runtime/Region.h`: `Gen`, `Ro`, `Rp`, `clear_fp`, `clearStatusBits`.
- `src/Compiler/Backend/X64/CodeGenX64.sml`: `ccall_stub`, `resetregion`.
- `src/Compiler/Backend/X64/CodeGenUtilX64.sml`: `reset_region`.
- ARM64 development in PR #251: `resetLoadedInto` and the preserving reset stub.
