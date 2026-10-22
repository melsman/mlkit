# Inline allocation-local dispatch: milestones 5–8

## Behavior and register contracts

With generated allocation enabled, MLKit `-par` and ReML `-no_par0` now check
the region mutex at each eligible ordinary allocation site. A NULL mutex uses
inline ordinary allocation, without an allocation frame or helper call. Protected
regions call shared atomic allocation code. CAS instructions are not duplicated
at allocation sites. Large requests, page expansion and unsuccessful reservations
retain the runtime allocator; profiling and reset policies are unchanged.

Default ReML retains unchecked inline allocation under `-par -par0`. Disabling
`--inline_allocation` still uses the existing C allocator with atomics when
required. There are no new flags, descriptor fields or prepared library variants.
In particular, no ReML `-no_par0` library preparation was added.

ARM64 uses x16/x17/x30 in the inline path. Its shared helper saves x0/LR in
reserved FP scratch registers d30/d31, replacing the previous 16-byte frame.
It restores them before returning or entering the full runtime stub. Protected
allocation retains the single acquire/release exclusive attempt and `clrex`
cleanup. The compatible helper still checks the mutex/size for existing ABI
callers; generated protected sites use a bounded-request entry that skips both
checks. Local allocation retains the old pointer and tests the page offset plus
request using bit 13, avoiding result reconstruction and a separate comparison.

X64 uses r11 for the descriptor and r10 for the inline page-offset test and
pointer update. Its new protected helper entry skips the already performed
mutex check. The shared helper saves rax and word count in reserved xmm14/xmm15.
After computing the new pointer and page bound, it reconstructs the expected
old pointer as `new - 8*words` before `lock cmpxchgq`; it never reloads a potentially
changed pointer to establish that expectation. Failure restores the original
arguments and rax before runtime fallback. The helper's former 16/24 bytes of
explicit stack saves are gone. An X64 `call` still pushes an 8-byte return address;
ARM64 uses LR and needs no allocation stack storage on success.

The resolved ABI and acceptance criteria are in
[the development plan](arm64-atomic-allocation-plan.md).

## Correctness assessment

Executed on Apple Silicon, macOS 26.5.1, Apple clang 21.0.0:

- Full native ARM64 suite: MLKit/ReML, calls/closures, integer and FP spills,
  finite/infinite regions, exceptions, stack arguments/results, tail calls,
  profiling, GC/generational GC, callbacks and REPLs.
- ARM64 pthread and Argobots 1.2 suite, including one/four execution streams,
  inferred/forced protection, default ReML and overrides, C-only allocation,
  concurrent generated/C allocation and parallel REPLs.
- Allocation-specific assembly checks: no duplicated exclusives at sites,
  inline ordinary success without a call/stack access, and no fast-helper frame.
- Direct plain/GC/generational-GC/parallel probes with generated paths both on
  and off. Added local-region exact-fill/expansion cases; existing protected
  boundary, finite/reset, large-object and tag-free cases remain covered.
- Forced exclusive-store failure, plus live integer/FP sentinels across success
  and fallback. Shared C/generated probes now explicitly test complete page fills,
  expansion and large objects in both protected and allocation-local regions.
- Eight-cache build-order check and read-only installed API check, including
  default ReML and profiling. Installed files remain unchanged.

X64 emitted assembly is compiled and executed through Rosetta on this ARM64
host. The new `X64/tests/check-allocation.sh` exercises the actual emitted helper,
protected/local boundary cases, large objects, shared contention, mixed C/generated
allocation, forced CAS failure, rax/FP sentinels, inline inferred/forced protection,
C-only allocation and ReML flag combinations. It rejects explicit helper stack
storage and CAS instructions at allocation sites. The Linux `check-compiler`
target now runs it with both MLKit and ReML for native CI coverage.

Rosetta execution is useful correctness coverage, but is not native X64 hardware
performance evidence. Native X64 validation and measurements must be distinguished
from the ARM64 measurements below.

## Measurement method

The comparison baseline is PR commit `ac46a18e`: both backends already had the
shared generated atomic fast path; ARM64 used its 16-byte helper frame. The new
version adds inline local dispatch and removes explicit fast-helper stack saves.
This differs from the original milestones 1–4 comparison against C-only ARM64
allocation, whose results remain in `arm64-atomic-allocation-results.md`.

Each case uses nine paired runs with alternating order and verified output.
Allocator probes report wall time around the allocation work. Whole programs and
the allocation-local loop report user CPU and wall time from `/usr/bin/time`.
The local loop allocates and traverses one hundred million list cells in reclaimed local
regions. The same source can force protection or use ReML's default separation.
Fresh per-version cache suffixes prevent object reuse across compilers.

Reproduction entry points:

```sh
SML_LIB=$PWD ARM64_EMITTER=$PWD/bin/arm64-emitter-test \
  ARM64_BASELINE_EMITTER=/path/to/baseline/arm64-emitter-test \
  sh src/Compiler/Backend/Arm64/tests/benchmark-allocation.sh
SML_LIB=$PWD MLKIT_ARM64=$PWD/bin/mlkit MLKIT_BASELINE=/path/to/baseline/mlkit \
  sh src/Compiler/Backend/Arm64/tests/benchmark-parallel-programs.sh
SML_LIB=$PWD MLKIT=$PWD/bin/mlkit MLKIT_BASELINE=/path/to/baseline/mlkit \
  sh src/Compiler/Backend/tests/benchmark-allocation-local.sh
```

`ALLOCATION_FLAGS` selects forced protection (`-no_gc -par --alloc_protect_always`)
or ReML modes (`-par -par0`, `-par -no_par0`); use matching ReML compiler binaries.

## ARM64 measurements

User CPU seconds are median [minimum, maximum] across nine runs per version.

| Workload | Previous helper | New inline dispatch | Median change |
| --- | ---: | ---: | ---: |
| Allocation-local loop, MLKit `-par` | 0.34 [0.34, 0.34] | 0.31 [0.31, 0.32] | -8.8% |
| Same loop, forced protection | 1.63 [1.60, 1.67] | 1.67 [1.64, 1.71] | +2.5% |
| Same loop, default ReML | 0.36 [0.36, 0.37] | 0.36 [0.36, 0.37] | unchanged |
| Same loop, ReML `-no_par0` | 0.34 [0.34, 0.34] | 0.31 [0.31, 0.31] | -8.8% |
| Parallel merge sort | 0.11 [0.11, 0.12] | 0.09 [0.09, 0.09] | -18.2% |
| Parallel allocation/computation | 0.13 [0.12, 0.15] | 0.13 [0.13, 0.13] | unchanged |

The representative programs are short and `/usr/bin/time` reports hundredths
of a second; the merge-sort percentage should be read with that granularity in
mind. These are workload-specific results, not a general speedup claim.

The C-driven bridge probes call the compatible helper directly, so they measure
frame removal rather than the new inline dispatch. Median wall times (seconds):

| Scenario | Previous helper | New helper | Median change |
| --- | ---: | ---: | ---: |
| One protected allocator | 0.008988 | 0.009074 | +1.0% |
| Four generated allocators, shared region | 0.060056 | 0.060837 | +1.3% |
| Mixed generated/C, shared region | 0.061232 | 0.061470 | +0.4% |
| Frequent expansion | 0.025603 | 0.025652 | +0.2% |
| Private regions through C bridges | 0.012813 | 0.012869 | +0.4% |

The ranges overlap in these bridge scenarios. Removing frames alone does not
establish a speed improvement on this host.

## Regression investigation and code size

The first version measured 0.34 to 0.36 seconds (+5.9%) in the local loop and
1.62 to 1.73 seconds (+6.8%) with forced protection. Assembly inspection identified
avoidable result reconstruction in the inline local path and repeated mutex/size
checks on the protected path. Retaining the old pointer, shortening the page test,
and providing the protected entry produced the final results above. Initial raw
samples are retained alongside the final samples. The remaining +2.5% forced-
protection cost is recorded rather than hidden: moving the dispatch to sites adds
a check before shared allocation, and moving saves to FP registers changes the
dependency chain. These observations do not isolate the cause of the residual
regression. No claim is made that fewer stack accesses must be faster.

For the local-loop fixture, one hot allocation site grows the ARM64 unit's
`__text` from 728 to 796 bytes (+68, including outlined fallback blocks). Shared
link code grows from 1348 to 1380 bytes (+32); executable instruction text grows
from 28976 to 29076 bytes (+100). Default ReML's unit/link/executable text sizes
are unchanged. Allocation-local inline success and protected helper success have
no allocation frame or stack accesses.

X64 assembly-size inspection shows unit text 442 to 516 bytes (+74, including
outlined blocks), shared link text 1785 to 1827 (+42), and executable instruction
text 20296 to 20424 (+128, including linker alignment). This size comparison uses
the earlier X64 flag-check compiler (`f45fa614` allocation implementation), whose
allocation code is unchanged through `ac46a18e`. X64 timings are deliberately not
inferred from Rosetta execution. Its helper has no explicit stack saves; `call`
still pushes its return address.

## Assessment and remaining coverage

Milestones 5–7 are implemented and validated within the execution coverage stated
above. ARM64 measurements for milestone 8 show local-allocation gains, unchanged
default ReML, code growth, and a modest forced-protection regression. Keep the PR
in draft for review of that tradeoff. Native X64 paired performance measurements
are deferred until the ThinkPad is available; the shared benchmark script can
run there with matching baseline/current compilers. CI runs correctness checks
only; no performance assessment or timing collection is added to CI.

The full ARM64 regression pass comprised 260 developer, 189 GC, 89 explicit-region
and 13 parallel checks. It preceded the final protected-mode-only instruction
refinement; the complete pthread/Argobots suite and deterministic allocation
probes were rerun after that refinement. Nonparallel/profiling behavior was not
changed by the refinement.

Raw timing distributions and instruction-text sizes:
[`native-performance/inline-allocation/`](native-performance/inline-allocation/).
