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

## Native X64 measurements (ThinkPad, 2026-10-22)

The ThinkPad has an Intel Core i7-1360P and runs Linux x86_64 with GCC 15.2.0.
It was on AC power with the balanced power profile. Both versions were built
using the same installed MLKit v4.7.15 seed, with clean compiler caches and the
same runtime archive for measured programs. This compares `ac46a18e` with
`03e40cf2`, isolating milestones 5–8 from the later standalone no-spawn policy.
Local-loop runs were pinned to CPU 0; four-worker tests used CPUs 0,2,4,6
(distinct performance cores, avoiding SMT siblings and efficiency cores).

Nine paired runs alternate order and verify output. User CPU seconds below are
median [minimum, maximum]. These are native hardware measurements, not Rosetta.

| Workload | Previous helper | New inline dispatch | Median change |
| --- | ---: | ---: | ---: |
| Allocation-local loop, MLKit `-par` | 0.29 [0.29, 0.30] | 0.24 [0.24, 0.24] | -17.2% |
| Same loop, forced protection | 1.07 [1.06, 1.07] | 1.11 [1.10, 1.11] | +3.7% |
| Same loop, default ReML | 0.25 [0.25, 0.25] | 0.23 [0.23, 0.24] | -8.0% |
| Same loop, ReML `-no_par0` | 0.29 [0.29, 0.29] | 0.24 [0.24, 0.24] | -17.2% |
| Same loop, forced protection, C-only allocation | 1.27 [1.27, 1.27] | 1.27 [1.27, 1.27] | unchanged |
| Parallel merge sort | 0.13 [0.12, 0.13] | 0.11 [0.10, 0.11] | -15.4% |
| Parallel allocation/computation | 0.13 [0.13, 0.13] | 0.13 [0.13, 0.13] | unchanged |

Default ReML benefits on X64 because its previous unchecked inline allocator
saved the descriptor on the stack. The new ordinary inline path removes that
save as well; it still performs no mutex check under `par0`. The default-ReML
unit text shrinks from 509 to 500 bytes. This differs from ARM64, where default
ReML's code and measured timing were unchanged.

Compatible-helper bridge probes, median wall seconds [minimum, maximum]:

| Scenario | Previous helper | New helper | Median change |
| --- | ---: | ---: | ---: |
| One protected allocator | 0.014425 [0.013787, 0.017325] | 0.014388 [0.014015, 0.016045] | -0.3% |
| Four generated allocators, shared region | 0.082375 [0.081420, 0.083987] | 0.081034 [0.080381, 0.082894] | -1.6% |
| Mixed generated/C, shared region | 0.081917 [0.080062, 0.083348] | 0.081374 [0.079983, 0.082070] | -0.7% |
| Frequent expansion | 0.036962 [0.036098, 0.037838] | 0.036517 [0.035631, 0.037927] | -1.2% |
| Private regions through C bridges | 0.027029 [0.025414, 0.028087] | 0.026437 [0.025604, 0.026883] | -2.2% |

The bridge ranges overlap. These small differences do not establish a helper-only
speedup. Bridge timing includes filling/verification and region lifecycle work.
Whole-program timings have hundredth-second granularity, especially relevant to
the short parallel programs.

### Protected-path investigation

A second set of nine alternating runs reproduces 1.07 s baseline versus 1.11 s
candidate. Diagnostic assembly variants retain the same C runtime and verify
output on every run:

| Diagnostic | Median user CPU |
| --- | ---: |
| New unit and helper | 1.11 s |
| Remove protected helper's size guard | 1.11 s |
| Replace site's mutex/local dispatch with a direct protected call | 1.11 s |
| Keep new unit/site, use old stack-saving protected helper | 1.07 s |

The helper implementation accounts for the observed cost in this fixture more
clearly than the added dispatch or size guard. The new helper transfers values
through FP scratch registers and reconstructs the CAS expected pointer with
shift/negate/add operations, whereas the old helper keeps that pointer in rax
and restores saved inputs from the stack. These diagnostics do not distinguish
FP-transfer latency, pointer dependencies and instruction layout; they do show
that eliminating stack saves alone does not imply better protected throughput.
The regression remains explicit. No production allocator change was made as
part of this assessment.

### Native code size and correctness

The local-loop unit `.text` grows 442 → 516 bytes (+74), shared startup/helper
`.text` 1822 → 1864 bytes (+42), and executable `.text` 28530 → 28658 bytes (+128,
including linker alignment). GNU `size`'s aggregate text grows 48961 → 49089 bytes;
that aggregate also includes read-only data and unwind information. Default ReML's
unit shrinks 9 bytes, while its executable `.text` grows 64 bytes because of shared
code/alignment. Neither successful helper has explicit stack saves in the candidate;
X64's call still pushes its eight-byte return address.

The native X64 allocation suite passes: inferred/forced protection, C-only and
ReML modes, boundary/large-object cases, forced CAS failure, register sentinels,
shared/mixed generated-C allocation and assembly invariants. All 13 existing
parallel regression cases pass natively as well. These checks ran against the
milestone-8 candidate; later no-spawn policy measurements are a separate assessment.

Reproduce local comparisons with the shared command above and matching X64
baseline/candidate compilers. The manual bridge entry point is:

```sh
SML_LIB=$PWD MLKIT=$PWD/bin/mlkit MLKIT_BASELINE=/path/to/baseline/mlkit \
  taskset -c 0,2,4,6 sh src/Compiler/Backend/X64/tests/benchmark-allocation.sh
```

The existing `Arm64/tests/benchmark-parallel-programs.sh` script is portable for
these programs; set its `MLKIT_ARM64` variable to the native X64 compiler.
[Raw samples, host settings, size reports and diagnostic recipe](native-performance/inline-allocation/thinkpad/README.md).

## Assessment

Milestones 5–8 are complete. Both architectures show allocation-local gains with
code growth and a modest protected-loop regression (ARM64 +2.5%, X64 +3.7%).
The X64 investigation places its regression in the helper implementation; it
does not support adding back stack saves without a separate implementation and
broader assessment. The results support reviewing the inline-local design with
this protected-throughput tradeoff explicit. The PR remains draft for that review.
No general speedup or zero-overhead claim follows from these workloads. CI runs
correctness checks only; no performance tests or timing collection are added.

The full ARM64 regression pass comprised 260 developer, 189 GC, 89 explicit-region
and 13 parallel checks. It preceded the final protected-mode-only instruction
refinement; the complete pthread/Argobots suite and deterministic allocation
probes were rerun after that refinement. Nonparallel/profiling behavior was not
changed by the refinement.

Raw timing distributions and instruction-text sizes:
[`native-performance/inline-allocation/`](native-performance/inline-allocation/).
