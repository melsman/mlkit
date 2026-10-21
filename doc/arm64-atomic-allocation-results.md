# ARM64 atomic allocation: validation and measurements

The generated parallel allocation helper now attempts a reservation before the
full preserving runtime path. The successful path saves only `x0` and LR in a
16-byte frame. It uses the existing runtime allocator on contention, expansion,
and large objects. The [design and X64 comparison](arm64-atomic-allocation-plan.md)
describe the concurrency and register-preservation invariants.

## Validation

Validation ran on macOS 26.5.1, native ARM64, with Apple clang 21.0.0. MLKit and
ReML were built with the native MLKit seed from the baseline source checkout;
all runtime variants used the same worktree sources. Argobots 1.2 was built from
its official release archive with `--disable-checks --disable-shared` in `/private/tmp/mlkit-argobots-release`.

- The full native ARM64 suite passes: MLKit/ReML calls, closures, regions,
  exceptions, floating-point spills, large frames, finite and dynamic regions,
  allocation/reset probes, forced GC and generational GC, profiling, callbacks,
  incremental REPL collection/recovery, metadata, and unsupported-option guards.
- The developer suite passes 260 checks across its ordinary and profiling test
  invocations, including its GC cases.
- The ordinary GC regression suite passes 189 cases.
- The parallel regression suite passes 13 cases.
- The explicit-region regression suite passes 89 cases.
- Targeted parallel checks cover pthreads and Argobots with one and four
  execution streams, MLKit/ReML, inferred/forced/private protection, profiling,
  callbacks, and incremental REPL helper linkage.
- Generated-code probes check exact fills, expansion, large objects, finite
  regions, reset modes, tag-free pointer adjustment, and integer/FP sentinels,
  including `x0`–`x3`. A modified test image inserts `clrex` before every
  `stlxr`, deterministically testing the failure-to-C handoff.
- Concurrent fixtures retain and verify all payloads while generated and C
  allocators share a protected region. They exercise all-generated allocation,
  alternating C/generated allocation, frequent page expansion, and private
  regions without a mutex.

The native and broad regression suites ran before the final frame reduction;
the final helper is covered by the subsequent complete targeted parallel rerun.
The reduction changes only the non-profiled protected-parallel helper; ordinary
GC and nonparallel code generation are unchanged by that refinement.

Commands (from the configured checkout, with absolute paths exported):

```sh
export SML_LIB="$PWD"
export MLKIT_ARM64="$PWD/bin/mlkit" REML_ARM64="$PWD/bin/reml"
export ARM64_EMITTER="$PWD/bin/arm64-emitter-test" KITTESTER="$PWD/bin/kittester"
export ARGOBOTS_ROOT=/private/tmp/mlkit-argobots-release
make mlkit emitter
make -C src/Runtime runtimeSystemArPar.a runtimeSystemArParProf.a \
  ARGOBOTS_ROOT="$ARGOBOTS_ROOT"
sh src/Compiler/Backend/Arm64/tests/check-native.sh
sh src/Compiler/Backend/Arm64/tests/check-parallel.sh
TEST_JOBS=4 REGRESSION_SUITES='parallel dev gc explicit' \
  sh src/Compiler/Backend/Arm64/tests/check-regressions.sh
```

The Argobots script also fixes an existing stale runtime option: execution
stream counts use `+RTS -par_threads N -RTS`; `-p` now selects profiling.
Profiling tests inspect the generated link unit to ensure the new helper is
excluded. Parallel GC and tagged-parallel configurations remain unsupported.

## Measurement method

Measurements use the final 16-byte helper after correctness workloads finish.
Each benchmark warms both variants, alternates their order over nine paired
runs, and checks outputs. Raw samples are saved under
`arm64-performance/atomic-allocation/`.

`benchmark-allocation.sh` compares the final generated helper with a baseline
image obtained by removing only its assembly fast prefixes. Both variants use
the same 320-byte preserving stubs, C wrapper, runtime archive, C workload,
compiler options, and private ABI. This isolates allocator behavior rather than
changes elsewhere in the compiler. The C fixture retains, fills, and verifies
allocated objects. Its monotonic wall-clock interval also includes region
creation, thread startup/join when applicable, validation, and region cleanup;
it is not an allocation-instruction latency measurement. It runs 131,072
allocations per worker (one worker for `single`, four otherwise), of 32 words.
The expansion case uses 4,096 allocations per worker of 1,000 words. `mixed`
alternates generated and C allocation; `private` allocates separate mutex-free
regions in each worker. The shared cases intentionally place heavy contention
on one allocation pointer.

`benchmark-parallel-programs.sh` compares the unchanged compiler from the
baseline checkout at `deffec47` with the implementation compiler, using identical
`-no_gc -par --no_basislib` flags and separate caches. It uses the existing
`spawn_msort.sml` (500,000 elements, eight-way parallel sorting) and
`spawn_alloccomp.sml` (eight workers, 200,000-element lists, reduced from 100 to
10 computation rounds for practical repeated measurements). Both compile and
execute without importing the Basis. Baseline and new output must match;
merge sort must additionally print `ok`. `/usr/bin/time -p` supplies process
wall/user/system times at 0.01-second resolution, so small differences should
not be overinterpreted.

```sh
sh src/Compiler/Backend/Arm64/tests/benchmark-allocation.sh
MLKIT_BASELINE=/Users/mael/gits/mlkit/bin/mlkit \
  sh src/Compiler/Backend/Arm64/tests/benchmark-parallel-programs.sh
```

## Generated code and frame costs

The parallel link image gains 284 bytes of instructions: 35 instructions for the
ordinary helper prefix and 36 for the pointer-adjusting variant. Allocation
sites retain their existing call sequence and the runtime fallback is retained.
The protected success path executes 22 instructions through return, including
one paired save and one paired restore. It uses 16 stack bytes; the previous
preserving stub uses 320 bytes before entering its C-ABI wrapper, which uses
another 48 bytes. Failure removes the 16-byte frame before tail-transferring to
the old stub, so these frames do not accumulate.

X64's corresponding protected path explicitly saves 24 bytes (region, word
count, `rax`) plus the return address pushed by `call`, for 32 bytes total.
Its unprotected path saves 16 bytes plus the return address, for 24 total.
ARM64 includes LR in its 16-byte frame and reconstructs the old pointer after
reservation, avoiding extra saved arithmetic registers. Both implementations
make one attempt and use the same C allocator for retry/expansion.

## Results

Medians of nine paired runs; percentages compare elapsed time, so positive
values denote a regression. These short C-driven cases deliberately stress one
protected allocation pointer; memory filling and verification are included.

| Allocation scenario | C-only baseline (ms) | Assembly fast path (ms) | Change |
| --- | ---: | ---: | ---: |
| Single protected allocator | 6.594 | 8.273 | +25.5% |
| Four generated allocators, one shared region | 52.808 | 57.247 | +8.4% |
| Mixed C/generated, one shared region | 47.392 | 58.155 | +22.7% |
| Frequent expansion | 23.888 | 23.593 | -1.2% |
| Separate mutex-free private regions | 13.379 | 13.434 | +0.4% |

The first three regressions are visible across the run ranges; expansion and
private-region changes are small relative to run variability. The new scheme
removes saves and C calls on success, but that alone does not guarantee faster
allocation. Disassembly shows that this host's C runtime uses LSE `casal`, while
the generated helper uses an exclusive-load/store reservation. This is a
possible contributor, alongside the extra fallback work on failed reservations;
these measurements do not isolate the causes. The implementation deliberately
keeps the baseline-compatible exclusive instructions and a bounded attempt.

The whole-program measurements show benefits for both selected programs:

| Program | Baseline user CPU (s) | New user CPU (s) | Change | Baseline wall (s) | New wall (s) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Parallel merge sort | 0.24 | 0.11 | -54.2% | 0.05 | 0.03 |
| Parallel allocation/computation (10 rounds) | 0.16 | 0.12 | -25.0% | 0.02 | 0.01 |

All measured outputs match. CPU time aggregates the worker threads. Wall-clock
figures are particularly coarse for these short runs; CPU ranges were
0.23–0.24 versus 0.11–0.12 seconds for merge sort and 0.16–0.19 versus
0.12–0.13 for allocation/computation. These are selected workloads on one host,
not a general parallel-performance claim. Programs can benefit from the cheap
mutex-null path as well as protected reservations, whereas the shared C-driven
cases concentrate contention on one region. The current draft therefore has a
measured protected-allocation regression for reviewers to consider alongside
the whole-program improvements. LSE-based reservation or a different retry policy
would require a separate measured change, not an assumed fix.

Raw data: [allocation samples](arm64-performance/atomic-allocation/allocation-samples.csv)
and [whole-program samples](arm64-performance/atomic-allocation/program-samples.csv).
The earlier exploratory 48-byte helper measurements and concurrent-test-load
runs are excluded from these results.
