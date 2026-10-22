# Saving the original pointer: X64 investigation

Measured 2026-10-22 on the same ThinkPad and common runtime as the parent
milestone-8 assessment, on AC power with the balanced profile. The candidate
was tested by modifying emitted helper assembly and relinking the unchanged
forced-protection program unit. The production allocator is unchanged.

The variant saves the old allocation pointer in xmm15 instead of the word count.
After the space test, it restores the CAS expected pointer directly, removing
shift/negate/add from success. On no-space or CAS failure it reconstructs the
word count as `(new_pointer - saved_old_pointer) >> 3`. It must use the saved
pointer: failed CAS replaces rax with the current shared allocation pointer.
Both protected and compatible unprotected helper paths use the same variant.
It retains the existing size guard, register ABI and absence of explicit stack saves.

All three versions use the same C runtime. `stack` is the pre-milestone-5 compiler
(`ac46a18e`); `previous` is the milestone-8 implementation (`03e40cf2`);
`pointer` is its unchanged program unit with the modified helper. Later no-spawn
policy changes are excluded. The two frame-free variants differ only in helper
assembly, including its resulting code placement.

Nine runs per variant alternate forward/reverse order and verify outputs.
`forced.csv` uses CPU 0. `bridges.csv` uses CPUs 0,2,4,6, distinct performance
cores. Time granularity and the bridge workload's timing scope are described
in the parent README. The latter uses 131072 allocations per worker.

## Results

Forced loop user CPU seconds, median [minimum, maximum]:

| Helper | User CPU |
| --- | ---: |
| Original stack-saving helper | 1.06 [1.06, 1.07] |
| Current helper, saving size | 1.11 [1.10, 1.11] |
| Experimental helper, saving pointer | 1.11 [1.11, 1.11] |

Bridge median wall seconds:

| Scenario | Stack | Current | Pointer |
| --- | ---: | ---: | ---: |
| One protected allocator | 0.014936 | 0.014679 | 0.014975 |
| Four generated allocators, shared region | 0.082074 | 0.082861 | 0.082713 |
| Mixed generated/C, shared region | 0.082311 | 0.081436 | 0.081920 |
| Frequent expansion | 0.037174 | 0.036501 | 0.036910 |
| Private regions through C bridges | 0.026820 | 0.026811 | 0.026858 |

Bridge ranges overlap. The pointer variant has no measured forced-loop gain and
no convincing bridge benefit. It shifts more arithmetic onto fallback and does
not resolve the protected-throughput regression, so it was not retained.
This result weakens the specific hypothesis that successful-path pointer
reconstruction is responsible for the regression. It does not identify the
remaining cause or establish that FP transfers alone explain it.

## Correctness and reproduction

The modified emitted helper passes existing generated/C allocation checks:
protected/private exact fills, expansion and large objects; rax/xmm13 sentinels;
four-worker shared/mixed allocation with retained data verification; and forced
CAS failure into runtime fallback. Those checks exercise restoration even when
CAS overwrites rax. The experiment used emitted assembly directly; no compiler build or full
regression rerun was performed. No production code was retained.

The scripts are manual experiments for the recorded milestone-8 sources and
fixture, not general assembly rewriting tools. Given the parent assessment's
source/build directory and retained forced-loop benchmark:

```sh
M8_ROOT=/tmp/mlkit-m8-20261022 taskset -c 0 sh benchmark-pointer.sh
# Set this to the output directory printed by the first script:
M8_ROOT=/tmp/mlkit-m8-20261022 POINTER_BENCHMARK=/tmp/mlkit-pointer-helper.XXXXXX \
  taskset -c 0,2,4,6 sh benchmark-bridges.sh
```

`helper.txt` records executable/object sizes and the modified helper assembly.
`forced.csv` and `bridges.csv` retain every sample. No timing collection is
added to CI.
