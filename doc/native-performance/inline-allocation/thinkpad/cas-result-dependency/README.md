# X64 allocation result independent of CAS output

Native ThinkPad measurements, 2026-10-22, Intel i7-1360P on AC power with the
balanced profile. This investigation follows the independently re-verified
protected-loop regression and the earlier pointer-saving/alignment experiments.
No timing collection or performance tests are added to CI.

## Hypothesis and assembly experiments

The old stack-saving helper computes its result from the proposed pointer and
saved size. The current frame-free helper used `movq %rax,%r11` after successful
CAS: rax is an implicit output of `cmpxchg`. A possible dependency through that
output could delay the caller's result/address computation.

Using the unchanged freshly rebuilt milestone-8 main object and common runtime,
three emitted-assembly variants were tested, with nine alternating short runs:

| Helper | Median user CPU [minimum, maximum], seconds |
| --- | ---: |
| Original stack-saving baseline (`ac46a18e`) | 1.07 [1.06, 1.07] |
| Current frame-free helper (`03e40cf2`) | 1.11 [1.10, 1.11] |
| Recompute result from proposed pointer and saved size | 1.10 [1.09, 1.10] |
| Save old pointer, still return rax (control) | 1.11 [1.11, 1.11] |
| Save old pointer, return it directly from xmm15 | 1.11 [1.10, 1.11] |

The direct pointer-return variant does not recover the regression. The independent
reconstruction variant has a small benefit, so it was confirmed with five balanced
blocks of a five-times-longer program (ten samples per candidate, five old-baseline
references): current 5.555 [5.54, 5.56], recompute 5.50 [5.50, 5.50], old baseline
5.35 [5.35, 5.35]. Half-hundredth medians average the two central rounded samples;
they do not imply additional timer precision.

All three assembly variants pass protected/private boundaries, expansion and large
objects, register sentinels, concurrent generated/C allocation/data verification,
and forced CAS failure. Recipes and raw samples are retained in `assembly-short/`
and `assembly-long/`. These experiments change instruction positions as well as
data dependencies; they do not prove that CAS-output latency alone explains the
measured difference. Most of the original approximately 4% regression remains.

## Retained implementation

Only protected success changes. After `cmpxchg` and its failure branch, the helper
constructs the old pointer from sources established before CAS:

```asm
movq %xmm15,%r11      # saved word count
salq $3,%r11
negq %r11
addq %r10,%r11        # proposed pointer - 8*words
movq %xmm14,%rax      # restore caller's rax
ret
```

This eliminates the result's register dependency on CAS's rax output without
changing pre-CAS reservation arithmetic, fallback, private/unprotected success,
register palette, descriptor layout or flags. It adds three instructions and
12 bytes to the shared helper. The measured MLKit program unit remains 532 bytes;
shared link text grows 1920 to 1932 bytes and GNU aggregate executable text grows
49889 to 49953 bytes, including alignment. There are no explicit stack saves.

The existing forced-failure correctness check now also perturbs CAS's returned
rax value with LEA (preserving flags and the descriptor), checking restoration
when that returned pointer differs from the pre-attempt pointer. This is a
correctness check only. Native allocation checks and all 13 parallel regressions
pass with the compiler-generated implementation.

## Current PR compiler confirmation

Before and after compilers were built natively from the current PR source
(`8fd08660`, followed by the CodeGen change) with the same installed MLKit v4.7.15
seed. Compiler/runtime/source hashes are in `host-and-hashes.txt`.

Both compilers use the same runtime archive. Unlike the earlier milestone-8
comparison, this source includes the later no-spawn policy. The benchmark retains
an unexecuted spawning function via an effectful unit initializer, and asserts
`mlkit_regions_may_spawn == 1` before the loop. Thus absence-of-spawning analysis
cannot disable protection. Both before/after invocations force protection and
use fresh separate caches; their output is checked on every run. The spawning
function is never executed. All builds finish before timing.

Five ABBA/BAAB blocks give ten samples per version/compiler/workload, CPU 0.
Median user CPU seconds [minimum, maximum]:

| Compiler / workload | Before | After |
| --- | ---: | ---: |
| MLKit / 100M cells | 1.11 [1.10, 1.11] | 1.10 [1.09, 1.10] |
| MLKit / 500M cells | 5.55 [5.54, 5.57] | 5.50 [5.50, 5.50] |
| ReML `-no_par0` / 100M cells | 1.10 [1.10, 1.11] | 1.10 [1.09, 1.10] |
| ReML `-no_par0` / 500M cells | 5.56 [5.54, 5.57] | 5.50 [5.50, 5.50] |

The long cases improve about 0.9% and 1.1% respectively. The result is a modest
improvement, not elimination of the original regression. Short ReML medians tie
at this timer resolution.

## Other comparisons and controls

Each comparison below has nine alternating paired runs with verified output.
Bridge values are median wall seconds; whole programs/controls use user CPU.
Here `baseline` in CSV files means the before-fix current-PR compiler, and `fast`
means after-fix, not the original pre-milestone-5 compiler.

| Workload | Before | After |
| --- | ---: | ---: |
| One protected allocator through C bridge | 0.014663 | 0.014747 |
| Four generated allocators, shared region | 0.080896 | 0.080723 |
| Mixed generated/C, shared region | 0.081442 | 0.081315 |
| Frequent expansion | 0.035972 | 0.035819 |
| Private regions through C bridges | 0.026770 | 0.026281 |
| Parallel merge sort | 0.11 | 0.11 |
| Parallel allocation/computation | 0.13 | 0.13 |
| Local allocation, MLKit `-par` | 0.24 | 0.24 |
| Default ReML `-par -par0` | 0.23 | 0.23 |

Bridge ranges overlap; no broad helper speedup or material regression is inferred
from these small differences. Whole-program controls have hundredth-second
resolution. All samples and size reports are retained, without discarded samples.

## ARM64 comparison

ARM64 does not have the analogous result-register dependency: `ldaxr` loads the
pointer into x0, arithmetic forms the proposed pointer there, and `stlxr w30,x0,[x16]`
writes only the status to w30. On success, `sub x16,x0,x17,lsl #3` computes the
result independently of that status output. The status branch remains necessary.
ARM64 code is unchanged. Its earlier measured 2.5% protected regression is not
explained or fixed by this X64-specific experiment; no new ARM64 timing claim is
made here.

## Reproduction

`experiment-assembly.sh` and `confirm-assembly-long.sh` take `M8_ROOT` and
`REVERIFY_BENCHMARK` pointing to the preserved milestone-8 and re-verification
output directories. Run under `taskset -c 0`; they verify outputs and save samples.

For the compiler comparison, `FIX_ROOT` names a current PR build with `bin/mlkit`
and `bin/reml` after the change, and saved `bin/mlkit-before-result` and
`bin/reml-before-result` executables. Run `benchmark-compiler.sh`, preferably
under a temporary idle inhibitor. Existing shared benchmark scripts reproduce
the helper, parallel-program and control comparisons. All remain manual.

## Branch prediction follow-up

Branch prediction remains an unconfirmed explanation for the remaining gap.
Earlier removal of the allocation-site dispatch and helper size check did not
recover the baseline timing. Alignment sensitivity alone cannot distinguish
branch prediction from instruction-fetch effects. A per-process user-space
`perf stat` attempt with cycles, instructions, branches and branch-misses was
denied by the ThinkPad kernel (`perf_event_paranoid=4`); no counter measurements
were obtained and no system settings were changed.
