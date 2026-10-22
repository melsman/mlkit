# X64 protected-helper alignment and register-save experiments

Manual experiments on the ThinkPad, 2026-10-22. They use the same milestone-8
source snapshots, common runtime, forced-protection program unit and CPU settings
as the parent assessment. No production allocator changes are part of these tests.

## Controls

The modified protected helper occupies a fixed 256-byte arena starting on a
64-byte boundary. Bytes before its entry and after its unconditional runtime
fallback jump are padding; that padding is never executed. Its entry can move
by 0–63 bytes while caller addresses, downstream code addresses and executable
text size remain fixed. The compatible unprotected path is unchanged.
`addresses.csv` records the protected entry, CAS, allocation caller, downstream
`__raise_overflow` and `.text` size for every generated variant. The script
checks that caller/downstream addresses and total text size are identical.

All measured programs use the same main object and C runtime archive. CPU 0 is
a performance core; the experiment is pinned there on AC power with the balanced
profile. Before timing, the script builds and executes every variant and runs
protected/private allocation boundaries, large objects, rax/xmm13 sentinels,
shared/mixed generated-C allocation and forced CAS failure with all four save
strategies. The concurrent checks use distinct performance cores 0,2,4,6.
Every timed execution must print the expected result.

## Experiment 1: alignment only

The protected helper's instructions are unchanged. Its entry takes every byte
offset from 0 through 63 within the arena. Three screening rounds use ascending,
descending and rotated order; the original current and old-helper executables
are interleaved as references after every sixteen samples. This is a complete
byte-offset screen at hundredth-second timing resolution, not a guarantee that
all code-layout effects are absent in other workloads.

## Experiment 2: save method with unchanged allocation arithmetic

Four methods use the same size guard, pointer/page-limit computation,
shift/negate/add reconstruction, CAS, result calculation and runtime fallback:

- `fp`: current helper, saving rax in xmm14 and size in xmm15.
- `stack-rax`: save/restore only rax with push/pop; size remains in xmm15.
- `stack-size`: save only size on the stack; rax remains in xmm14.
- `stack-both`: save rax and size on the stack.

These change save/load instructions and necessary stack bookkeeping only. The
stack is restored before returning or entering the runtime fallback; early size
rejection occurs before saves. Each method is measured at entry offsets 0,16,32,48,
nine runs per method/offset, alternating forward/reverse ordering. Original
current and old-helper executables provide reference timings in each round.
The save instructions have different lengths: CAS is at entry+64 for `fp`,
entry+60 for either single-stack method, and entry+56 for `stack-both`. This
residual internal layout change is recorded rather than assumed irrelevant.

## Results

The complete alignment screen gives per-offset median user CPU of 1.11 s for
61 offsets, 1.10 s for offsets 38 and 44, and 1.14 s for offset 17. Thus alignment
can make this helper slower, but none of the tested offsets reproduces the old
helper's approximately 1.07 s timing. An independent nine-run confirmation of
these selected offsets is recorded in `confirmation.csv`:

| Confirmation case | Median user CPU [minimum, maximum] |
| --- | ---: |
| Offset 0 | 1.11 [1.10, 1.11] |
| Offset 17 | 1.14 [1.14, 1.14] |
| Offset 38 | 1.11 [1.11, 1.11] |
| Offset 44 | 1.11 [1.10, 1.11] |
| Original current executable | 1.11 [1.10, 1.11] |
| Original old-helper executable | 1.07 [1.06, 1.07] |

The apparent best-offset gain in the three-run screen did not persist in this
independent confirmation. The slow offset did.

The save-method comparison gives the following median user CPU seconds, nine
runs per entry offset:

| Save method | Offset 0 | Offset 16 | Offset 32 | Offset 48 | Full range |
| --- | ---: | ---: | ---: | ---: | ---: |
| Both in FP registers | 1.11 | 1.11 | 1.11 | 1.11 | 1.10–1.11 |
| Only rax on stack | 1.11 | 1.11 | 1.11 | 1.10 | 1.10–1.11 |
| Only size on stack | 1.12 | 1.12 | 1.12 | 1.12 | 1.12–1.13 |
| Both on stack | 1.12 | 1.12 | 1.12 | 1.12 | 1.12–1.12 |

Interleaved original executables give median 1.11 s for the current helper
(range 1.10–1.11) and 1.07 s for the old helper (range 1.06–1.07), nine references
each in this save-method phase. The small apparent rax-only improvement does not
persist across entry offsets; its pooled median over all 36 samples is 1.11 s.
Stacking the size or both values is slightly slower with the new arithmetic.

The hypotheses are narrowed, not settled: simple protected-entry alignment and
replacing FP saves by stack saves do not recover the original performance.
The old helper also differs in pointer/register data flow and instruction
scheduling; these experiments do not identify a specific remaining bottleneck.
The save-method test still changes internal instruction positions and stack
bookkeeping. It does not prove that integer/FP transfers have zero cost.
No production change is supported by these results, so the allocator is unchanged.

## Interruption and retained data

The ThinkPad suspended during alignment round 2, offset 11. Its raw sample has
wall time 5326.32 s and is retained in `raw-samples.csv` and `exclusions.csv`.
After wake-up the run completed, with a temporary idle inhibitor for its remaining
work. `repeat-interrupted.sh` repeated the identified sample after the main run;
`replacement.csv` records the repeat (user 1.10 s). `samples.csv` substitutes that
one repeat and otherwise contains every original sample unchanged. No other
outlier was discarded. The independent confirmation runs also use a temporary
idle inhibitor. Inhibitors end with their benchmark processes and change no
persistent power settings.

`addresses.csv` contains 76 variants. In all cases, caller address is 0x279a,
downstream entry is 0x3000 and executable `.text` is 28914 bytes. The protected
arena starts at 0x2f00. `run.log` retains the address/correctness/round completion
checks; `host-status.txt` records the final host state.

## Reproduction

Given the retained milestone-8 directory (including its `forced.log` pointing to
the original forced-loop benchmark), run:

```sh
M8_ROOT=/tmp/mlkit-m8-20261022 taskset -c 0 sh benchmark.sh
```

The script reports its output directory, verifies address controls and correctness,
and writes `samples.csv` and `addresses.csv`. These are diagnostic assembly
transformations for the recorded fixture, not production compiler changes.
After screening, `confirm-offsets.sh` measures the selected fast/slow offsets
independently. Set `LAYOUT_BENCHMARK` to the generated experiment directory and
run it under `taskset -c 0`. All recipes remain manual; no timing collection is
added to CI.
