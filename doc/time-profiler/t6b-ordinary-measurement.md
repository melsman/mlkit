# T6b: ordinary-build measurement

This batch predates T6d compact records and uses 65,536 32-byte records.
See [T6c/T6d measurements](t6c-optional-attribution.md) for current layouts and defaults.

Run `sh test/time_profile/measure-ordinary.sh` on macOS ARM64. `OUT`, `MLKIT`,
`PVIEW` and `REPEATS` can be overridden; five repetitions are the default and
three are the minimum. Builds and measurements run serially. Raw executables,
build logs, stdout/stderr, binary profiles and decoded records remain in `OUT`.
The controller warms each workload and deterministically shuffles settings
within each repetition. Program output must agree across settings; the busy-C
phase's deliberately nondeterministic return value is excluded from comparison.

## Comparisons

GC, GenGC and no-GC each run compute/allocation/busy-C/sleep phases, `kkb_eq`,
and twenty ML-Yacc generations of the Topdec grammar. Ordinary executables run
with sampling off, initially paused (except the explicitly started phases),
and active at 1ms, 100us and 10us. These settings use the same executable,
runtime archive and library cache. The off executable already contains passive
function metadata: this comparison measures activation cost, not the cost of
adding metadata relative to an older compiler. Executable sizes are retained;
there is no controlled pre-T6a binary-size or disabled-code timing comparison.

The phase workload also runs with 64 records at 100us, 4,096 at 10us, and the
maximum 1,048,576 at 10us. The default 65,536-record buffer costs 2 MiB;
the other capacities cost 2 KiB, 128 KiB and 32 MiB respectively, plus metadata,
stdio and other runtime memory. `/usr/bin/time -l -p` records peak process RSS,
wall/user/system seconds. RSS includes the entire program, not just the sampler.

Separate region-profiled executables run with both profilers off, region-only
at 10ms, and combined region/time at 10ms/100us. Every enabled region run must
contain snapshots; every enabled time run must have a final status and a
recorded count equal to the decoded samples. Initially paused runs must contain
zero time samples. Combined runs use the same `.rp` wire format and pview
reader. These comparisons retain the cost of the region build's instrumentation
and do not isolate the shared timer's contribution.

## Interpreting accuracy

Phase capture ratio is recorded count × requested interval / phase duration.
Visible buffer drops are reported separately. Final phase status follows pausing
and draining, so the duration includes the closing drain while the count excludes
its masked interval. Capture deficits cannot be assigned entirely to signal
coalescing. Shutdown serialization and metadata setup also contribute to total
wall time. No recorder-state samples can measure masked drains.

The harness resolves raw PCs against the recorded image/function ranges and
retains ML, named main-image C, and unknown counts. It separately counts the
busy C function in its known phase. Shared-library sleep/clock locations can
remain unknown in ordinary builds; that is a resolution limit, not evidence of
lost samples. Optional C-origin attribution is evaluated separately in T6c.

A small full buffer retains the first samples of each phase and drops later
ones until the next lifecycle drain. Its capture ratio is consequently biased
toward the phase's beginning. No drop counter measures missed/coalesced signal
expirations. Even a large buffer cannot guarantee a sample per timer expiration.

Results use child wall times at 0.01s resolution, medians and observed min/max
ranges. The controller's launch-inclusive elapsed time is also retained. Short
workloads, scheduling and output costs affect the comparison. There are no
performance thresholds in CI and these measurements are not universal bounds.

## Local results

Apple M2 Max, Darwin 25.5.0; native ARM64 executables, x64 Node controller. Five repetitions per setting, 390 runs. Raw artifacts: `/tmp/mlkit-t6b-measure`.

Child wall seconds below are median (min–max). All settings use the default buffer unless indicated. Off is the same ordinary executable without profiling activation. Paused includes metadata/output setup and buffer initialization with zero samples.

| Workload | GC | Off | Paused | 1ms | 100us | 10us |
|---|---|---:|---:|---:|---:|---:|
| phases | gc | 1.00 (1.00–1.01) | — | 1.02 (1.02–1.04) | 1.06 (1.05–1.07) | 1.20 (1.18–1.25) |
| phases | gengc | 1.00 (0.99–1.11) | — | 1.03 (1.00–1.03) | 1.04 (1.03–1.09) | 1.18 (1.15–1.45) |
| phases | no_gc | 0.94 (0.94–0.96) | — | 0.97 (0.96–0.97) | 0.98 (0.98–1.01) | 1.13 (1.11–1.14) |
| kkb | gc | 0.11 (0.11–0.11) | 0.12 (0.12–0.13) | 0.12 (0.12–0.13) | 0.13 (0.13–0.13) | 0.17 (0.17–0.19) |
| kkb | gengc | 0.14 (0.13–0.14) | 0.15 (0.15–0.15) | 0.15 (0.15–0.15) | 0.15 (0.15–0.16) | 0.20 (0.20–0.21) |
| kkb | no_gc | 0.10 (0.10–0.11) | 0.11 (0.11–0.12) | 0.12 (0.12–0.12) | 0.12 (0.12–0.12) | 0.15 (0.15–0.16) |
| mlyacc | gc | 0.36 (0.33–0.48) | 0.53 (0.44–0.56) | 0.76 (0.44–0.77) | 0.48 (0.46–0.63) | 0.61 (0.57–1.15) |
| mlyacc | gengc | 0.34 (0.33–0.34) | 0.44 (0.43–0.45) | 0.44 (0.43–0.45) | 0.46 (0.45–0.47) | 0.57 (0.56–0.61) |
| mlyacc | no_gc | 0.33 (0.32–0.33) | 0.43 (0.42–0.44) | 0.43 (0.42–0.45) | 0.45 (0.45–0.46) | 0.57 (0.57–0.57) |

Region-profiled comparisons (same workload, separate instrumented executable):

| Workload | GC | Profiled off | Region-only 10ms | Combined 10ms/100us | Region snapshots, median combined | Time samples, median combined |
|---|---|---:|---:|---:|---:|---:|
| phases | gc | 1.01 (1.01–1.02) | 1.11 (1.11–1.11) | 1.15 (1.15–1.47) | 54 | 10398 |
| phases | gengc | 1.00 (0.99–1.03) | 1.11 (1.09–1.44) | 1.15 (1.13–1.33) | 54 | 10342 |
| phases | no_gc | 0.98 (0.97–0.98) | 1.07 (1.07–1.08) | 1.11 (1.11–1.36) | 51 | 10285 |
| kkb | gc | 0.17 (0.17–0.17) | 0.22 (0.22–0.49) | 0.24 (0.23–0.25) | 18 | 1906 |
| kkb | gengc | 0.19 (0.19–0.20) | 0.25 (0.25–0.49) | 0.27 (0.26–0.28) | 21 | 2214 |
| kkb | no_gc | 0.16 (0.16–0.16) | 0.21 (0.21–0.21) | 0.22 (0.22–0.47) | 18 | 1828 |
| mlyacc | gc | 0.49 (0.42–0.56) | 1.34 (1.02–1.37) | 1.23 (1.05–1.41) | 53 | 5369 |
| mlyacc | gengc | 0.43 (0.42–0.44) | 0.98 (0.97–1.33) | 1.00 (0.99–1.57) | 45 | 4615 |
| mlyacc | no_gc | 0.60 (0.58–0.90) | 1.20 (1.20–1.21) | 1.26 (1.25–1.28) | 64 | 7256 |

Phase capture ratios aggregate all three configurations (15 observations per row). The range includes every run. Buffer/routing losses are separate from unobservable missed expirations.

| Interval / capacity | Phase | Capture ratio median (min–max) | Buffer drops across 15 phases |
|---|---|---:|---:|
| 1ms / 65536 | compute | 0.998 (0.949–0.999) | 0 |
| 1ms / 65536 | allocate | 0.997 (0.993–0.998) | 0 |
| 1ms / 65536 | c | 0.996 (0.996–0.996) | 0 |
| 1ms / 65536 | sleep | 0.998 (0.942–0.999) | 0 |
| 100us / 65536 | compute | 0.996 (0.979–0.999) | 0 |
| 100us / 65536 | allocate | 0.994 (0.949–0.999) | 0 |
| 100us / 65536 | c | 0.996 (0.931–0.999) | 0 |
| 100us / 65536 | sleep | 0.997 (0.961–0.999) | 0 |
| 10us / 65536 | compute | 0.964 (0.870–0.977) | 0 |
| 10us / 65536 | allocate | 0.968 (0.795–0.983) | 0 |
| 10us / 65536 | c | 0.977 (0.834–0.983) | 0 |
| 10us / 65536 | sleep | 0.955 (0.405–0.976) | 0 |
| 100us-small / 64 | compute | 0.019 (0.019–0.023) | 46581 |
| 100us-small / 64 | allocate | 0.037 (0.033–0.039) | 25539 |
| 100us-small / 64 | c | 0.026 (0.026–0.026) | 36397 |
| 100us-small / 64 | sleep | 0.025 (0.025–0.025) | 36757 |
| 10us-4096 / 4096 | compute | 0.097 (0.067–0.119) | 540781 |
| 10us-4096 / 4096 | allocate | 0.195 (0.129–0.225) | 247100 |
| 10us-4096 / 4096 | c | 0.164 (0.163–0.164) | 305018 |
| 10us-4096 / 4096 | sleep | 0.154 (0.154–0.158) | 304244 |
| 10us-max / 1048576 | compute | 0.969 (0.886–0.975) | 0 |
| 10us-max / 1048576 | allocate | 0.971 (0.844–0.981) | 0 |
| 10us-max / 1048576 | c | 0.976 (0.892–0.983) | 0 |
| 10us-max / 1048576 | sleep | 0.961 (0.951–0.970) | 0 |
| combined-100us / 65536 | compute | 0.996 (0.910–0.997) | 0 |
| combined-100us / 65536 | allocate | 0.995 (0.914–0.999) | 0 |
| combined-100us / 65536 | c | 0.996 (0.982–0.999) | 0 |
| combined-100us / 65536 | sleep | 0.998 (0.997–0.999) | 0 |

Peak RSS in MiB for the phase workload, median (min–max). Capacities change only sampler storage; process memory also includes ML regions, libraries and metadata.

| GC | Off | 64 records | 4,096 records | 65,536 records | 1,048,576 records |
|---|---:|---:|---:|---:|---:|
| gc | 50.30 (50.28–50.30) | 50.52 (50.47–50.52) | 50.58 (50.56–50.58) | 52.47 (52.47–52.47) | 82.47 (82.47–82.53) |
| gengc | 73.05 (73.05–73.09) | 73.22 (73.22–73.27) | 73.36 (73.36–73.41) | 75.25 (75.23–75.31) | 105.25 (105.23–105.28) |
| no_gc | 246.33 (246.33–246.34) | 246.48 (246.48–246.53) | 246.63 (246.61–246.67) | 248.50 (248.50–248.53) | 278.50 (278.50–278.52) |

Default-buffer output and losses (five runs each):

| Workload | GC | Interval | Median samples | Median file bytes | Buffer drops, total | Routing drops, total |
|---|---|---|---:|---:|---:|---:|
| phases | gc | 1ms | 990 | 201203 | 0 | 0 |
| phases | gc | 100us | 10231 | 764904 | 0 | 0 |
| phases | gc | 10us | 111791 | 6960064 | 0 | 0 |
| phases | gengc | 1ms | 979 | 200628 | 0 | 0 |
| phases | gengc | 100us | 10037 | 753166 | 0 | 0 |
| phases | gengc | 10us | 109085 | 6795094 | 0 | 0 |
| phases | no_gc | 1ms | 939 | 184954 | 0 | 0 |
| phases | no_gc | 100us | 9561 | 710896 | 0 | 0 |
| phases | no_gc | 10us | 106133 | 6601788 | 0 | 0 |
| kkb | gc | 1ms | 110 | 86547 | 0 | 0 |
| kkb | gc | 100us | 1180 | 151817 | 0 | 0 |
| kkb | gc | 10us | 14877 | 987334 | 0 | 0 |
| kkb | gengc | 1ms | 134 | 88107 | 0 | 0 |
| kkb | gengc | 100us | 1393 | 164906 | 0 | 0 |
| kkb | gengc | 10us | 16194 | 1067767 | 0 | 0 |
| kkb | no_gc | 1ms | 104 | 80855 | 0 | 0 |
| kkb | no_gc | 100us | 1099 | 141550 | 0 | 0 |
| kkb | no_gc | 10us | 12890 | 860801 | 0 | 0 |
| mlyacc | gc | 1ms | 609 | 721363 | 0 | 0 |
| mlyacc | gc | 100us | 3700 | 909914 | 0 | 0 |
| mlyacc | gc | 10us | 47940 | 3608554 | 21309 | 0 |
| mlyacc | gengc | 1ms | 333 | 704624 | 0 | 0 |
| mlyacc | gengc | 100us | 3477 | 896408 | 0 | 0 |
| mlyacc | gengc | 10us | 44489 | 3398140 | 0 | 0 |
| mlyacc | no_gc | 1ms | 325 | 675458 | 0 | 0 |
| mlyacc | no_gc | 100us | 3412 | 863765 | 0 | 0 |
| mlyacc | no_gc | 10us | 41573 | 3191586 | 0 | 0 |

[Per-run measurements](t6b-measurements.json) include CPU times, peak RSS, executable/file sizes, raw-PC resolution counts, explicit phase boundaries, capture ratios and losses. [Summary](t6b-summary.json) retains median/min/max values and ratios against ordinary off; those ratios include activation/setup and output costs.

## Assessment

For the phase workload, ordinary active sampling increased median wall time by
2–3% at 1ms, 4–6% at 100us and 18–20% at 10us relative to off. The phase
capture medians at 1ms and 100us were above 99%, with zero visible drops;
individual observations were lower. At 10us, medians fell to roughly 95–98%
and the default buffer overflowed in two GC ML-Yacc runs (21,309 drops total).
The maximum buffer eliminated visible phase overflow but did not recover missed
expirations or masked drain intervals. Capacity should be chosen for the longest
interval between drains, not the program's aggregate recorded sample count.

The default increased phase peak RSS by about 2.2 MiB; the maximum increased it
by about 32.2 MiB. This agrees with one touched record buffer plus other
activation storage. The default phase reports grew from about 0.18–0.20 MB at
1ms to about 6.6–7.0 MB at 10us. Larger buffers also preserve more data for later
serialization, so they do not remove output cost.

Ordinary `kkb_eq` at 1ms took 0.12/0.15/0.12s for GC/GenGC/no-GC,
versus 0.11/0.14/0.10s off. The corresponding profiled-off builds took
0.17/0.19/0.16s. This supports separating ordinary sampling from the region
build's instrumentation, but does not make activation free. Paused ML-Yacc
already took 0.53/0.44/0.43s versus 0.36/0.34/0.33s off. Metadata discovery,
encoding/output and buffer setup are included in that difference. Those costs
were not individually timed. Sampling starts after metadata initialization, so
recorded samples do not describe that startup work. Main-image catalog construction currently scans
ML ranges and symbols for each function start and is a candidate for a future
startup optimization; this batch does not establish its share of setup cost.

The GC ML-Yacc timing range is especially broad: off varied from 0.33–0.48s
and 1ms from 0.44–0.77s. Settings are shuffled and all observations are retained;
the resulting non-monotonic interval comparison is not evidence that 1ms is
intrinsically slower than 100us. The batch cannot cleanly separate machine
variation, scheduling and profiler perturbation. Neither these results nor the
historical T6 batch justify a universal overhead bound.

In ordinary 1ms phase runs, 94.8–98.0% of the busy-C phase's recorded PCs
resolved directly to `tp_measure_busy` (median 96.0%). Its clock calls and other
locations receive their own rows or remain unknown. All 45 combined runs
contained region snapshots and time samples, with final count consistency,
output parity and zero visible buffer/routing drops. All paused runs recorded
zero samples. No routing drops occurred anywhere in this batch.

Generated time-only and combined reports passed pview's DOM checks for
categories, denominators, shared time filtering, view switching and empty
intervals; combined profiles were checked for all three GC configurations.
Browser visual layout remains unverified. No runtime/compiler behavior changes
were needed for T6b. Keep 1ms as the starting interval for opt-in investigation;
use finer intervals with attention to startup cost, loss counters and capacity.
T6c's optional detailed C attribution remains separate work.

Command examples use current T8 names (`pview`, `-p_file`). Archived measurement JSON retains the historical command names used when those measurements were collected; no measurements were rerun for this rename.
