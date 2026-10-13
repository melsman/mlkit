# T6: single-thread measurement and hardening

This is a historical milestone record. Later milestones supersede some of its
implementation and validation details; see the [final design and milestone map](t8-unified-profiling.md)
and [Linux X64 support](t9-linux-x64.md). Retained measurements describe the
binaries measured at that milestone, not later changes.

These measurements describe the earlier profiled-build, double-buffer prototype.
T6a supersedes that recorder/build design; see the
[T6c/T6d measurements](t6c-optional-attribution.md) and [T9 Linux measurements](t9-linux-x64.md)
for the later layouts and measured costs.

This milestone evaluates the experimental static, single-thread macOS ARM64 wall
sampler. It does not add CPU timers, parallel delivery or dynamic REPL images.

## Reproduction

Run `sh test/time_profile/measure.sh` on macOS ARM64. `OUT`, `MLKIT`, `PVIEW`
and `REPEATS` may be overridden; at least three repetitions are required, with
five the default. The harness builds GC/non-GC plain and profiled executables,
uses fresh ML-Yacc library sources to avoid unrelated local caches, warms each
workload, and deterministically shuffles settings within each repetition.

Settings distinguish a plain compiler build, a profiled build with sampling off,
a paused recorder (except workloads that explicitly start their own phases),
and active 1ms, 100us and 10us intervals. Callback/exception workloads use the
profiled-off build as their baseline because their C assertions exercise the
boundary instrumentation itself. ML-Yacc generates MLKit's Topdec parser twenty
times per invocation. `kkb_eq` runs its original three completions. A phase
workload brackets ML compute, allocation with retained live data, busy C and
restartable nanosleep with start/pause status records. The callback workload is
the existing nested callback/C-raised-exception/GC-policy regression program.

Raw stdout, stderr, binary profiles, decoded records and elapsed/user/system
measurements are retained. Non-phase program output is compared across settings.
The controller records launch-inclusive elapsed time; `/usr/bin/time -p`
provides child wall and CPU measurements at 0.01s resolution. The report uses
child wall time for overhead comparisons. The local Node controller is X64
under translation; the measured ML executables are native ARM64. Run performance batches serially, without concurrent
builds or tests. Report medians and the observed min/max range, not exact overhead
thresholds for CI. Short runs are sensitive to scheduling, filesystem and startup
cost; measurements describe this machine and workload, not a universal bound.

## Memory, output and cost

On the supported ARM64 ABI, TimeRecord is 32 bytes (PC, tick, state, padding,
origin PC). Two fixed buffers cost `64 × capacity` bytes: 256 KiB at the default
4096 entries, and 64 MiB at the maximum 1,048,576. They are allocated/touched
outside the handler and retained for process lifetime. Metadata and stdio add
separate memory costs. Each binary time sample uses 61 bytes, plus status/header
and executable function metadata. Output growth therefore depends on *delivered*
samples, not elapsed/requested-interval alone.

The profiled-off versus plain comparison includes compiler boundary and safe-point
instrumentation. Active versus profiled-off additionally includes timer delivery,
handler work, drains and output. These measurements do not isolate the handler
from kernel delivery or serialization. Recorder-state samples provide a coarse
observation of drain time; zero such samples does not prove drains are free.
No per-signal diagnostic timestamps are inserted into production code merely to
measure the handler, because that would perturb the quantity being measured.

## Accuracy and uncertainty

Phase start/pause records establish independent known boundaries. Compare each
phase's recorded count × requested interval against its recorded duration, and
compare equal-duration busy-C and sleeping phases across repetitions. A tighter
interval can increase captured detail while increasing perturbation; it does not
promise proportionally more independent observations. Standard SIGALRM delivery
can coalesce. Buffer/routing drops are observable, coalescing is not. Samples are
correlated with execution and timer scheduling, so a naive binomial confidence
interval is not a calibrated uncertainty bound. Report observed run-to-run
variation and phase capture ratios instead.

The wall coordinate is Mach uptime and excludes machine sleep. It includes
blocking, handler work and recorder drains. Restarting nanosleep after every
signal can increase wall duration. Sampling a C primitive does not establish
CPU consumption. Function totals remain count-based estimates, and unknown PCs
stay in the denominator. Percentages cannot reconstruct lost expirations.

## Signal/timer interaction and focused checks

The sampler owns SIGALRM/ITIMER_REAL and shares them with region profiling,
using independent deadlines. Existing timers or non-default handlers and a
blocked SIGALRM at initialization are rejected. A C primitive that masks SIGALRM
later causes coalescing; one that replaces the handler/timer violates the sampler's
ownership contract. Foreign pthread delivery is rejected and counted, rather
than being attributed to the ML thread. Parallel ML and REPL recording are
rejected. GC and non-GC single-thread configurations are supported.

`check-conflicts.sh` checks startup mask/handler/timer conflicts in both builds.
The conflict diagnostic now says to disable time sampling or release the
conflicting facility; setting the region interval to zero cannot disable `-tp`.
`check-phases.sh` checks explicit compute/allocation/C/sleep boundaries at 1ms and
100us without demanding exact percentages. Existing recorder tests cover small
buffers/overflow, signal masking, slow output, pause/flush, independent region
frequency and malformed input. Attribution tests cover nested callbacks, caught
C exceptions and restored GC policy. These focused tests run in profiler CI.

The linked CI failure was a missing Code.sml dependency in the staged
pretty-print reader test; its MLB and Makefile staging/dependencies now include it.

## Local results

Apple M2 Max, Darwin 25.5.0; five repetitions per setting. Native ARM64 ML
executables; the controller runs under X64 translation. The table reports median
child wall seconds and observed min–max, including startup. Callback overhead
is relative to profiled-off; other overhead is relative to plain.

| Workload | GC | Setting | Median s (range) | Overhead | Median samples | Median output bytes | Drops across 5 runs |
|---|---|---|---:|---:|---:|---:|---:|
| phases | gc | plain | 0.97 (0.97–0.98) | 0.0% | 0 | 0 | 0 |
| phases | gc | off | 0.97 (0.96–0.98) | 0.0% | 0 | 0 | 0 |
| phases | gc | 1ms | 0.98 (0.97–0.98) | 1.0% | 964 | 158595 | 0 |
| phases | gc | 100us | 1.06 (1.05–1.36) | 9.3% | 10504 | 740673 | 0 |
| phases | gc | 10us | 1.21 (1.20–1.21) | 24.7% | 75351 | 4698410 | 211143 |
| phases | gc | 10us-large | 1.22 (1.21–1.23) | 25.8% | 117752 | 7282732 | 0 |
| kkb | gc | plain | 0.11 (0.11–0.11) | 0.0% | 0 | 0 | 0 |
| kkb | gc | off | 0.17 (0.17–0.17) | 54.5% | 0 | 0 | 0 |
| kkb | gc | paused | 0.18 (0.18–0.18) | 63.6% | 0 | 40029 | 0 |
| kkb | gc | 1ms | 0.18 (0.18–0.18) | 63.6% | 175 | 50773 | 0 |
| kkb | gc | 100us | 0.20 (0.20–0.20) | 81.8% | 1993 | 161671 | 0 |
| kkb | gc | 10us | 0.28 (0.27–0.28) | 154.5% | 23241 | 1458558 | 0 |
| mlyacc | gc | plain | 0.32 (0.32–0.34) | 0.0% | 0 | 0 | 0 |
| mlyacc | gc | off | 0.41 (0.39–0.41) | 28.1% | 0 | 0 | 0 |
| mlyacc | gc | paused | 0.40 (0.39–0.41) | 25.0% | 0 | 628095 | 0 |
| mlyacc | gc | 1ms | 0.40 (0.40–0.44) | 25.0% | 401 | 652625 | 0 |
| mlyacc | gc | 100us | 0.44 (0.44–0.47) | 37.5% | 4412 | 897434 | 0 |
| mlyacc | gc | 10us | 0.59 (0.56–0.60) | 84.4% | 53977 | 3922555 | 0 |
| callbacks | gc | off | 1.66 (1.63–1.66) | 0.0% | 0 | 0 | 0 |
| callbacks | gc | 1ms | 1.62 (1.62–1.64) | -2.4% | 1621 | 199544 | 0 |
| callbacks | gc | 100us | 1.77 (1.75–1.78) | 6.6% | 17675 | 1179321 | 0 |
| callbacks | gc | 10us | 2.09 (2.07–2.15) | 25.9% | 134236 | 8293199 | 186806 |
| phases | no_gc | plain | 0.94 (0.92–0.94) | 0.0% | 0 | 0 | 0 |
| phases | no_gc | off | 0.95 (0.95–0.96) | 1.1% | 0 | 0 | 0 |
| phases | no_gc | 1ms | 0.95 (0.95–0.96) | 1.1% | 933 | 151280 | 0 |
| phases | no_gc | 100us | 1.03 (1.02–1.04) | 9.6% | 10214 | 717559 | 0 |
| phases | no_gc | 10us | 1.17 (1.17–1.39) | 24.5% | 71632 | 4465920 | 211080 |
| phases | no_gc | 10us-large | 1.18 (1.16–1.20) | 25.5% | 113679 | 7028855 | 0 |
| kkb | no_gc | plain | 0.10 (0.10–0.10) | 0.0% | 0 | 0 | 0 |
| kkb | no_gc | off | 0.16 (0.16–0.16) | 60.0% | 0 | 0 | 0 |
| kkb | no_gc | paused | 0.17 (0.17–0.46) | 70.0% | 0 | 41323 | 0 |
| kkb | no_gc | 1ms | 0.17 (0.17–0.17) | 70.0% | 166 | 51518 | 0 |
| kkb | no_gc | 100us | 0.19 (0.19–0.19) | 90.0% | 1907 | 157719 | 0 |
| kkb | no_gc | 10us | 0.26 (0.26–0.27) | 160.0% | 25833 | 1618033 | 0 |
| mlyacc | no_gc | plain | 0.30 (0.30–0.31) | 0.0% | 0 | 0 | 0 |
| mlyacc | no_gc | off | 0.57 (0.55–0.90) | 90.0% | 0 | 0 | 0 |
| mlyacc | no_gc | paused | 0.56 (0.55–0.58) | 86.7% | 0 | 620628 | 0 |
| mlyacc | no_gc | 1ms | 0.58 (0.56–0.70) | 93.3% | 575 | 655772 | 0 |
| mlyacc | no_gc | 100us | 0.64 (0.64–0.66) | 113.3% | 6378 | 1009962 | 0 |
| mlyacc | no_gc | 10us | 0.84 (0.82–0.84) | 180.0% | 65925 | 4644330 | 0 |
| callbacks | no_gc | off | 1.61 (1.59–1.62) | 0.0% | 0 | 0 | 0 |
| callbacks | no_gc | 1ms | 1.58 (1.58–1.59) | -1.9% | 1573 | 191192 | 0 |
| callbacks | no_gc | 100us | 1.72 (1.70–1.76) | 6.8% | 17029 | 1134491 | 0 |
| callbacks | no_gc | 10us | 2.07 (2.04–2.13) | 28.6% | 141627 | 8738902 | 174560 |

Phase capture ratio = recorded samples × requested interval / recorded phase
duration. Values below are medians with observed ranges across both configurations
(ten observations); drops remain excluded from recorded counts.

| Interval / capacity | Phase | Capture ratio (range) |
|---|---|---:|
| 1ms / 4096 | compute | 0.997 (0.992–0.998) |
| 1ms / 4096 | allocate | 0.996 (0.994–0.999) |
| 1ms / 4096 | c | 0.996 (0.996–0.996) |
| 1ms / 4096 | sleep | 0.998 (0.997–0.999) |
| 100us / 4096 | compute | 0.999 (0.997–0.999) |
| 100us / 4096 | allocate | 0.999 (0.998–1.000) |
| 100us / 4096 | c | 0.998 (0.996–0.999) |
| 100us / 4096 | sleep | 0.999 (0.998–0.999) |
| 10us / 4096 | compute | 0.982 (0.963–0.985) |
| 10us / 4096 | allocate | 0.982 (0.976–0.983) |
| 10us / 4096 | c | 0.164 (0.164–0.164) |
| 10us / 4096 | sleep | 0.154 (0.154–0.155) |
| 10us-large / 65536 | compute | 0.974 (0.966–0.988) |
| 10us-large / 65536 | allocate | 0.974 (0.964–0.978) |
| 10us-large / 65536 | c | 0.977 (0.965–0.981) |
| 10us-large / 65536 | sleep | 0.968 (0.947–0.975) |

[All per-run measurements](t6-measurements.json) retain durations, CPU times,
counts, output sizes, losses and phase-state counts. Raw profiles/build logs are
retained locally under `/tmp/mlkit-t6-measure` and `/tmp/mlkit-t6-phases`.

Use 1ms as the starting point for opt-in investigation. In these phase runs, 1ms
and 100us captured over 99% of nominal phase duration without buffer/routing
loss. That does not guarantee the same behavior for short, I/O-heavy programs
(such as a single parser-generator invocation). At 10us, the default capacity
fills during a 250ms C extent; 65,536 entries remove that overflow but capture
ratios still fall to roughly 95–99%. The larger buffers cost 4 MiB and generate
about 7 MiB of output per phase-workload invocation. Masking or slow foreign
calls can require more capacity even at larger intervals.

The sampled recorder-drain share has a median of 0% at 1ms, 0.039% at 100us,
1.098% at 10us/default capacity and 0.324% at 10us/large capacity in the phase
runs. These are sample shares, not separately measured handler/drain durations.
The compute phase includes C integer-modulo helpers; its regression resolves
those initiating PCs back to the compute function instead of treating every
foreign-state sample as a distinct non-ML contribution.

The profiled-off overhead demonstrates the cost of using the existing region
profiling build and C-boundary instrumentation. Active 1ms adds relatively little
in this batch, but total overhead against a plain build is substantial for
`kkb_eq` and ML-Yacc. Keep the feature experimental and use it for targeted
investigation; these results do not justify enabling profiled builds by default.
Lowering compiler instrumentation cost is a separate follow-up. The measurement
milestone is complete without claiming universally low overhead or calibrated
confidence intervals.

A phase's closing status is emitted after pausing and draining. Its duration thus
includes the final drain while its count excludes samples during that paused
drain. Capture deficits, especially with larger buffers, cannot be assigned
entirely to timer coalescing. Recorder-state shares observe only drains performed
while sampling is active. These boundaries are reproducible but are not an
independent measurement of handler or shutdown-drain cost.

Command examples use current T8 names (`pview`, `-p_file`). Archived measurement JSON retains the historical command names used when those measurements were collected; no measurements were rerun for this rename.
