# T6c: optional detailed C attribution

This is a historical milestone record. Later milestones supersede some of its
implementation and validation details; see the [final design and milestone map](t8-unified-profiling.md)
and [Linux X64 support](t9-linux-x64.md). Retained measurements describe the
binaries measured at that milestone, not later changes.

[T7](t7-parallel-sampling.md) adds static macOS ARM64 no-GC pthread sampling,
per-thread compact buffers and thread/stream views. Earlier parallel rejection
statements below describe the pre-T7 implementation.

Time recording has two independently selectable attribution modes:

```
# Ordinary build: interrupted-PC self-time with compact records.
./ordinary +RTS -tp -tp_attribution passive -p_file passive.rp

# Code compiled with -rp, but no region snapshots requested.
./instrumented +RTS -tp -tp_attribution detailed -p_file detailed.rp

# The same instrumented executable: compact self-time plus region snapshots.
./instrumented +RTS -tp -tp_attribution passive -rp -p_file combined.rp
```

Without the option, ordinary code chooses passive and instrumented single-thread
code chooses detailed, preserving existing behavior. Explicit detailed selection
requires the compiler's C-origin instrumentation (`-no_par -rp`); ordinary code
rejects it before recorder setup. Invalid modes and mode options without `-tp`
are rejected. Unsupported parallel/platform/REPL activation stays rejected.

Passive records contain PC/tick only. Detailed records also retain state and
initiating C-call PC. The default buffer uses 2 MiB in either mode: 131,072
passive records or 65,536 detailed. Mode selection is fixed for the session.
Explicit `-tp_buffer N` means records, not bytes. See [T6d](t6d-compact-records.md).
The wire format remains shared with region profiling and compatible with pview;
passive drains serialize zero state/origin. The sampler observes the atomic
`doing_gc` flag and substitutes the address of `gc` for all GC work, including
while executing shared allocation helpers. Detailed mode keeps the actual PC
and records GC state.

## Attribution and instrumentation decision

Passive resolves known ML and main-image runtime/C PCs directly. Shared GC,
Allocation and Reset ranges keep their category rows. Unknown shared-library
locations remain unresolved in the denominator. Detailed recording additionally
captures compiler boundary state and C-origin PCs. Recognized raw ML/C function
PCs still receive self-time rows; unresolved C locations can fall back to their
initiating ML function. GC state covers the collector's extent, including its
helpers. Callback/exception restoration prevents an enclosing C origin from
claiming recognized callback ML execution.

The `passive` option on an instrumented build disables C-origin recording,
not the generated boundary instructions or region instrumentation. Thus comparing
passive/detailed on that same executable compares recording modes at the same
2 MiB budget; it does not isolate compiler marker cost. Comparing instrumented-off
against ordinary-off includes all region-build compiler/runtime differences,
not C markers alone. Existing T6/T6b results already show that aggregate cost.

This milestone keeps detailed instrumentation in the existing optional `-rp`
compilation mode. It adds no time-specific runtime archives or cache families.
Ordinary passive builds retain their existing caches; profiled builds retain the
existing `_CODE1_TP1` metadata/instrumentation cache suffix. Runtime switching
needs no recompilation. A separately cached ordinary build with only C markers
would add another compiler configuration; these measurements do not isolate
its prospective cost or establish a need for that extra configuration.

IR documents and function ranges are emitted in ordinary ARM64 and X64 builds. Both
recording modes can navigate a sampled ML function in pview with matching IR,
without region snapshots or allocation-site records. Runtime/C rows have no ML
IR document. They remain time rows and are excluded from ML companion searches.
That exclusion also fixes the linked macOS CI failure: runtime/C ranges were
incorrectly reported as hundreds of missing region-flow IR companions.

## Reproduction and interpretation

Run `sh test/time_profile/measure-attribution.sh`. `OUT`, `MLKIT`, `PVIEW` and
`REPEATS` may be overridden (five repeats by default; minimum three). The harness
reuses the serial ordinary benchmark builder and runs GC/GenGC/no-GC phases,
`kkb_eq` and twenty Topdec ML-Yacc generations. Settings include ordinary off,
instrumented off, ordinary 100us, instrumented passive/detailed 100us, and both
combined modes at region 10ms/time 100us. Phases additionally compare ordinary
1ms/10us and an explicit 65,536-record compact buffer at 100us.

Runs are warmed and deterministically shuffled. Stable output must match;
busy-C's nondeterministic result is excluded. Each enabled time run must end
with a final recorded count equal to its samples; passive states/origins must
be zero, and detailed runs must contain valid C-origin samples. Combined runs
must contain snapshots. The decoded session capacity must match the selected
layout or explicit override. Raw-PC resolution, state counts and potential
C-origin fallback counts are retained, including per-phase fallback counts.

Raw executables, logs, stdout/stderr, `.rp` and decoded records remain in `OUT`.
The child `/usr/bin/time -l -p` records wall/user/system seconds and process peak
RSS. Child wall times have 0.01s resolution. RSS includes ML heaps/regions,
metadata and runtime memory. The controller records launch-inclusive elapsed
separately. Performance batches run serially without concurrent builds/tests.

Phase capture ratio = count × interval / closing-status duration. Closing status
includes the paused drain while that drain masks delivery; missed/coalesced
expirations are unobservable. Small buffers bias samples toward earlier work.
Mode comparisons include startup, output and scheduling costs and do not prove
handler-only speedups. There are no timing thresholds in CI or calibrated
confidence intervals; retain all min/max observations and avoid universal claims.

## Local results

Apple M2 Max, Darwin 25.5.0; native ARM64 executables and x64 Node controller. Five repetitions per setting, 360 runs. Raw artifacts: `/tmp/mlkit-t6cd-measure`.

Child wall seconds: median (min–max). All active settings in this table use 100us and their default 2 MiB budget. Profiled passive/detailed use the same executable.

| Workload | GC | Ordinary off | Ordinary passive | Profiled off | Profiled passive | Profiled detailed |
|---|---|---:|---:|---:|---:|---:|
| phases | gc | 1.00 (0.99–1.01) | 1.07 (1.07–1.07) | 1.00 (1.00–1.01) | 1.15 (1.15–1.17) | 1.16 (1.14–1.16) |
| phases | gengc | 0.98 (0.97–0.98) | 1.04 (1.03–1.05) | 0.98 (0.98–0.99) | 1.13 (1.12–1.44) | 1.13 (1.13–1.14) |
| phases | no_gc | 0.93 (0.92–0.94) | 1.00 (0.98–1.01) | 1.00 (0.96–1.27) | 1.10 (1.09–1.10) | 1.10 (1.09–1.11) |
| kkb | gc | 0.11 (0.11–0.14) | 0.13 (0.13–0.13) | 0.17 (0.17–0.17) | 0.24 (0.23–0.27) | 0.24 (0.23–0.24) |
| kkb | gengc | 0.13 (0.13–0.14) | 0.16 (0.15–0.16) | 0.18 (0.18–0.19) | 0.27 (0.26–0.27) | 0.27 (0.27–0.27) |
| kkb | no_gc | 0.11 (0.10–0.14) | 0.13 (0.13–0.13) | 0.16 (0.16–0.44) | 0.23 (0.22–0.23) | 0.23 (0.23–0.23) |
| mlyacc | gc | 0.33 (0.32–0.33) | 0.44 (0.44–0.47) | 0.40 (0.39–0.41) | 1.03 (1.01–1.42) | 1.02 (1.00–1.06) |
| mlyacc | gengc | 0.31 (0.31–0.33) | 0.44 (0.43–0.46) | 0.41 (0.40–0.48) | 0.96 (0.95–0.99) | 0.96 (0.95–0.97) |
| mlyacc | no_gc | 0.32 (0.32–0.34) | 0.45 (0.45–0.46) | 0.58 (0.58–0.92) | 1.17 (1.16–1.18) | 1.17 (1.16–1.19) |

Combined modes at region 10ms / time 100us:

| Workload | GC | Passive seconds | Detailed seconds | Region snapshots median, passive / detailed | Time samples median, passive / detailed |
|---|---|---:|---:|---:|---:|
| phases | gc | 1.16 (1.16–1.17) | 1.17 (1.16–1.44) | 55 / 55 | 10545 / 10537 |
| phases | gengc | 1.15 (1.14–1.16) | 1.15 (1.15–1.16) | 55 / 55 | 10448 / 10456 |
| phases | no_gc | 1.12 (1.11–1.13) | 1.12 (1.12–1.13) | 53 / 53 | 10427 / 10463 |
| kkb | gc | 0.24 (0.24–0.27) | 0.24 (0.23–0.52) | 19 / 19 | 1967 / 1948 |
| kkb | gengc | 0.27 (0.27–0.27) | 0.27 (0.27–0.57) | 22 / 22 | 2276 / 2283 |
| kkb | no_gc | 0.23 (0.23–0.23) | 0.23 (0.23–0.23) | 19 / 18 | 1937 / 1908 |
| mlyacc | gc | 1.00 (1.00–1.05) | 1.03 (1.00–1.09) | 42 / 45 | 4294 / 4606 |
| mlyacc | gengc | 0.96 (0.95–0.98) | 0.97 (0.96–1.33) | 43 / 43 | 4424 / 4452 |
| mlyacc | no_gc | 1.26 (1.25–1.29) | 1.27 (1.24–1.29) | 64 / 64 | 7309 / 7409 |

Peak phase RSS in MiB (median). The explicit 65,536-record passive override uses 1 MiB rather than the default 2 MiB.

| GC | Ordinary off | Passive 131,072 | Passive 65,536 | Profiled off | Profiled passive 131,072 | Profiled detailed 65,536 |
|---|---:|---:|---:|---:|---:|---:|
| gc | 50.31 | 52.47 | 51.48 | 55.19 | 57.64 | 57.66 |
| gengc | 73.08 | 75.25 | 74.23 | 86.58 | 89.02 | 89.02 |
| no_gc | 246.33 | 248.50 | 247.52 | 365.69 | 368.03 | 368.03 |

Capture ratios across all three configurations (15 phase observations each). Values are median (min–max), including paused final-drain time.

| Setting | Phase | Capture ratio | C-origin fallback fraction of recorded samples |
|---|---|---:|---:|
| 1ms | compute | 0.997 (0.996–0.999) | 0.0% (0.0–0.0%) |
| 1ms | allocate | 0.996 (0.994–0.999) | 0.0% (0.0–0.0%) |
| 1ms | c | 0.996 (0.996–0.996) | 0.0% (0.0–0.0%) |
| 1ms | sleep | 0.998 (0.997–0.999) | 0.0% (0.0–0.0%) |
| 100us | compute | 0.999 (0.996–0.999) | 0.0% (0.0–0.0%) |
| 100us | allocate | 0.998 (0.995–0.999) | 0.0% (0.0–0.0%) |
| 100us | c | 0.998 (0.996–0.999) | 0.0% (0.0–0.0%) |
| 100us | sleep | 0.999 (0.998–0.999) | 0.0% (0.0–0.0%) |
| 10us | compute | 0.975 (0.968–0.986) | 0.0% (0.0–0.0%) |
| 10us | allocate | 0.974 (0.924–0.988) | 0.0% (0.0–0.0%) |
| 10us | c | 0.978 (0.949–0.987) | 0.0% (0.0–0.0%) |
| 10us | sleep | 0.961 (0.940–0.976) | 0.0% (0.0–0.0%) |
| profiled-passive | compute | 0.999 (0.997–0.999) | 0.0% (0.0–0.0%) |
| profiled-passive | allocate | 0.998 (0.996–0.999) | 0.0% (0.0–0.0%) |
| profiled-passive | c | 0.998 (0.996–0.999) | 0.0% (0.0–0.0%) |
| profiled-passive | sleep | 0.999 (0.988–0.999) | 0.0% (0.0–0.0%) |
| profiled-detailed | compute | 0.998 (0.989–0.999) | 0.0% (0.0–0.0%) |
| profiled-detailed | allocate | 0.998 (0.993–0.999) | 0.0% (0.0–0.0%) |
| profiled-detailed | c | 0.998 (0.995–0.999) | 3.1% (2.1–3.6%) |
| profiled-detailed | sleep | 0.999 (0.997–0.999) | 100.0% (100.0–100.0%) |
| combined-passive | compute | 0.999 (0.996–0.999) | 0.0% (0.0–0.0%) |
| combined-passive | allocate | 0.999 (0.996–0.999) | 0.0% (0.0–0.0%) |
| combined-passive | c | 0.998 (0.997–0.999) | 0.0% (0.0–0.0%) |
| combined-passive | sleep | 0.999 (0.998–0.999) | 0.0% (0.0–0.0%) |
| combined-detailed | compute | 0.999 (0.998–0.999) | 0.0% (0.0–0.0%) |
| combined-detailed | allocate | 0.999 (0.995–0.999) | 0.0% (0.0–0.0%) |
| combined-detailed | c | 0.998 (0.991–0.999) | 3.3% (2.8–4.0%) |
| combined-detailed | sleep | 0.999 (0.995–0.999) | 100.0% (100.0–100.0%) |

Visible buffer/routing drops across 270 active runs: 0 / 0. All 90 combined runs contained both streams. This does not count coalesced/missed expirations.

[Per-run measurements](t6cd-measurements.json) retain wall/CPU times, peak RSS, executable/output sizes, actual capacities, record sizes, state counts, raw-PC resolution, C-origin fallbacks, phase capture and losses. [Summary](t6cd-summary.json) retains timing ranges and ordinary-off comparisons.

## Cache footprint

Logical file sizes from fresh isolated ML-Yacc `src`/`lib` caches, excluding shared Basis and main-driver caches:

| GC | Ordinary cache MiB | Existing profiled cache MiB | Ordinary / profiled object MiB | Ordinary / profiled IR MiB |
|---|---:|---:|---:|---:|
| gc | 5.73 | 9.41 | 1.71 / 4.45 | 1.50 / 2.41 |
| gengc | 5.80 | 9.41 | 1.79 / 4.46 | 1.50 / 2.41 |
| no_gc | 5.35 | 9.03 | 1.28 / 4.03 | 1.63 / 2.54 |

The larger profiled cache includes region metadata and instrumentation as well as C markers; it is not a marker-only comparison. Switching passive/detailed at runtime adds no cache files and no runtime variants. [Cache measurements](t6c-cache-sizes.json) include file counts and byte totals. The opt-in harness records them after the serial runs; compile time was not measured.

## Assessment and validation

The compact default kept the same approximately 2.2 MiB activation increase in
phase RSS while doubling record capacity. An explicit 65,536-record compact
buffer reduced peak RSS by about 1 MiB relative to the new default, as expected;
the automatic default uses that space for more entries. On the same instrumented
executable, passive/detailed median RSS was essentially equal and wall medians
were equal or differed by 0.01s in most comparisons. The timing ranges overlap;
this batch does not establish a handler-speed improvement from compact records.

Ordinary 1ms phase capture medians were above 99% with no visible loss. At 10us,
phase medians were about 96–98% despite zero buffer drops. Retaining twice as many
records does not remove timer coalescing or masked drain gaps. None of these
measurements licenses a claim of one sample per requested expiration.

Detailed attribution supplied a resolvable initiating ML origin for essentially
100% of recorded sleeping-phase samples across the 15 detailed observations.
Passive keeps those shared-library locations unresolved. About 3% of the busy-C
phase also used origin fallback; most other busy-C samples resolve directly to
the named C function. These are wall observations including blocking, not CPU
consumption or a reconstructed call graph. GC extent state is also available
only with the detailed instrumentation.

The optional full profiled build still has substantial aggregate cost. For
ML-Yacc, ordinary off medians were 0.33/0.31/0.32s (GC/GenGC/no-GC), versus
0.40/0.41/0.58s profiled off, and roughly 0.96–1.17s for active profiled modes.
Both recorder modes incur the profiled build's startup/runtime work; disabling
origin recording does not disable compiler instrumentation. This justifies
retaining ordinary passive sampling as the default for ordinary builds and
keeping detailed attribution an explicitly selected optional compilation path.
It does not quantify a standalone C-marker-only compiler mode.

Local checks passed for explicit passive/detailed availability, malformed and
unaccompanied options, actual default capacities, state/origin fields, final
counts, and combined passive recording in all three GC configurations. Actual
sampled ML functions navigated to matching highlighted IR in time-only and
combined reports without allocation-site metadata. Callback/C-exception/GC-policy,
lifecycle/overflow/masking/slow-output, phase, timer-conflict, metadata/ASLR/image
mismatch, binary/viewer regressions also passed. All existing runtime variants
built. The passive handler's disassembly/relocations retain only errno accessor
and Mach clock external calls; atomics compile inline. The linked all-regions CI failure was traced to runtime/C entries in the IR
companion search and corrected; local all-regions and IR11 passed with the runtime/C exclusion.

The new explicit-mode checks run in macOS ARM64 profiler CI. Cloud checks for
these milestone snapshots were not assessed here; current CI status is in
[the final design reference](t8-unified-profiling.md). Real-browser visual layout
inspection remains unverified. Detailed recording
continues to reuse existing runtimes/caches; [T7](t7-parallel-sampling.md) subsequently
completed supported pthread sampling and [T9](t9-linux-x64.md) extended it to Linux X64.

Command examples use current T8 names (`pview`, `-p_file`). Archived measurement JSON retains the historical command names used when those measurements were collected; no measurements were rerun for this rename.
