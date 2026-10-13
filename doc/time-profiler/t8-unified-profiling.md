# Unified profiling options and terminology (T8)

This is the final design reference for issue [#241](https://github.com/melsman/mlkit/issues/241).
The milestone notes linked below retain the development history and original
measurements; historical scopes and costs are not claims about the final head.

| Completed milestone | Result and record |
| --- | --- |
| T1 | [ARM64 timer/PC feasibility](t1-feasibility.md); CPU sampling timers remain unsupported |
| T2 | [Function ranges and image identities](t2-function-metadata.md) |
| T3 | [Buffered recording prototype](t3-recording.md), superseded by T6a's lifecycle draining |
| T4 | [Optional detailed C-origin, callback, exception and GC attribution](t4-attribution.md) |
| T5 | [Offline report, shared slider and function IR navigation](t5-report.md) |
| T6 | [Historical prototype measurements and hardening](t6-measurement.md) |
| T6a | [Ordinary-build sampling in existing runtimes/caches](t6a-ordinary-builds.md) |
| T6b | [Historical ordinary-build measurements](t6b-ordinary-measurement.md) |
| T6d | [16-byte passive records; increased entry capacity at the same memory budget](t6d-compact-records.md), implemented before T6c |
| T6c | [Optional detailed mode and passive/detailed measurements](t6c-optional-attribution.md) |
| T7 | [Targeted no-GC pthread sampling, lifetimes, streams and CPU accounting](t7-parallel-sampling.md) |
| T8 | Unified options, `pview`, shared output and existing manual terminology (this document) |
| T9 | [Linux X64 support, ELF identity, native ABI checks and measurements](t9-linux-x64.md) |

## Final supported configurations

Static macOS ARM64 and Linux X64 executables support single-thread GC, GenGC
and no-GC sampling, plus no-GC pthread sampling. All reuse existing runtime
archives and library-cache families. Detailed C-origin recording requires
single-thread `-rp` instrumentation. CPU sampling timers, parallel GC, Argobots
time sampling, dynamic REPL/images and macOS X64 time sampling remain outside
the completed milestones. Per-thread CPU totals are accounting, not CPU samples.

Each sampled thread has a preallocated buffer: by default 2 MiB, containing
131,072 passive 16-byte PC/tick records or 65,536 detailed 32-byte records.
`-tp_buffer N` selects a record count. Flush/start/pause, thread exit and normal
shutdown drain buffers; compiler safe-point polls do not drain time samples.
Draining blocks delivery and introduces an observation gap. Buffer/routing losses
are visible; timer coalescing and missed expirations cannot be reconstructed.
Time-profile API calls are inert without an enabled time session, including
when region profiling alone has opened the shared output.

Passive sampling checks the atomic `doing_gc` flag and stores the `gc` address
for the entire GC extent, including helpers and shared allocation calls.
Detailed mode retains actual helper PCs and records GC state. No bitmap or
extra record field is used. Ordinary allocation samples remain separate.

Both native backends emit reproducible function metadata and IR companions in
ordinary builds. The viewer automatically searches nearby `MLB` caches when
object paths are absent; `--ir-dir` is for extra/moved locations. Identity/unit
checks and function-parent metadata support safe navigation of repeated names.
The compact scrollable time table and shared slider are described in [T5](t5-report.md).
`pview profile.prof --format text` provides a non-interactive aggregate time
overview on standard output; `.txt` output files select the same format. HTML,
SVG and raw JSON remain available. Text and HTML share attribution rules.

`pview` is the MLKit **Profile Viewer**. A **profile** is one binary file that
can contain **region snapshots**, **time samples**, or both. The default filename
is `profile.prof`; the viewer reads existing `.rp` files as well. The file format
is unchanged. `rpview`, `-rp_file`, and `-tp_file` have been removed without aliases.

## Compiler support and runtime recording

| Option | Compiler | Runtime (`+RTS ... -RTS`) |
| --- | --- | --- |
| `-rp` | Emit region profiling instrumentation | Record region snapshots |
| `-tp` | Ensure time metadata; ordinary native builds already emit it | Record time samples |
| `-p` | Equivalent to `-rp -tp` | Equivalent to `-rp -tp` |
| `-p_file PATH` | Select the REPL child's output where supported | Select the shared profile output |
| `-p_paused` | Start enabled REPL profilers paused where supported | Start enabled profilers paused |

Compiler support does not start recording in a batch executable. Time-only
sampling still needs no compiler flag and reuses ordinary runtimes and caches.
Explicit compiler `-tp` is available in both native backends. Runtime time sampling
requires static macOS ARM64 or Linux X64 executables; supported pthreads use passive attribution.
Combined activation fails when either requested profiler is unavailable.
Runtime `-p` is advertised only in executables compiled with region profiling
(`-rp` or `-p`); ordinary executables reject it and use `-tp` for time-only output.
Dynamic REPL time profiling is unsupported and rejects an explicit request;
REPL region profiling forwards the shared file and pause options.

```sh
mlkit -no_par -p -o run program.mlb
./run +RTS -p -p_file program.prof -RTS
pview program.prof -o program.html

mlkit -no_par -o run program.mlb
./run +RTS -tp -p_file time.prof -RTS
pview time.prof -o time.html
```

`-p_file` and `-p_paused` require at least one enabled profiler. File selection
never implicitly enables recording. Multiple file selections use the last path.
`-p_paused` affects both profilers if enabled; the existing `-rp_paused` and
`-tp_paused` controls remain available for independent startup. Existing API
start/pause/flush calls remain independent. Region intervals (`-rp_interval`)
and time intervals (`-tp_interval`), attribution, buffer sizes and region
selection remain separate controls.

Argobots execution-stream count now uses `+RTS -par_threads N -RTS`, freeing
`-p` for combined profiling. Argobots time sampling remains unsupported.

Build and install with `make pview` and the normal installation targets.
Test/benchmark viewer overrides now use `PVIEW`. User help, man pages and current
command examples use the unified names. Archived measurement JSON keeps the
original recorded commands. The existing manual source is `doc/manual/mlkit.tex`
(the repository has no `doc/manual.tex`); its existing profiling names and defaults
are updated without rerunning examples or adding time profiling documentation.

## T8 milestone validation (historical)

Local macOS ARM64 checks passed for compiler `-tp`/`-p`, runtime time-only,
region-only and combined recording, shared file/default/pause controls, removed
file options, legacy `.rp` input, REPL shared-option forwarding and prompt rejection
of unsupported REPL time activation. All existing runtime variants rebuilt
without warnings. Binary and viewer regressions, runtime argument parsing,
region accounting, time lifecycle/overflow/combined recording and pthread
sampling/stream/IR checks passed. Build seed selection checks passed.
A staged normal installation contains `pview` and its man page and no `rpview`.
The installed viewer reads the new default and existing `.rp` files.
Manual examples and timing benchmarks were not rerun; no time profiling section
was added to the manual. These are the original T8 local checks; final CI and
remaining visual-validation limits are recorded below.

## Final CI and review status

[CI run 38320520735](https://github.com/melsman/mlkit/actions/runs/38320520735)
passed macOS ARM64 (MLKit), Linux X64 (MLKit) and Linux X64 (MLton) at commit
`9ada996d`, including bootstrap, profiling, installation and release checks.
The prior run failed because the viewer DOM harness lacked `document.querySelector`;
the harness and view-switch assertions were corrected.

The subsequent documentation/Copilot review fixes make disabled
`TimeProfile.flush()` leave region output untouched and make the feasibility
helper report its default wall mode without passing a null string to `printf`.
Targeted regressions cover both. Those follow-ups require green CI on their
new head before merging. Real-browser visual layout inspection remains unverified.
Archived timing JSON and manual examples were not regenerated for this review;
the old measured costs do not establish the exact overhead of later fixes.
