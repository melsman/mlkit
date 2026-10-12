# Unified profiling options and terminology (T8)

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

## Validation

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
was added to the manual. Browser visual layout and new cloud CI remain unverified.
