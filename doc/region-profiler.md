# Sampled region profiler

Compile the executable and its ML dependencies with `-region_profile`. Enable a
session with the executable's `-rp` option. This profiler is independent of the
old `-prof` object profiler; the two cannot be combined.

```sh
mlkit -no_gc -region_profile -o app app.mlb
./app -rp -rp_interval 10ms -rp_file profile.rp -rp_report
rpview profile.rp --output profile.html
```

Supported combinations are single-threaded no-GC, GC and generational GC, and
no-GC pthreads or Argobots. **GC plus parallelism remains excluded.** Both
native backends emit the metadata and polling bridges. ARM64 execution checks
are passing; X64 execution still needs the planned ThinkPad checks. Keep the
PR draft until those checks have been completed.

## Controls

| Runtime option | Meaning |
| --- | --- |
| `-rp` | Enable a process-wide session. Otherwise the API and runtime bookkeeping are disabled. |
| `-rp_file PATH` | Output path; defaults to `profile.rp`. |
| `-rp_interval Nms`, `Ns`, or `0` | Integral wall-clock interval; defaults to `10ms`. Zero disables automatic periodic snapshots. |
| `-rp_paused` | Initialize the session and bookkeeping, but pause automatic samples. |
| `-rp_gc_samples` | Add paired before/after-GC snapshots, with the collection kind. Requires GC. |
| `-rp_report` | Report completed samples, frames/pages traversed, timing, skipped requests, the sampled peak, and maximum allocated page count. |
| `-rp_control SOCKET` | Bind a private Unix datagram socket for local live controls. |
| `--` | End runtime options and preserve subsequent application arguments verbatim. |

Configuration options require `-rp`. Include `kitlib/region-profile.mlb` for
`RegionProfile.start`, `pause`, `sample`, `mark`, and `flush`. Start and pause
capture their transition boundaries and are idempotent. Explicit samples work
while paused. Marks and flushes also work while paused. Disabled operations are
no-ops. All snapshots describe actual safe-point capture times, not reconstructed
states at the timer deadline.

The REPL accepts the same profiler options at startup. For example:

```sh
mlkit -no_gc -region_profile -rp -rp_interval 20ms -rp_file repl.rp
mlkit -gengc -region_profile -rp -rp_gc_samples
```

Runtime options are forwarded to its child process and remain fixed across
phrases. The ML API controls the running session. Normal REPL termination now
uses the runtime's termination protocol, allowing the profiler to close its
output and report. A rejected runtime startup is detected without hanging on
the command FIFO. Idle REPL polling captures persistent regions with an explicit
end-of-ML-stack anchor.

## Accounting and metadata

* Finite bindings contribute their reserved stack storage, including storage
  reserved for results that have not been initialized. Zero-sized bindings add
  zero bytes.
* Infinite bindings are measured by following page links and subtracting the
  unused tail of the last page. Page headers and slack in earlier pages remain
  included. No object or page payload is scanned; **no per-region page count is
  maintained**. A process-wide atomic live count and high-water mark track page
  allocation, reset, release and GC reclamation while `-rp` is enabled, even
  when sampling is paused. Released chains are counted by following their links.
  The maximum excludes cached free pages and includes simultaneous GC from-space
  and to-space. No maximum stack counter is maintained.
* Both generations have their own page counts and unused-tail subtraction.
  Large-object allocation sizes are recorded separately and removed on region
  reset/release and GC reclamation.
* Descriptors and free-page caches are reported separately. The displayed region
  footprint sums page footprint, large-object bytes and finite reservations;
  it excludes descriptors and caches.
* Shared infinite regions are deduplicated by descriptor address. Thread
  attribution follows the owner of the binding's lifetime, not the thread that
  happened to allocate into it. Persistent/global descriptors have stable
  session-local IDs and their own display category.
* Argobots records logical-thread IDs and execution-stream ranks separately.
  Pthread worker identity is unavailable (`-1`). These are not physical CPU IDs.
* Recursive instances are summed, not overwritten. Unit identity plus binding
  identity is the static key; equal source names in different units are distinct.
  Explicit ReML region names are retained and interned per compilation unit.

Native map version 2 extends the M1 map with relative source-name references.
GC bitmaps precede the profiler extension; both GC walkers locate the original
bitmap before interpreting roots. Polling bridges preserve live registers.
ARM64 optimized self-loop backedges pass through the poll; other tail calls
re-enter a polled function. Profiling compilation has a separate cache identity.

Loaded REPL code remains resident. Its self-describing maps and name strings
can therefore be used directly, without a second native image registry. Stream
binding definitions are emitted once on first observation. Future code unloading
would require explicit metadata-lifetime handling.

## Coordination and foreign boundaries

The signal handler sets a lock-free request flag only. ML safe points perform
the walk. A registry coordinates a common stable state across participating
threads, including creation and termination. `Thread.get` publishes a stable
ML anchor while joining. Allocator locks are not held while parking. Argobots
waiters yield their execution stream, including when there are more logical
threads than workers.

The sampler copies region totals while participants are stopped, then resumes
them before serializing the copied records. Output writers and subsequent
snapshots are coordinated separately. Free-list inspection uses the existing
free-list lock where a completed join can transfer pages.

An arbitrary foreign call is not assumed quiescent. A periodic rendezvous that
cannot complete in one second is cancelled, releases its participants, and
records `sample_skipped`. An explicit request in that situation exits with a
clear diagnostic rather than reporting an incomplete snapshot. Timer requests
inside C-to-ML callbacks are deferred; explicit sampling across such a callback
boundary remains unsupported and is diagnosed. Ordinary callbacks continue to
work when no explicit snapshot is requested inside them.

Periodic/live operation reserves `SIGALRM` and `ITIMER_REAL` and rejects an
existing timer/handler at startup. Applications using them must use
`-rp_interval 0` without `-rp_control`. The profiler retains its harmless signal
handler until process exit, because a signal can still be pending on another
OS thread after timer disarm. A control socket keeps a timer running while
paused, or at 10ms when the sampling interval is zero, so commands can be
handled at subsequent safe points.

Requests coalesce; they never build a sample backlog. `coalesced` in the report
estimates elapsed superseded timer intervals. `wait_ns` includes time queued
behind another snapshot and rendezvous waiting. Traversal and serialization
wall time are separate; CPU time measures the sampling thread. These are
measurement costs, not allocation costs or an application-wide CPU profile.

## Stream and offline HTML viewer

Output is version 3 JSON Lines. The reader also accepts version 1 and 2 files.
Version 3 adds a `stack` record per captured thread with `active_bytes`,
`finite_bytes`, and `stack_bytes = active_bytes - finite_bytes`. The active span
runs from the innermost captured ML frame through the outermost ML return slot,
including alignment and spilled-result reservations. It excludes the sampler's
C frames, foreign-call frames, and unused OS stack capacity. Native map version
2 is unchanged; relink executables with the new runtime to record stack data.
The reader validates stack arithmetic and preserves missing stack data in older
streams as unavailable, rather than zero.
Records include binding definitions, thread lifecycle events, markers, samples,
skipped requests, and normal session termination. A sample is committed by
`sample_end`; readers ignore an incomplete final record or unfinished sample.
The stream preserves unsigned 64-bit counters. The viewer transports them as
decimal strings and sums them with `BigInt`; only chart coordinates use floating
point.

`rpview` is a compiled Standard ML program in `src/Tools/RegionProfile`.
It reads a profile file and writes one self-contained HTML file. Both building
and running the tool require no Python. The HTML/JavaScript template is embedded
in the executable at build time by a small SML helper; the installed executable
needs no template files. There is no HTTP server, live polling, or network access
in the tool or generated page. Open the output directly in a browser and
regenerate it to include new samples.

The normal build/install includes `bin/rpview`. To build it separately with an
existing native compiler:

```sh
make -C src/Tools/RegionProfile MLKIT=/absolute/path/to/mlkit
bin/rpview profile.rp --output profile.html
```

The defaults are `profile.rp` and `profile.html`; `-o` is an alias for `--output`.
The tool validates the stream before opening its output and rejects an output
path that aliases its input. The offline HTML has no external dependencies. Its default graph stacks the ten largest
region bindings and the remaining ML stack as colored bands. **Regions shown**
sets the number of individual regions (0 retains all); the largest are selected
by summed sampled size. **Other** sums the omitted regions at each snapshot and
occupies the bottom band. The stack does not count against the region limit.
Remaining bands run smallest to largest, bottom to top, as in `rp2ps`. The
legend follows their appearance from top to bottom. A binding keeps its color when changing
snapshots or filters. Axes show elapsed seconds and automatically scaled memory
units (bytes, KiB, MiB, etc.); tooltips and tables retain exact byte counters.
Finite reservations belong to their region bands, so the stack band subtracts
them. Resident descriptors are already part of the stack span; descriptors and
free-page caches are not added again. The runtime report's `sampled_peak_bytes`
remains a region-only peak; the default graph's sampled maximum includes stack.

The **Pages** metric shows the full memory capacity of assigned region pages,
without subtracting unused tails. Its axis uses scaled memory units and its table
shows exact bytes, with page counts in a separate column. **Page footprint**
subtracts unused last-page tails in each generation. In the aggregate **Pages**,
**Regions + ML stack**, and **Page footprint** views, a red horizontal line shows
the process-wide maximum allocated page count multiplied by the recorded page
size, including allocations between snapshots. This is a page-capacity reference,
not a maximum for combined region-and-stack memory; stack storage and large
objects are excluded. The axis accommodates both the bands and reference line. This line is omitted in filtered views
because the counter is process-wide. Older files without the counter show an
unavailable notice. Optional `max_pages` fields on version-3 `sample_end` and
`session_end` records store the running and final maximum. The reader uses the
largest recorded value, including the final summary after the last snapshot;
a truncated file can only report the maximum recorded before truncation.

The **View** selector applies to both graph and table: all threads, one logical
thread, one Argobots execution stream, or one OS logical CPU where recorded.
Linux records the CPU when a thread publishes its safe-point anchor; migration
can move that owner's storage between CPU bands over time. This is not physical
core topology or allocation-origin tracking. macOS reports CPU identity as
unavailable, while Argobots execution-stream selection remains available.
Shared and persistent/global regions follow their lifetime owner's recorded
identity and are counted once. Unavailable identities have explicit selectors;
older files show a stack-unavailable notice.

The snapshot slider, metric selection, markers, and table grouping remain
available. Changing table grouping does not merge the region bands. Maxima are
explicitly labelled as sampled. `rp2ps` remains unchanged.

A reproducible ReML example uses three named regions with different growth/reset
phases and a growing recursive stack:

```sh
printf '%s\n' "$PWD/test/region_profile/graph.sml" > /tmp/region-graph.mlb
reml -no_par -region_profile -o /tmp/region-graph /tmp/region-graph.mlb
/tmp/region-graph -rp -rp_interval 0 -rp_file /tmp/region-graph.rp
rpview /tmp/region-graph.rp --output graph.html
```

Hover a legend entry to see its full unit/binding identity. Select a snapshot to
inspect exact values; the vertical guide shows its position on the timeline.

The existing runtime `-rp_control` socket remains available to external clients,
but is not needed for this workflow. Use runtime flags or `RegionProfile` API
operations to select phases, then convert the resulting file to HTML. Optional
runtime controls are tested separately from the offline viewer.

## Validation and measurements

```sh
make -C src/Tools/RegionProfile MLKIT=/absolute/path/to/mlkit
sh test/region_profile/check.sh
ARGOBOTS_ROOT=/path/to/configured/argobots sh test/region_profile/check-extended.sh
python3 test/region_profile/check-live.py /path/to/instrumented/periodic
python3 test/region_profile/check-viewer.py /path/to/profile.rp
python3 test/region_profile/check-graph.py  # requires Node.js for graph-code tests
python3 test/region_profile/check-sml-reader.py /path/to/profile.rp
```

The regression harness uses Python and, for graph-code tests, Node.js; neither
is a dependency of the built tool. Scripts accept `MLKIT`, `REML`, `RPVIEW`, `CC`,
and `PYTHON` overrides as appropriate. Tests run a relocated viewer with an empty
executable search path to check that it needs no interpreter or external assets.
The live test needs local socket permissions. The extended script prints all
artifact paths. Build `runtimeSystemArPar.a` first for its optional Argobots test.

ARM64 checks cover controlled finite/page/large-object totals, reset and release,
recursion, spilled results, exceptions, periodic tail loops, paused operation,
invalid intervals, repeated snapshots, shared allocations, joins and thread
exit, blocked foreign calls, GC/genGC, retained REPL values/closures and clean shutdown.
M7 checks cover exact frame spans and finite subtraction, recursive stack growth,
colored band sums/order, thread/worker/CPU filters (including synthetic CPU
migration), axis units, old/truncated/single-sample streams, and counters above
2^53. Linux CPU capture and X64 stack execution await the ThinkPad checks. Both generation
tails were also checked against exact synthetic totals under ASan/UBSan.
Argobots 1.2 passed with thirteen logical threads and one or two execution
streams. The viewer was checked in the browser, and live commands/socket cleanup
passed. X64 compiler builds and cross-assembly checks cover polling, GC sampling
bridges, callbacks and thread creation; execution is still pending.

An M2 ARM64 benchmark, before the M7 stack records were added, ran a billion iterations of a deliberately tiny loop,
with seven runs per mode. Median elapsed times were:

| Mode | Seconds | Profile bytes, last run |
| --- | ---: | ---: |
| Uninstrumented | 0.865 | 0 |
| Instrumented, disabled | 1.149 | 0 |
| Enabled, paused | 1.150 | 185 |
| Active, 1ms | 1.170 | 1,769,680 |
| Active, 10ms | 1.154 | 177,307 |
| Active, 100ms | 1.149 | 19,694 |

This exposes the cost of a poll relative to almost no useful loop work: about
33% over the uninstrumented loop even with the session disabled. It is not a
representative application overhead figure. Active sampling at 10ms added less
than 1% relative to the instrumented loop in this experiment. Timing and sample
counts vary with scheduling; the default interval remains provisional.

An isolated C fixture captured one region 1,000 times, writing to `/dev/null`:

| Pages per region | Mean traversal time per snapshot |
| ---: | ---: |
| 1 | 0.029 µs |
| 16 | 0.089 µs |
| 256 | 1.069 µs |
| 4,096 | 47.480 µs |

The last case represents about 32 MiB of region pages. These hot-list results do
not cover arbitrary heap layouts or thread contention. They support retaining
page traversal for now; maintaining a page count would not remove polling or
serialization costs. Reproduce with `test/region_profile/benchmark.py` and
`src/Runtime/tests/region-profile-bench.c` before making that tradeoff on X64 or
representative applications.
