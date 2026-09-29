# Sampled region profiler

Compile the executable and its ML dependencies with `-region_profile`. Enable a
session with the executable's `-rp` option. This profiler is independent of the
old `-prof` object profiler; the two cannot be combined.

```sh
mlkit -no_gc -region_profile -o app app.mlb
./app -rp -rp_interval 10ms -rp_file profile.rp -rp_report
python3 src/Tools/RegionProfile/rp-read.py profile.rp
python3 src/Tools/RegionProfile/rp-view.py profile.rp --output profile.html
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
| `-rp_report` | Report completed samples, frames/pages traversed, timing, skipped requests, and the sampled peak. |
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
  included. No object or page payload is scanned; **no new page count is
  maintained**.
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

## Stream and live viewer

Output is version 2 JSON Lines. The reader also accepts M1 version 1 files.
Records include binding definitions, thread lifecycle events, markers, samples,
skipped requests, and normal session termination. A sample is committed by
`sample_end`; readers ignore an incomplete final record or unfinished sample.
The stream preserves unsigned 64-bit counters. The viewer transports them as
decimal strings and sums them with `BigInt`; only chart coordinates use floating
point.

The offline HTML has no external dependencies. It provides a timeline, a
snapshot selector, metric selection, markers, and tables grouped by binding,
lifetime owner or execution stream. Maxima are explicitly labelled as sampled.

```sh
./app -rp -rp_control /tmp/app-profile.sock -rp_file /tmp/app-profile.rp
python3 src/Tools/RegionProfile/rp-view.py /tmp/app-profile.rp \
  --serve --control /tmp/app-profile.sock
```

The viewer prints a loopback URL. It polls the append-only file for committed
samples and sends start/pause/sample/flush commands through the local socket.
Commands are queued until an ML or idle-REPL safe point; HTTP acceptance is not
an acknowledgement of completed capture. The socket is created with owner-only
permissions, refuses to replace an existing path, and is removed on normal
shutdown. The HTTP server binds only to loopback and authenticates control
requests with a per-server token. Large files are currently re-read by the live
viewer; incremental transport is a future scalability improvement.

## Validation and measurements

```sh
sh test/region_profile/check.sh
ARGOBOTS_ROOT=/path/to/configured/argobots sh test/region_profile/check-extended.sh
python3 test/region_profile/check-live.py /path/to/instrumented/periodic
python3 test/region_profile/check-viewer.py /path/to/profile.rp
```

The scripts accept `MLKIT`, `REML`, `CC`, and `PYTHON` overrides as appropriate.
The live test needs local socket permissions. The extended script prints all
artifact paths. Build `runtimeSystemArPar.a` first for its optional Argobots test.

ARM64 checks cover controlled finite/page/large-object totals, reset and release,
recursion, spilled results, exceptions, periodic tail loops, paused operation,
invalid intervals, repeated snapshots, shared allocations, joins and thread
exit, GC/genGC, retained REPL values/closures and clean shutdown. Both generation
tails were also checked against exact synthetic totals under ASan/UBSan.
Argobots 1.2 passed with thirteen logical threads and one or two execution
streams. The viewer was checked in the browser, and live commands/socket cleanup
passed. X64 compiler builds and cross-assembly checks cover polling, GC sampling
bridges, callbacks and thread creation; execution is still pending.

A local ARM64 benchmark ran a billion iterations of a deliberately tiny loop,
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
