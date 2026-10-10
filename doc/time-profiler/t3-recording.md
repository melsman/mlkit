# T3: buffered interrupted-PC recording

Issue [#241](https://github.com/melsman/mlkit/issues/241). This experimental
recorder supports static, single-thread macOS ARM64 executables. Compile with
`mlkit -no_par -rp`; run with `+RTS -tp`. Add runtime `-rp` to also collect region
snapshots. Time-only recording takes no automatic region snapshots and disables
allocation occupancy recording. The existing compiled safe-point polls drain
time buffers; no function-entry attribution bookkeeping is added.

Runtime options:

| Option | Meaning |
| --- | --- |
| `-tp` | Enable time recording |
| `-tp_clock wall` | Wall sampling; CPU clocks are rejected following T1 |
| `-tp_interval 1ms` | Requested interval, integer `us`, `ms`, or `s`, 1us–1s |
| `-tp_buffer 4096` | Capacity of each of two buffers, 2–1048576 records |
| `-tp_paused` | Start paused |
| `-tp_file PATH` | Shared time/region output file, default `profile.rp` |

Import `$(SML_LIB)/kitlib/time-profile.mlb` for `TimeProfile.start`, `pause`, and
`flush`, each `unit -> unit`. These operations are harmless when time recording
is disabled. Pause drains previously captured samples; flush drains the current
buffer and flushes the output. Shutdown disables sampling, drains, and writes a
final cumulative status. Starting after shutdown has no effect.

## Sampling and serialization

Time and region profiling share SIGALRM/ITIMER_REAL. The timer uses the smaller
active requested interval; independent deadlines select time samples and region
requests. Region snapshots still wait for safe points. SIGALRM must initially
be unblocked and its timer and disposition unused. Parallel runtimes and REPL
sessions reject time recording.

The SA_SIGINFO handler saves errno, obtains the interrupted PC and Mach uptime,
and publishes one record using always-lock-free atomics. It performs no heap
allocation, file output, function lookup, region traversal, or stack walking.
Interrupted SP is checked against the initializing thread's native stack bounds;
deliveries on other stacks are rejected and counted. This deliberately does not
support alternate stacks or sampling arbitrary foreign threads.

Draining swaps buffers with SIGALRM briefly blocked, then serializes after
restoring the previous mask. Signals during output write to the other buffer.
Buffers are allocated, initialized and touched before handler installation and
retained until process exit. Bounded storage cannot grow under backpressure.

Timestamps are nanoseconds from a shared Mach uptime origin, excluding machine
sleep. When time recording is enabled, region events use the same coordinate.
A sample observes the location at signal delivery, which can be inside a blocked
C primitive. It does not reconstruct locations at nominal timer expirations.
Very short requested intervals are accepted but are not a timing guarantee.

## Wire records and loss

Three additive version-1 time records extend the region profile v10 stream:

* Tag 23, `time_session`: version, interval, per-buffer capacity, logical thread
  and stream (both reserved zero), clock, semantics, and time coordinate.
* Tag 24, `time_sample`: contiguous sequence, timestamp, raw PC, thread, stream,
  state and origin PC. State 0 is unclassified raw-PC capture; state 1 means
  recorder draining. Origin PC is reserved zero. GC/C origin tracking is T4.
* Tag 25, `time_status`: timestamp, cumulative serialized count, cumulative buffer
  overflow drops, rejected-stack delivery count, active flag, final flag, and
  timer loss visibility (`unobservable`).

Buffer overflow counts observed samples that could not fit, with saturation at
UINT64_MAX. Routing drops count rejected signal deliveries while time recording
is active. Neither counter measures coalesced or otherwise undelivered timer
expirations: that loss is unobservable. Paused periods do not generate samples.
Unknown PCs remain raw observations and can be resolved with T2 metadata;
C/GC PCs remain outside ML function ranges until later attribution work.

`rpview --format json` exports these records. The reader validates lifecycle,
sequence, timestamps, identities, counts and loss counters. A completed stream
requires a final inactive status. Existing readers without these additive tags
must be upgraded. Time report UI and aggregation remain T5 work.

## Validation

`test/time_profile/check-recording.sh` exercises GC and non-GC executables,
initial pause, start/pause/resume, masked delivery, shutdown drain, forced buffer
overflow, combined region/time timers, and invalid runtime options. A FIFO with
a slow consumer forces interruption during serialization and verifies visible
drain-state samples and loss with a readable stream. Existing region tests and
T2 metadata tests cover compatibility. CPU calibration remains the separate T1
experiment, not a release timing gate.

The native ARM64 handler's disassembly and relocations were checked: its only
external calls are Darwin's errno accessor and `mach_absolute_time`; atomic
publication and loss updates compile inline. The timestamp and errno accessors
are invoked before handler installation. These checks apply to the tested
macOS ARM64 build, not a portability claim about other timestamp APIs.
