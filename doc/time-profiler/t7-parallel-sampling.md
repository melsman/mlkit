# T7: targeted pthread sampling and thread views

Static macOS ARM64 executables built with `-no_gc -par` now support `+RTS -tp`.
Existing ordinary/profiling parallel runtimes and library caches are reused.
Parallel GC/tagged values remain unsupported by the compiler; Argobots, dynamic
REPL images, CPU timers and detailed C-origin attribution remain unsupported.
Explicit `-tp_attribution detailed` on parallel code rejects before setup.

## Delivery and buffers

A coordinator pthread sends SIGUSR2 directly to each registered ML pthread once
per nominal interval. Every live thread is targeted, with rotating send order.
This avoids process-wide SIGALRM delivery selecting only one worker. Sleeping
and waiting threads can be observed, so estimates describe wall time, not CPU
consumption. The coordinator is not an ML thread and is not sampled. It does
not change target signal masks and does not serialize records.

Each registered thread gets one compact 16-byte PC/Mach-tick buffer. The default
is 131,072 entries / 2 MiB **per live thread**; `-tp_buffer N` overrides the entry
count per thread. Buffers are allocated and touched outside signal context.
They drain on explicit flush/start/pause, thread exit and normal shutdown.
Exit frees the buffer; a fixed descriptor remains valid until process exit.
The bounded registry supports at most 1,024 thread lifetimes per session and
rejects excess registration explicitly. Descriptor scanning remains bounded
and does not consult function metadata or thread-local storage in the handler.

The handler reads interrupted SP/PC, scans published stack ranges to identify
the registered stream, and writes only lock-free atomics to its own buffer.
There is no handler allocation, I/O, stack walking, pthread API or lock. A gate
atomically combines recording permission and handler-in-flight state. Draining
clears permission and waits for a handler already admitted before reuse/free.
All streams are gated off during an explicit session drain. This creates an
observation gap; paused/late requests can be ignored and signals can coalesce.
Overflow retains earlier samples and counts later delivered samples as drops.

Registry/coordinator and lifecycle operations are serialized outside handlers.
Wire frames use a separate output mutex, so time records and cooperative region
records can interleave without corrupting frames. Lock ordering does not require
stopping ML execution for a time drain. Shutdown stops/joins the coordinator,
drains all remaining streams and emits their ends before the final status.

SIGUSR2 must have its default disposition at startup and be unblocked in each
registered ML pthread. Ownership conflicts are rejected. A primitive that later
blocks the signal causes coalescing; one that changes its handler violates the
ownership contract. Foreign pthreads are not registered or targeted. Code that
switches outside the registered stack range is not attributed to an ML thread.

## Representation and report

Parallel sessions use time version 3 inside the existing `.rp` version-10
container. Existing sample/status fields remain unchanged; sample thread IDs
identify runtime ML pthreads. Each logical thread has its own pthread execution
stream with the same lifetime ID; this runtime has a one-to-one mapping and
no logical-thread migration between streams. OS core migration does not change
that identity. New `time_thread`
records (tag 26) carry thread/stream, real elapsed timestamp, recorded total,
buffer drops, lifetime CPU nanoseconds and `start`/`end` event.
CPU totals come from the owner’s CLOCK_THREAD_CPUTIME_ID at registration and
normal thread exit, outside the handler;
they include profiling work, may have coarse resolution, and are not slider-filtered.
Workers still alive at process shutdown cannot be read through the owner clock;
their CPU accounting is unavailable. UINT64_MAX marks unavailable accounting and
is displayed explicitly. Sequence numbers remain contiguous across
serialized samples. Buffered streams can be drained in different orders; the
reader validates timestamps per stream, rather than demanding global file order.
It validates declared live streams, passive state/origin, matching end counts,
final lifecycle completion and reconciled per-thread/session losses.

All sample and lifetime timestamps share one Mach uptime origin with combined
region snapshots. They preserve real timing gaps, rather than infer time from
sample counts. Versions 1/2 remain readable; older readers reject parallel
version 3 clearly and should be rebuilt before viewing these profiles.

pview retains the shared elapsed-time slider and function histogram. The
execution selector includes declared streams even when a thread has no samples.
A thread-details table shows start/end times, samples in the window, recorded
totals, session-wide buffer drops and measured lifetime CPU totals. Clicking a thread selects its function
view. Matching worker functions still navigate to highlighted IR independently
of allocation-site records. Info icons explain scope and loss limitations.
Aggregate estimates sum overlapping **thread wall time**; they are not elapsed
program time or reconstructed call stacks. Percentages retain unresolved PCs.

## Bootstrap fixed-point fix

The linked macOS CI run passed profiler and runtime checks, then failed its
bootstrap comparison. Ordinary ARM64 metadata had introduced time-generated IR
identities and an unused map containing stage-specific object paths into the
executable. Ordinary identities now hash unit, source and fixed-layout IR text;
ordinary links omit the unused path map. Profiling links retain the object map
needed for allocation/region IR metadata. Companion/object digest validation
remains intact, and ordinary time IR navigation continues through content IDs
and explicit IR roots. This makes ordinary metadata reproducible rather than
masking UUID differences in bootstrap comparison.

## Validation

`check-parallel.sh` exercises ordinary/profiled builds, two waves of four workers,
output parity, lifecycle flush/pause/start, bounded overflow and combined region
recording. It checks every worker receives samples, per-stream time/lifetime
bounds, end/final counts, loss reconciliation and strong delivery imbalance.
Malformed identities, timestamps, attribution and event counts are rejected.
DOM checks exercise thread details, stream/time filtering and highlighted worker IR.
`check-parallel-blocking.sh` compares sleeping and masked workers using actual
lifetime durations; `check-parallel-conflicts.sh` rejects ownership/mask conflicts.
These checks run in macOS ARM64 profiler CI. Timing delivery is experimental and
no exact sampling-rate or universal overhead claim is made.

Local macOS ARM64 validation passed for both ordinary and profiled parallel
programs, two worker waves, concurrent explicit drains, pause/start, output parity,
overflow, combined region snapshots, per-thread CPU accounting, malformed stream
records, thread/stream details and worker IR navigation. Sleeping/masked workers
and SIGUSR2 conflicts passed. Region-only parallel execution is explicitly tested
with time profiling off. Existing single-thread ordinary GC/GenGC/no-GC,
attribution/callback/exception, lifecycle/masking/slow-output, phase/conflict,
metadata/ASLR/build-mismatch, accounting/all-regions/IR11, binary/viewer and
pretty-print artifact checks passed. All existing runtime variants rebuilt.

The native parallel handler audit found only errno-access and Mach-clock external
calls; atomics compile inline. Simple equivalent builds using distinct ordinary
cache directories produced byte-identical stripped executables. The complete
three-stage ARM64 bootstrap then passed its stage-2/stage-3 byte comparison and
installed the final compiler. Ordinary and parallel profiling/IR checks also
passed with that rebuilt compiler. Artifacts are retained locally under
`/tmp/mlkit-t7-bootstrap-fixed`, `/tmp/mlkit-t7-repro`, and the
`/tmp/mlkit-t7-new-compiler-ordinary` / `-parallel` directories. Cloud CI for this
change and browser visual layout remain unverified. The checks establish the
supported configuration, not an exact timer delivery rate or overhead bound.
