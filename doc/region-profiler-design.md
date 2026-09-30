# A sampled region profiler

Design for [issue #237](https://github.com/melsman/mlkit/issues/237).
This document specifies the overall design. M1–M7 are implemented.
See [usage, validation, and measurements](region-profiler.md).
ARM64 execution checks cover the implemented runtime combinations. Initial
ThinkPad X64 execution passed accounting/graph checks and exposed two issues
(GCC weak-constant folding and profiler-call alignment), now fixed. The complete
X64 rerun is pending after SSH became unavailable. Milestone boxes track
implementation, not completion of outstanding cross-backend validation.

## Objectives and scope

The profiler records how region storage grows and shrinks during execution,
with views by region, thread, worker, and aggregate. It uses ordinary object
representations and support in the normal runtime, without the old profiler's
per-object descriptors or separate profiling runtime variant.

The supported targets are X64 and ARM64, initially without GC, including
parallel execution. Single-threaded GC and generational GC follow. Combining
GC with parallel execution is a separate investigation and is out of scope.
The design also covers the REPL and explicit regions in ReML.

This is a sampled storage profiler. It does not reconstruct object allocation
sites or establish which objects are reachable. Short-lived regions and peaks
between samples may be missed. Footprint and stack maxima are sampled maxima. The allocated-page maximum is
maintained exactly by a process-wide counter, including while sampling is paused.

## Measurements

### Infinite regions

Traverse the page-list links to count pages, without inspecting page contents.
For each generation, calculate:

```text
page footprint = page count * page size - unused bytes in the last page
```

Record page count and unused tail as well as the resulting footprint so that
the viewer can also display reserved page capacity. The footprint includes
page headers and unused space left in earlier pages; it is not exact object
payload size. Use the runtime's actual page size for the selected variant.

Do not maintain per-region page counts. Measure traversal overhead first. If
needed, a later optimization can maintain per-region counts at page growth, reset, and
reclamation, without instrumenting every small allocation.

Large objects are allocated outside the page lists and must be accounted for
separately. The ordinary large-object descriptor does not retain the requested
size. The initial implementation should record sizes in a side table at large
allocation and remove them when the objects are freed. This preserves object
and region-descriptor layouts. These hooks must cover allocations in C runtime
helpers as well as generated code, and remain consistent across collection.
Enabling the session before program initialization ensures that pre-phase
allocations are known even when sampling starts paused.

### Finite regions and stacks

For each active finite-region binding, count the space set aside for it on the
stack, using the compiler-known size. Do not inspect its contents or attempt
to count initialized objects. A zero-sized binding contributes zero bytes.

Frame metadata identifies which bindings are active at a sampling point.
Storage reused by successive bindings must not be counted twice. Physical
frame storage outside active finite regions belongs to the other-stack-storage
category, even if a later binding will use it. Region descriptors and other
stack storage are separate categories, preventing double counting when a
viewer displays total stack use.

### GC

Sample only at stable points outside collection. A request made during GC
remains pending until collection finishes. Traverse pointers using the
runtime's masking helpers for generation and page metadata bits.

For ordinary GC, apply the page calculation to `g0`. For generational GC,
apply it independently to `g0` and `g1`, subtracting each generation's unused
tail, then sum the results. The current region-memory-usage helper subtracts
only the `g0` tail and omits large objects; it cannot be reused unchanged as
the full accounting function.

Finite-region stack reservations are counted in the same way with GC enabled.
Optional before/after-GC samples expose reclamation, with records identifying
the collection and whether it was minor or major. They do not trigger an
additional collection. Between collections, footprints include unreachable
objects that have not yet been reclaimed.

Cached free pages are distinct from storage assigned to regions. Where
reported, keep them in a separate runtime category: freeing a region need not
reduce memory retained by the process. Region footprints are not process RSS.

## Compiler metadata and safe points

A proposed compiler option, `-region_profile`, emits profiling metadata and
polls independently of the existing `-prof` mode and GC options. It links the
ordinary runtime for the selected execution mode. Profiling is disabled at
runtime unless explicitly enabled.

`CalcOffset` already knows region-binding offsets and finite sizes. Extend the
backend metadata to describe, at each sampling point and call continuation:

- Frame layout and how to find the caller.
- Active region bindings, with stack offsets, finite/infinite kind, static
  region IDs, and finite sizes.
- A terminating or bridging record at ML entry boundaries.

Return-address-associated metadata is already used by GC. Generalize the
necessary machinery so it also exists without GC, without changing the
existing GC descriptor ABI accidentally. Put names and source information in
per-unit tables; use compact references in continuation metadata rather than
repeating strings at every call site.

The current frame needs an explicit map at the poll; caller return addresses
alone are insufficient. Polls must cover function entries and optimized loop
backedges. Instrument only at states where descriptors are initialized and
the stack layout is known. Respect register preservation and each backend's
calling convention, including ARM64's link register. Audit tail calls, frame
elision, exception handlers, and C-to-ML callbacks.

All ML frames crossed by the walker need compatible metadata, including
library frames. Include the option in compilation-cache identities and
diagnose unsupported mixed builds. In particular, an executable with no
profiling metadata must reject `-rp` with a recompilation instruction.

## Threads and attribution

Use cooperative sampling. A timer or explicit request advances a sampling
epoch. Threads acknowledge it at safe points and publish a stable ML stack
position. Collect a consistent snapshot, resume execution, and serialize
copied records outside the pause. Never walk a running thread's mutable
stack or region lists asynchronously.

Add a live-thread registry with stable IDs and explicit lifecycle states.
Handle creation, termination, joins, blocking C calls, and callbacks. A thread
blocked in C can count as quiescent only under a defined boundary protocol;
its region state must not be read while that code can mutate it. Unsupported
foreign activity must delay or explicitly prevent a complete snapshot, not
silently produce one. Polling and rendezvous must not occur while holding
allocator locks needed by another participant.

For Argobots, distinguish logical threads from execution streams. A logical
thread waiting for an epoch must yield its execution stream so other logical
threads can reach their safe points. Suspended threads also need a defined,
stable snapshot state.

Attribute a region to the thread that owns its lifetime. Region arguments
are aliases, not additional owned regions. Count each physical region once
in aggregate, even when several threads allocate into it. The existing
`topregion` chain can help validate discovery of infinite regions.

Record available worker identity, distinguishing it from an OS CPU/core ID.
An owner grouped by its worker at sampling time is not a measurement of which
worker allocated its bytes. Physical-core attribution and allocation-origin
accounting require additional design; neither can be inferred from a stack
snapshot. Keep persistent/global regions in an explicit ownership category
where thread ownership is inappropriate.

Static region identity is a unit identity plus a binding identity; display
names alone are not unique. Initially aggregate recursive instances by
binding and owner within each snapshot. Tracking individual dynamic instances
across snapshots would need lifetime identities because stack addresses can
be reused.

## User controls

Runtime options use the existing single-dash, underscore convention, with an
`-rp` prefix distinct from the old profiler's options:

| Option | Default | Meaning |
| --- | --- | --- |
| `-rp` | Off | Enable a session before program initialization and start sampling. |
| `-rp_file PATH` | `profile.rp` | Choose the output file. |
| `-rp_interval DURATION` | `10ms` | Request periodic wall-clock samples; accept explicit `ms` and `s` units. |
| `-rp_gc_samples` | Off | Add before/after-GC samples; requires a GC runtime. |
| `-rp_paused` | Off | Enable the session but start with automatic sampling paused. |
| `-rp_report` | Off | Print profiling statistics to stderr at normal exit. |

Require `-rp` explicitly; other options configure it. Reject malformed values
and invalid combinations rather than silently ignoring them. An interval of
`0` disables periodic requests, allowing explicit or GC-only sampling. The
initial `10ms` default is provisional pending overhead measurements.

The interval requests sampling; actual capture occurs at safe points. Store
actual timestamps and coalesce timer requests when sampling cannot keep up.
Do not build a queue of overdue timer ticks. Use a monotonic wall clock shared
by all threads, and retain request/capture timing sufficient to report delay.

Examples:

```sh
./program -rp
./program -rp -rp_interval 50ms -rp_file experiment.rp
./program -rp -rp_interval 0 -rp_gc_samples
./program -rp -rp_paused -rp_report
./program -rp -rp_interval 20ms -- application-arguments
```

Add explicit `--` support to the runtime argument parser and remove that
separator along with runtime options from `CommandLine.arguments`. Application
arguments after it must be passed through unchanged. Preserve the existing
first-unrecognized-argument behavior for invocations without the separator.

The report should include completed and coalesced requests, frames and pages
visited, sampling CPU time, and wall time spent waiting for threads separately
from capture and serialization. Sampled maxima must be labelled as such.

### ML and REPL control

Provide a small proposed interface:

```sml
signature REGION_PROFILE =
sig
  val start : unit -> unit
  val pause : unit -> unit
  val sample : unit -> unit
  val mark : string -> unit
  val flush : unit -> unit
end
```

Controls apply to the process-wide session. `start` resumes rather than
resetting the session or file; `pause` stops periodic and GC-triggered samples.
Take boundary snapshots when transitioning into and out of active sampling.
Repeated start/pause calls in the same state do not add boundary snapshots.
Explicit `sample` works even while paused and completes the coordinated
snapshot before returning. `pause` completes its boundary snapshot before
returning. Both need safe runtime bridges and must permit other participants
to make progress. Markers record a label and timestamp even while paused;
`flush` writes buffered records without requesting a sample. With no enabled
session, all operations are no-ops. Sampling while paused does not disable the
bookkeeping necessary for later snapshots, including large-object sizes.

The REPL should expose these operations and forward startup profiler options
to its runtime process. Keep a session across phrases. Each loaded unit carries self-describing maps and interned name references,
available before its initializer runs, with unique compiler-generated unit
identities to avoid collisions between phrases. Stream binding definitions are
emitted on first observation; no second native-image registry is needed while
loaded code remains resident. Account for persistent/global
regions even while no phrase is executing, and define the idle REPL boundary.
The existing dynamic GC-image registration provides a model, but profiling
registration must also work without GC. Loaded code currently stays resident;
future unloading must unregister its metadata before releasing it.

## Output and visualization

Use `profile.rp` by default. Design a versioned, append-only format with
fixed-width counters, explicit units, unit/region definitions, thread
lifecycle records, timestamps, samples, markers, and GC events. A reader
should be able to use complete records in a truncated file and consume a
flushed prefix while the process is running. Record completeness and timing
information so delayed or incomplete captures cannot masquerade as exact
simultaneous snapshots.

Extend the HTML viewer with combined, colored region-and-stack graphs (M7).
Keep `rp2ps` unchanged. A separate future integration could add a new input path
there; legacy object-allocation-site views cannot be reconstructed from these
samples. Do not silently truncate counters or present footprint as exact payload.

Record per-thread data and perform region/thread/worker filtering and
aggregation in the generated HTML. Keep the workflow offline: the profiled
program writes `profile.rp`; the compiled SML `rpview` tool reads that file and
writes `profile.html`. Embed the template in the executable, with no Python,
HTTP server, or network dependency. Runtime filtering and any future live
viewer are separate work.

## Implementation milestones

- [x] **M1: Single-threaded, no-GC explicit sampling.** Implementation and
  ARM64 checks are present; X64 execution checks remain pending. Implement compiler
  maps and safe sampling bridges on ARM64 and X64, finite-region reservations,
  page-list counting, and large-object accounting. Add the core session/API
  and versioned output plus a reader or aggregate graph conversion. Validate
  known page growth, unused tails, reset, nested/recursive finite regions,
  reused stack slots, zero-sized regions, and large objects. Compare measured
  bytes against controlled allocations, not just successful program exit.
- [x] **M2: Periodic sampling and overhead.** Add timer-requested sampling,
  entry/backedge polls, request coalescing, the runtime flags, `--` handling,
  and `-rp_report`. Validate tail recursion, exceptions, C boundaries, short
  phases, and metadata compatibility. Benchmark uninstrumented, instrumented
  but disabled, enabled but paused, and actively sampled runs across intervals
  and heap sizes. Measure file size, traversal cost, and sampling delay before
  deciding whether to maintain page counts.
- [x] **M3: Pthreads.** Add the registry and coordinated snapshots, lifecycle
  records, shared-region deduplication, and owner attribution. Exercise
  concurrent growth/reset, joins, thread exit, blocked calls, and shared-region
  allocation. Check aggregate totals and stress the rendezvous for deadlocks.
- [x] **M4: Argobots and REPL/ReML.** Make the rendezvous scheduler-aware and
  report logical-thread/execution-stream identities. Add REPL option forwarding,
  per-unit metadata registration, persistent-region accounting, and explicit
  ReML names. Test more logical threads than workers, joins and suspension,
  repeated phrases, retained closures, exceptions, and duplicate region names.
- [x] **M5: Single-threaded GC and generational GC.** Add stable pre/post-GC
  snapshots, per-generation tail accounting, and large-object reclamation
  tracking. Validate minor/major collections, both generations' unused tails,
  finite stack reservations, and requests arriving during collection. Repeat
  relevant REPL checks with GC. GC plus parallel execution remains excluded.
- [x] **M6: Visualization.** Provide offline region/thread/worker views using
  the file record model and expose measurement/attribution semantics. The
  offline workflow replaces the earlier HTTP viewer; no live-control socket
  or external command listener is included.

- [x] **M7: Stacked region-and-stack graphs.** Add a graph showing all region
  bindings and the stack together as colored, stacked areas over time, with
  a legend and labelled axes: elapsed time with explicit units (for example,
  seconds), and memory with explicit units (bytes, KiB, MiB, or GiB). Keep each
  region's color and vertical order stable across the displayed timeline. Sort
  region bands by the sum of their sampled sizes, smallest first, matching
  `rp2ps`; use deterministic tie-breaking. Show the nine largest regions by
  default, with a user-selectable limit (0 means all) and an Other band for the
  remainder. Order the legend top-to-bottom to match the graph, with optional
  placement on the right. Provide checkboxes for base names, finite/infinite
  kind, inferred region type, and peak page capacity. Add a Pages
  metric in memory units, with a horizontal process-wide peak page-capacity
  line in the aggregate Pages, Regions + ML stack, and Page footprint views.
  Count assigned pages, excluding free caches and including GC from/to-space
  overlap; maintain no maximum stack counter.
  Support the aggregate of all threads and filtering to one selected logical
  thread or execution stream. Label execution streams as such: physical-core
  selection must not be inferred from worker IDs. Record the OS logical CPU
  at the publishing safe point on Linux; permit CPU filtering, including changes
  of CPU between samples. CPU identity is unavailable on macOS and is labelled
  accordingly. This is logical-processor attribution, not a physical-core or
  allocation-origin measurement.
  Attribute shared regions once by lifetime owner, consistent with the existing
  stream, and make the treatment of persistent/global storage visible in
  filtered views. Extend the sampled stream to measure active ML stack storage;
  version 3 adds per-thread active/finite/remaining-stack byte counts, while
  older files display a stack-unavailable notice. Keep finite-region reservations
  in their region bands and exclude
  those bytes from the stack band; account for descriptors exactly once. Label
  the stack metric as active ML stack storage, not reserved OS stack capacity
  or arbitrary foreign-call stack usage. Extend the existing
  interactive HTML viewer. Implement the file-to-HTML converter in Standard ML
  under `src/Tools/RegionProfile`, with its template embedded in the executable.
  Leave the legacy `rp2ps` tool unchanged. Preserve 64-bit
  counters through aggregation and distinguish sampled footprint/stack peaks
  from the maintained page maximum. Validate
  band sums against measured totals, ordering/colors, axis units, thread/worker
  filters, shared-region deduplication, recursion/finite-stack accounting,
  missing identities, and truncated streams. Include an example graph and
  document the input and filtering options.

Run relevant generated-code and runtime checks on both native backends.
Complete the ThinkPad X64 execution rerun when connectivity is restored. Record results
and overhead numbers as milestones are implemented, rather than assuming
that existing GC tests validate the new profiler.

## Initial implementation touchpoints

- `src/Compiler/Backend/CalcOffset.sml`, `NativeCompile.sml`, and
  `FrameLayout.sml`: active binding maps, offsets, and GC-independent emission.
- `src/Compiler/Backend/X64/` and `Arm64/`: continuation records, safe-point
  stubs, loop polls, entry boundaries, and register preservation.
- `src/Runtime/Region.h` and `Region.c`: stable page traversal, per-generation
  footprint helpers, and large-object bookkeeping.
- `src/Runtime/Spawn.h` and `Spawn.c`: thread registry, lifecycle, ownership,
  and scheduler-aware rendezvous.
- `src/Runtime/GC.c`: stable sampling boundaries and collection events.
- `src/Runtime/CommandLine.c` and `Runtime.c`: flags and session lifetime.
- `src/Manager/Repl.sml`, `src/Runtime/Repl.c`, and the dynamic-image machinery:
  runtime controls and per-unit profiling registration.
- `src/Tools/Rp2ps/`: offline rendering compatibility or conversion.

Keep the old profiler operational while the new one is developed. Its
object-level data and the new footprint samples have different semantics;
the new implementation should not silently change old profiling behavior.
