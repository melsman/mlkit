# Sampled region profiler

Runtime options below belong inside `+RTS ... -RTS`; see
[runtime arguments](runtime-arguments.md) for delimiters and application arguments.

The compiler flag `-rp` is an alias for `-region_profile` in both MLKit and ReML.
For batch compilation it emits profiling metadata; run the resulting executable
with `+RTS -rp -RTS` to start recording. In an interactive session, either spelling enables
both metadata generation and profiling of the session runtime.

Compile the executable and its ML dependencies with `-rp` (or `-region_profile`). Enable a
session with the executable's `-rp` option. This is the unified object-and-region profiler; rpview is the sole interface. See [site occupancy](allocation-profiler.md).

```sh
mlkit -no_gc -rp -o app app.mlb
./app +RTS -rp -rp_interval 10ms -rp_file profile.rp -rp_report -RTS
rpview profile.rp --output profile.html
```

Supported combinations are single-threaded no-GC, GC and generational GC, and
no-GC pthreads or experimental Argobots. The Argobots runtime is an optional
source build and is not shipped or installed by the normal release targets. **GC plus parallelism remains excluded.** Both
native backends emit the metadata and polling bridges. The complete profiler
CI suite passes on ARM64 and native Linux X64, including GC, generational GC,
pthreads, REPL sessions, and read-only installed API caches. The ThinkPad X64
compilers were built with MLKit; runtime archives were built with GCC 15.2.
X64 validation also passes the standalone accounting fixture under ASan/UBSan.

## Controls

| Runtime option | Meaning |
| --- | --- |
| `-rp` | Enable a process-wide session. Otherwise the API and runtime bookkeeping are disabled. |
| `-rp_file PATH` | Output path; defaults to `profile.rp`. |
| `-rp_region UNIT:BINDING` or `-rp_region all` | Record site occupancy for one binding or every infinite region. With `all`, choose a region later in rpview. |
| `-rp_interval Nus`, `Nms`, `Ns`, `Ni`, or `0` | Integral wall-clock interval (e.g. `400us`); defaults to `10ms`. `Ni` samples every N compiled ML function entries per thread. Zero disables automatic periodic snapshots. |
| `-rp_paused` | Initialize the session and bookkeeping, but pause automatic samples. |
| `-rp_gc_samples` | Add paired before/after-GC snapshots, with the collection kind. Requires GC. |
| `-rp_report` | Report completed samples, frames/pages traversed, timing, skipped requests, the sampled peak, and maximum allocated page count. |
| `-RTS` | End the runtime block; application arguments follow. |
| `--RTS` | End all runtime parsing; discard this marker. |
| `--` | End all runtime parsing; preserve this marker and following arguments. |

### Sampling by function entries

`./run +RTS -rp -rp_interval 8000i -RTS` requests a snapshot every 8000
compiled ML function entries in each thread. `1i` samples at every entry;
`0i` is invalid (use plain `0` to disable periodic sampling). Each thread has
its own countdown; a parallel snapshot still includes all participating threads.
Tail calls and optimized self-recursive loops count. Inlined calls and C calls
are not separate entries. Counting continues while paused, but produces no
snapshots until recording resumes. Explicit and GC snapshots remain available.

This mode installs no timer or signal handler. Snapshot timestamps remain
wall-clock times, so blocking I/O can still leave gaps in the graph. It adds a
counter decrement to entry checks; small counts can produce substantial overhead
and large profiles. Recompile ML code with the updated compiler to obtain the
entry counters.

Configuration options require `-rp`. Include `kitlib/region-profile.mlb` for
`RegionProfile.start`, `pause`, `sample`, `mark`, and `flush`. Start and pause
capture their transition boundaries and are idempotent. Explicit samples work
while paused. Marks and flushes also work while paused. Disabled operations are
no-ops. All snapshots describe actual safe-point capture times, not reconstructed
states at the timer deadline.

The REPL accepts the same profiler options at startup. For example:

```sh
mlkit -no_gc -rp -rp_interval 20ms -rp_file repl.rp
mlkit -gengc -region_profile -rp_gc_samples
```

Runtime options are forwarded to its child process and remain fixed across
phrases. The ML API controls the running session. Normal REPL termination now
uses the runtime's termination protocol, allowing the profiler to close its
output and report. A rejected runtime startup is detected without hanging on
the command FIFO. Idle REPL polling captures persistent regions with an explicit
end-of-ML-stack anchor.

## Accounting and metadata

* Finite bindings contribute only to ML stack storage, including uninitialized
  reservations; they have no object descriptors or separate region bands.
* Infinite bindings are measured by following page links and subtracting the
  unused tail of the last page. Page headers and slack in earlier pages remain
  included. Selected regions additionally scan packed object descriptors for site occupancy.
  **No per-region page count is maintained**. A process-wide atomic live count and high-water mark track page
  allocation, reset, release and GC reclamation while `-rp` is enabled, even
  when sampling is paused. Released chains are counted by following their links.
  The maximum excludes cached free pages and includes simultaneous GC from-space
  and to-space. No maximum stack counter is maintained.
* Both generations have their own page counts and unused-tail subtraction.
  Large-object allocation sizes are recorded separately and removed on region
  reset/release and GC reclamation.
* Descriptors and free-page caches are reported separately. The displayed region
  footprint sums page footprint and large-object bytes;
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

Native map version 4 contains relative source-name
references, inferred region types and source filenames.
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

Periodic sampling reserves `SIGALRM` and `ITIMER_REAL` and rejects an
existing timer/handler at startup. Applications using them must use
`-rp_interval 0`. The profiler retains its harmless signal
handler until process exit, because a signal can still be pending on another
OS thread after timer disarm. Pausing disarms the sampling timer;
`-rp_interval 0` installs no timer or signal handler.

Requests coalesce; they never build a sample backlog. `coalesced` in the report
estimates elapsed superseded timer intervals. `wait_ns` includes time queued
behind another snapshot and rendezvous waiting. Traversal and serialization
wall time are separate; CPU time measures the sampling thread. These are
measurement costs, not allocation costs or an application-wide CPU profile.

## Stream and offline HTML viewer

Output is a compact version-10 binary stream. `rpview` accepts only this format; older profiles must be regenerated. Each captured thread has a `stack` record with
`active_bytes`, `finite_bytes = 0`, and `stack_bytes = active_bytes`. Finite-region
storage is included in the stack, not split into separate bands.
The active span runs from the innermost captured ML frame through the outermost
ML return slot, including alignment and spilled-result reservations. It excludes
the sampler's C frames, foreign-call frames, and unused OS stack capacity.
The reader validates stack arithmetic, generation/page accounting and the
required current metadata: source names, region kinds/types, page maxima, cache
bytes and GC counts. Native frame maps use version 4.
On macOS ARM64, new recordings also retain function code ranges and the
executable's UUID/load address for offline interrupted-PC resolution. See
[T2 function metadata](time-profiler/t2-function-metadata.md) for the metadata
extension, `rpview --resolve-pc`, supported static-code scope, and unknown-PC
handling. This metadata does not yet enable time sampling.
Binding definitions carry a session-local `definition` ID and the static
`unit`, `binding`, `source`, `name`, `kind`, and `region_type` fields. Definitions
are emitted once, immediately before the first snapshot that uses them; no
up-front list is needed, including for code loaded by later REPL phrases.
A region measurement contains only its `definition` reference, sample/thread/
worker/CPU identities, and changing storage counters. Different metadata for the
same source binding receives a separate definition; source binding identities
still control graph grouping. `rpview` resolves references and rejects undefined
IDs and duplicate definitions. Static metadata has no slot in measurement records.
Relink profiled executables with the current runtime and regenerate data files
to obtain site occupancy; native compiler frame maps are unchanged.

Records include binding definitions, thread lifecycle events, markers, samples,
skipped requests, and normal session termination. A sample is committed by
`sample_end`; readers ignore an incomplete final record or unfinished sample.
The file starts with eight bytes: `4d 4c 4b 52 50 00 0a 00` (`MLKRP`, NUL,
version 10, NUL). Each record starts with a little-endian 32-bit payload length,
followed by a one-byte tag and its fixed-order fields. Numeric fields are
little-endian 64-bit integers; worker/CPU `-1` uses the all-ones representation.
Strings are byte sequences with a little-endian 32-bit length, preserving
embedded NULs in markers. The format uses neither native structs nor pointers,
so decoding is independent of architecture, alignment and host byte order.
The runtime buffers one encoded record and writes it through buffered stdio.
Tags and field order are specified by `ProfileBinary.schema` in
`src/Tools/RegionProfile/Binary.sml` and the matching runtime writer.

For readable output, dump the decoded records as JSON Lines:

```sh
rpview profile.rp --format json                 # stdout
rpview profile.rp -o profile.json               # file; infer format
rpview profile.rp --format json -o -            # explicit stdout
```

JSON output preserves the record sequence, definition IDs, timestamps, markers,
thread events and exact decimal integers; it does not apply graph filters or
expand definitions into every measurement. A partial final binary record is
ignored. Complete records from an unfinished sample remain visible in the dump,
but that sample is excluded from graphs. JSON is an inspection output, not a
second supported input format. Do not parse large counters as floating-point
numbers in external tools.

The stream preserves unsigned 64-bit counters. The viewer transports them as
decimal strings and sums them with `BigInt`; only chart coordinates use floating
point.

`rpview` is a compiled Standard ML program in `src/Tools/RegionProfile`.
It reads a binary profile file and writes HTML, SVG, or decoded JSON Lines. Both building
and running the tool require no Python. The HTML/JavaScript template is embedded
in the executable at build time by a small SML helper; the installed executable
needs no template files. There is no HTTP server, live polling, or network access
in the tool or generated page. Open the output directly in a browser and
regenerate it to include new samples.

The normal build/install includes `bin/rpview`. Both the viewer and its HTML
embedding tool use the seed compiler selected by `./configure --with-compiler`.
To build the viewer separately:

```sh
make rpview
bin/rpview profile.rp --output profile.html
```

The page caption is `Region profile for X (GC Y)`, where X is the main source
basename (the final linked ML initialization unit) and Y is `enabled` or
`disabled`. REPL sessions use `REPL`. The header records `main_source` and
`gc_enabled`, so this works even without completed snapshots. For GC-enabled
programs, the page shows the number of completed collections below the graph.
The counter includes collections while sampling is paused and does not require
`-rp_gc_samples`. `sample_end` and `session_end` carry `gc_collections`; a file
without its final summary shows the recorded count as incomplete. A header-only
file has no recorded collection count yet; this is shown as unavailable.

The defaults are `profile.rp` and `profile.html`; `-o` is an alias for `--output`.
The tool validates the stream before opening its output and rejects an output
path that aliases its input. The offline HTML has no external dependencies. Its default graph stacks the nine largest
region bindings and the remaining ML stack as colored bands. **Regions shown**
sets the number of individual regions (0 retains all); the largest are selected
by summed sampled size. **Other** sums the omitted regions at each snapshot and
occupies the bottom band. The stack does not count against the region limit.
Remaining bands run smallest to largest, bottom to top, as in `rp2ps`. The
legend follows their appearance from top to bottom. A binding keeps its color when changing
snapshots or filters. Axes choose elapsed-time units (ns, µs, ms or s) from the recorded elapsed
time and memory units (bytes, KiB, MiB, GiB, etc.) from the displayed range.
Captions, markers and peak annotations use the same units. Band tooltips also
include exact byte counts, and tables retain exact byte counters.
Finite reservations are part of the ML stack band. Infinite-region descriptors
stored in active ML stack frames are already part of that span; global and
persistent descriptors outside it are excluded. Descriptors and free-page caches
are not added again. The **ML stack + finite regions** metric (`--metric stack`)
shows the active stack, excluding region pages and large objects. The runtime
report's `sampled_peak_bytes` remains a region-only peak; the default graph's
sampled maximum includes stack.

Only **Legend on the right** is checked by default; base names, type and
peak capacity start hidden. **Show base names** includes the
source filename in region labels, such as `life.sml` (global regions use
`global`, and interactive code uses `REPL #N`). Full source paths and internal
unit identifiers appear in hover details. The internal unit identifier remains
the aggregation key, so matching filenames do not merge distinct regions.
Profiles contain only infinite regions; finite storage appears in the stack.
Infinite regions use pages and may hold large objects. **Show region type** adds the compiler's
inferred `top`, `bot`, `pair`, `triple`, `string`, `array`, or `ref` type. The `region_type` field in binding definitions carries this information;
an unavailable inferred type is displayed as `type unavailable`.
These label options apply to the legend,
band tooltips and region-grouped table without merging distinct bindings.
**Show Peak page capacity** toggles the reference line and its annotation;
when hidden, the memory axis scales to the sampled bands alone.
**Legend on the right** places the legend beside the graph on wide screens;
it defaults to on and falls back to below the graph on narrow screens. While
selected, labels use compact region IDs such as `r5` instead of `Region #5`,
retaining any selected base names, kinds, types and explicit region names. Hover
over a legend label, graph band, or region-grouped table label to see the full
source path, base name, type and kind, regardless of the label-display checkboxes.

Region types come from native frame-map version 4 (magic `0x52504d34`), which
includes a source-name reference per frame and one type word per binding,
plus a linker-generated table of global region
slots and types. Compile programs and their dependencies
with the current compiler and runtime to obtain version-10 profiles. No object scans or
allocation bookkeeping are needed to obtain region types.

Release builds precompile the Basis (including REPL support) and Kit libraries
together with `kitlib/region-profile.mlb`, for ordinary and sampled-profiler
builds in three configurations: non-GC,
non-GC with pthread parallelism, and GC. The sampled variants use `-region_profile`
and mode-specific caches such as `RI_PROF`, `RI_GC_PROF`, and `RI_PROF_PAR`
(with an `ARM64_` prefix on ARM64), without ABI-version suffixes.
Use `make -j6 all` (or `make -j6 mlkit_basislibs`) to build the six independent
library variants concurrently. Each variant builds its Basis, profiler API, and
supported ReML libraries in sequence to avoid competing writes to the same cache.
`make -j6 mlkit_kitlibs` also builds the full Kit libraries after these prerequisites.
The profiler API sources and their matching caches are installed under
`$(SML_LIB)/kitlib`, allowing MLKit and ReML clients to import the API from
a read-only installation. `test/region_profile/check-installed-api.sh` checks
all six MLKit configurations and ordinary/profiled ReML with and without
parallelism against such an installation. `basis/reml.mlb` (the `Region`
structure) is also precompiled with ReML for those four non-GC configurations
and installed in the matching Basis caches. The check exercises both APIs,
including an explicit region parameter.
All three sampled Basis builds use `-log_to_file -Prfg -Ppp -Pcee` to
write per-source diagnostic logs, including region-flow graphs, program points,
and call-explicit code. These logs are installed alongside the IR files in each
variant's cache directory (for example,
`basis/MLB/ARM64_RI_GC_PROF/List.sml.log`). Use the logs for inspecting compiled
library code; the `.ir` files provide metadata for rpview. Profiling compilations
with `-log_to_file` also use this layout for application sources. Printing
options and missing diagnostic logs never force recompilation of cached code,
whether the cache is writable or read-only. Logs reflect the settings used
when a unit was last compiled. To obtain different diagnostics, explicitly
rebuild the relevant sources in a writable checkout; installed library logs
are supplied at installation time.

Small blue ticks above the time axis mark every completed snapshot. Thin red
bars below it show GC intervals, from the end of a `before_gc` snapshot to the
start of the matching `after_gc` snapshot recorded with `-rp_gc_samples`. These
are observed intervals, including any intervening profiler overhead, rather
than separate measurements of collector CPU time. A GC completion without a
matching start remains a red tick. Very short bars have a minimum width of one
SVG unit for visibility. Counts alone do not supply timestamps, so profiles
recorded without GC snapshots cannot show individual GC intervals.
Both HTML and SVG outputs include these marks.

**Download SVG** saves the current graph as a standalone vector image. The
caption, selected metric/view, graph, right-side legend and applicable GC/
peak notes are all inside the SVG. The graph retains its 3:2 aspect ratio;
long labels wrap and the outer image grows to accommodate every legend entry.
Export follows the selected regions, filters, label options and peak display,
regardless of where the HTML legend is placed. No server or external assets are
needed. The ML-stack band explanation is omitted from SVG output. Axis and
legend text use 16 SVG user units, and the right margin adjusts to the
legend width. Open the SVG in a vector editor or convert it to PDF when needed.

`rpview` can also generate SVG directly, entirely in Standard ML:

```sh
rpview profile.rp -o profile.svg
rpview profile.rp -o pages.svg --metric pages --regions 9 --show-peak
rpview profile.rp -o thread.svg --scope thread:2 --caption 'Thread 2 allocations'
rpview profile.rp -o profile.html --show-base --show-type
```

The output extension selects SVG or HTML; `--format svg|html` overrides it.
Without an output path, the default is `profile.html` (or `profile.svg` with
`--format svg`). Both outputs accept `--caption TEXT`, `--regions N` (0 = all),
`--metric total|stack|pages|page_footprint|large_bytes|finite_bytes|descriptor_bytes`,
and `--scope all|thread:N|worker:N|cpu:N`. Worker/CPU identity `-1` selects
unavailable identities. `--show-base`, `--show-type`, and
`--show-peak` enable the corresponding settings; `--hide-*` disables them.
`--legend-right` (default) selects compact region names and a right-hand HTML
legend; `--legend-below` selects longer names and a legend below the HTML graph.
SVG always places its legend inside the image on the right. `--group
aggregate|region|thread|worker` selects the HTML table grouping. These options
set the initial HTML controls, which remain interactive. `--help` lists them.
No completed snapshots is an error for SVG; HTML can display an empty profile.

The colour palette cycles through contrasting hues, starting with the largest
regions by summed full-profile occupancy. Colours remain fixed across metrics,
region limits and thread/core filters, and match between HTML and direct SVG.
ML stack and Other have separate neutral colours.

The **Pages** metric shows the full memory capacity of assigned region pages,
without subtracting unused tails. Its axis uses scaled memory units and its table
shows exact bytes, with page counts in a separate column. **Page footprint**
subtracts unused last-page tails in each generation. In the aggregate **Pages**,
**Regions + ML stack**, and **Page footprint** views, a red horizontal line shows
the process-wide maximum allocated page count multiplied by the recorded page
size, including allocations between snapshots. This is a page-capacity reference,
not a maximum for combined region-and-stack memory; stack storage and large
objects are excluded. The axis accommodates both the bands and reference line. This line is omitted in filtered views
because the counter is process-wide. The `max_pages` fields on `sample_end` and
`session_end` records store the running and final maximum. The reader uses the
largest recorded value, including the final summary after the last snapshot;
a truncated file can only report the maximum recorded before truncation.

The **View** selector applies to both graph and table: all threads, one logical
thread, one Argobots execution stream, or one OS logical CPU where recorded.
Linux records the CPU when a thread publishes its safe-point anchor; migration
can move that owner's storage between CPU bands over time. This is not physical
core topology or allocation-origin tracking. Execution-stream selectors and table
grouping appear only when the profile
contains recorded execution-stream IDs; ordinary profiles omit these controls.
Argobots profiles label this support as experimental. macOS reports CPU identity as
unavailable, while Argobots execution-stream selection remains available.
Shared and persistent/global regions follow their lifetime owner's recorded
identity and are counted once. Unavailable identities have explicit selectors;
each completed snapshot includes its captured threads' stack records.

The two-handle snapshot-range slider directly below the graph is aligned with
the time axis. Drag its endpoints, or focus either handle and use the arrow
keys, to narrow the visible snapshots. The graph rescales its axes and ranks
regions within that range; the single-snapshot slider and table stay within
the range as well. **Full range** restores all snapshots. Downloaded SVGs use
the narrowed range. Allocation-attribution counters remain whole-run totals:
the current format does not record per-snapshot counter deltas.

The snapshot slider, metric selection, markers, and table grouping remain
available. Changing table grouping does not merge the region bands. Maxima are
explicitly labelled as sampled. rp2ps is retired; use rpview.

A reproducible ReML example uses three named regions with different growth/reset
phases and a growing recursive stack:

```sh
printf '%s\n' "$PWD/test/region_profile/graph.sml" > /tmp/region-graph.mlb
reml -no_par -region_profile -o /tmp/region-graph /tmp/region-graph.mlb
/tmp/region-graph +RTS -rp -rp_interval 0 -rp_file /tmp/region-graph.rp -RTS
rpview /tmp/region-graph.rp --output graph.html
```

Hover a legend entry to see its full unit/binding identity. Select a snapshot to
inspect exact values; the vertical guide shows its position on the timeline.

Use runtime flags or `RegionProfile` API operations to select phases, then
convert the resulting file to HTML or SVG. The runtime has no live-control
socket or external command listener.

## Validation and measurements

After `make mlkit_basislibs`, run `sh test/region_profile/check-ci.sh` for the
native CI suite. It uses `bin/mlkit`, `bin/reml`, and `bin/rpview`, with optional
absolute-path overrides through `MLKIT`, `REML`, and `RPVIEW`; `CC` overrides the
C compiler. The existing Linux X64 and macOS ARM64 CI jobs run this entry point.
It covers accounting, GC, pthreads, REPL sessions, binary decoding, HTML/SVG
generation, and both region APIs against a staged read-only installation.
It retains logs and reports failure details, and excludes optional Argobots
experiments, timing benchmarks, and browser-executed graph assertions.

The fixture encoder is a test-only tool built with the configured seed compiler.
Build it before running these checks individually; the CI script builds it automatically.

```sh
make rpview rpfixture
sh test/region_profile/check.sh
ARGOBOTS_ROOT=/path/to/configured/argobots sh test/region_profile/check-extended.sh
sh test/region_profile/check-binary.sh
sh test/region_profile/check-viewer.sh /path/to/profile.rp
sh test/region_profile/check-graph.sh /tmp/rp-graph-check  # open the emitted HTML in a browser
sh test/region_profile/check-svg.sh
```

The regression harness uses POSIX shell and standard tools such as `awk`,
`sed`, `grep`, `sort`, and `cmp`; it requires neither Python nor Node.js.
Scripts accept `MLKIT`, `REML`, `RPVIEW`, and `CC` overrides as appropriate.
`check-records.sh` validates streams with the compiled SML reader and checks
independent native-fixture accounting/lifecycle expectations with `awk`.
Those native fixtures have small counters; exact uint64 boundary checks use
literal expected strings in `check-viewer.sh`, avoiding awk floating-point
rounding. That suite also covers malformed records, unsupported format versions,
truncation, Unicode, injection escaping, metadata, relocation with an empty
executable search path, and input/output aliases.

`check-binary.sh` uses hand-encoded wire bytes to check little-endian decoding,
uint64 limits, JSON/stdout output, every truncation point in a final record,
and malformed headers, tags, lengths and booleans. Editable JSON test fixtures
are converted by the test-only SML encoder (`encode-fixture.sh`); `rpview` itself
accepts only binary files. The encoder uses `MLKIT` and can be overridden with
`RPENCODER` for a prebuilt executable.

`check-svg.sh` checks the direct vector output and CLI defaults; if `xmllint`
is installed, it additionally checks XML well-formedness. `check-graph.sh`
generates a self-contained browser test page containing the original graph
assertions and the current viewer script. Open the emitted `check-graph.html`
to run the interactive model/export checks and see PASS or FAIL; generating
the page alone does not run those assertions. No JavaScript command-line runtime
is required. The extended script prints all artifact paths. Build
`runtimeSystemArPar.a` first for its optional Argobots test.

ARM64 checks cover controlled finite/page/large-object totals, reset and release,
recursion, spilled results, exceptions, periodic tail loops, paused operation,
invalid intervals, repeated snapshots, shared allocations, joins and thread
exit, blocked foreign calls, GC/genGC, retained REPL values/closures and clean shutdown.
M7 checks cover exact frame spans including finite storage, recursive stack growth,
colored band sums/order, thread/worker/CPU filters (including synthetic CPU
migration), axis units, truncated/single-sample streams, and counters above
2^53. The direct SML SVG suite covers filters, aggregation, palette stability,
units, caption escaping, CLI defaults, and empty/single profiles without a runtime
PATH. Both generation
tails were also checked against exact synthetic totals under ASan/UBSan.
The current binary profiler passed 30 Argobots 1.2 stress runs on ARM64:
thirteen logical threads on one, two and four execution streams, five runs per
stream count with explicit-only sampling and five with 1ms periodic sampling.
Checks covered the 61 explicit snapshots, thread starts/exits, shared-region
accounting, worker ranks, completed streams and identical program results.
Profiling-disabled runs also passed for all three stream counts. The viewer was checked in the browser. X64 compiler builds and
cross-assembly checks cover polling, GC sampling
bridges, callbacks and thread creation. Full Linux/X64 execution validation,
including Linux CPU capture, remains pending. Experimental Argobots has not
been validated on X64 and is not a release requirement.

Measure overhead with `sh test/region_profile/benchmark.sh PLAIN INSTRUMENTED`,
using plain and profiler-enabled builds of the same workload. The script reports
seven-run medians for disabled, paused and active sampling at several intervals.
It uses POSIX `time -p`, whose resolution is platform-dependent. Use
`src/Runtime/tests/region-profile-bench.c` to isolate page-list traversal costs.
Measure representative programs and thread contention before replacing page-list
traversal with maintained per-region counts; such counts would not remove polling
or serialization costs.

## Allocation attribution

See [Selected-region site occupancy](allocation-profiler.md) for the unified
`-rp` compiler mode, launch-time region selection, per-snapshot function/site
histograms, packed descriptors, and IR navigation. All new profiles use version 10.

For site contributions within the recorded selected region, use
`rpview sites.rp --sites -o sites.svg`. For an all-regions recording, select one
region with `rpview sites.rp --region r163 -o r163-sites.svg`;
see [site occupancy](allocation-profiler.md).

Experimental time recording: [T3 recorder and runtime API](time-profiler/t3-recording.md).

C/GC attribution: [T4 boundary state and interpretation](time-profiler/t4-attribution.md).

Offline time reporting: [T5 report, estimates and time filtering](time-profiler/t5-report.md).
