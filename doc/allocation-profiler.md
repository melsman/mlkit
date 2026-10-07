# Sampled site occupancy in rpview

Runtime options below belong inside `+RTS ... -RTS`; see
[runtime arguments](runtime-arguments.md) for delimiters and application arguments.

Compile every ML unit with `-rp`, then use rpview as the profiler interface.
There is no separate allocation-volume instrumentation mode or rp2ps stream.

```sh
mlkit -no_gc -rp -o app app.mlb
./app +RTS -rp -rp_interval 10ms -rp_file discovery.rp -RTS
rpview discovery.rp -o discovery.html
# Copy UNIT:BINDING for the infinite region of interest from rpview.
./app +RTS -rp -rp_interval 10ms -rp_region 'UNIT:BINDING' -rp_file sites.rp -RTS
# Or record site occupancy for every infinite region:
./app +RTS -rp -rp_interval 10ms -rp_region all -rp_file all-sites.rp -RTS
rpview sites.rp -o sites.html
```

An explicit `RegionProfile.sample()` works with `-rp_interval 0`. Zero disables
periodic samples; it does not produce a final allocation histogram. Choose an
interval short enough to observe the region before its lifetime ends.

## Representation and allocation

Every object in an infinite region has one extra descriptor word, including
when execution-time recording is disabled. Finite objects have no descriptor;
all their storage is included in the ML stack band.

The low 16 descriptor bits encode payload size in words. Value 65535 denotes a
large object, whose full size is recorded separately. The upper 48 bits hold a
build-local site token: the aligned resident metadata address divided by eight.
Supported native platforms must place metadata below address 2^51. Token 1 is
runtime/unknown; zero is reserved for page padding. Process addresses never
appear as site identities in the file: site definitions identify the compilation
unit and its local site number, function, source and IR identity.

Small ML allocations use an inline pointer bump, boundary test and descriptor
store. Page overflow, large allocations and runtime-sized values use runtime
helpers. GC preserves the descriptor's site token, including for untagged pairs,
references and triples. Allocating foreign calls receive an explicit site token
as their last argument through the existing profiling macros. C helpers forward
it unchanged; nonallocating calls need no extra argument or wrapper.

There are no per-allocation statistics-table updates or unconditional
function-entry profiler calls. Entry polls test the pending-snapshot flag before
preserving registers and calling the sampler. Region-level page accounting
continues independently of selected-region object scans.

## What a snapshot means

The sampler stops participating ML threads at safe points. It scans descriptors
only for the selected static region binding, including each recursive instance
and both generations. Large objects are included. Payload bytes include ordinary
ML headers, but exclude the profiling descriptor, page headers and unused space.

Occupancy records identify the snapshot, a snapshot-local region instance,
binding definition, owner thread, worker/CPU and allocation site. Per-instance
summaries contain payload, object count, descriptor overhead and unused page
space. The reader checks that site totals equal the corresponding summary.
The region graph continues to show region footprint; selected-region summaries
explain its payload and profiling overhead separately.

Resetting a region removes its previous objects from later snapshots. GC removes
reclaimed objects and preserves the sites of survivors. Without GC, unreachable
objects still occupying region storage remain included. Occupancy is not
allocation volume, and short-lived objects may never appear in a snapshot.

## Viewer

With `-rp_region all`, **Site occupancy for** selects a recorded region binding
or **All regions**. The selection controls both the site graph and the allocation
table; choosing one binding also enables its region slice. The aggregate counts
each recorded object once, even when the same site allocates into several regions.
`--sites` SVG output for an all-regions profile aggregates sites across all regions;
the HTML **Download SVG** exports the current region selection.

All-region recording uses the same compiled object descriptors and allocation
path. It scans every recorded infinite-region instance at each snapshot, increasing
snapshot pauses, temporary aggregation memory and output volume. Finite regions
remain part of the stack; no per-site finite-region accounting is added.

For reports recorded with `-rp_region`, choose **Metric → Site contributions for
rN** to split the graph into allocation-site payload bands. The snapshot window,
thread/worker/CPU filters, top-site limit and Other band apply to this graph.
Colours remain stable when filtering. Click a band or legend entry to open its IR;
**Download SVG** exports the displayed site graph. Descriptors and unused page
space are excluded from site payloads. The snapshot table shows memory and object
counts instead of region-page statistics.

The snapshot slider filters site occupancy as well as the graph. With one
snapshot selected, the table shows that snapshot. With a range selected, it
shows the snapshot with the largest selected payload in that range, explicitly
identified in the heading note. Function collapse therefore sums objects present
at the same time. Thread and execution-stream filters apply to ownership at the
snapshot, not to the allocating thread. Existing IR navigation and region slices
use the same site metadata as before.

For a standalone stacked SVG of site contributions over time:

```sh
rpview sites.rp --sites -o region-sites.svg
rpview sites.rp --sites --regions 0 --scope thread:0 -o thread-sites.svg
```

`--sites` uses the region selected when recording with `-rp_region`, or all
regions when recorded with `-rp_region all`. Each band
shows one site's payload occupancy at each snapshot, summed across selected
region instances. `--regions N` limits the largest site bands (default nine);
remaining sites form Other. Colours remain stable across thread/worker filters.
IR function names are used when matching companions are available. Descriptors,
page headers, unused space and stack storage are excluded and labelled as such.
This option requires SVG output and version-10 occupancy data; ordinary region
SVG export is unchanged.

Region and stack maxima are observed snapshot maxima. The separate process-wide
peak page-capacity counter is not a true maximum of live payload or ML stack.
`-rp_interval Ni` samples every N compiled ML entries per thread; `1i` samples
every entry. Even this does not guarantee true maxima between entries.

## Implementation milestones

* U1: one infinite-object representation and site namespace across units;
  unknown runtime origins are explicit.
* U2: descriptor-aware inline allocation and pending-flag polling on ARM64/X64;
  finite storage is stack storage and legacy counters are removed.
* U3: version-9 occupancy records and reconciled per-instance summaries.
* U4: snapshot/thread filtering, region slices and IR navigation in rpview.
* U5: descriptor, reset, large-object, GC, exception and backend validation;
  the old profiler stream and rp2ps build/install paths are retired.

Validation covers ARM64 and X64 under Rosetta: packed-size boundaries, finite
stack storage, page overflow, reset, large objects, callbacks/exceptions, ordinary
and generational GC, tagged pairs, and pthread occupancy. Viewer checks cover
range filtering, incomplete snapshots, malformed occupancy summaries, and IR
navigation. Read-only installed libraries were checked with MLKit and ReML,
including pthread and GC variants. Argobots was not exercised in this validation.

The IR milestones below describe the retained metadata/navigation facilities.

## IR5: one identity from IR to objects

Physical-size inference assigns each allocation site an ID, using the existing
program-point counter location. Subsequent transformations retain that ID.
Within a compilation unit, both native backends reuse one resident metadata
record for all allocations from that site. Duplicated allocations therefore
aggregate during the snapshot scan, while the IR table can retain several spans
for the same site.

Allocating foreign calls and their argument-storage allocations share the
initiating call's site and location kind. Generated allocations without an
origin receive fresh IDs from the same counter and explicit generated metadata
(kind 2); runtime/unknown uses site 0. Generated sites do not link to IR spans.

Stream version 10 removes the site-to-program-point field: a site's ID directly
indexes the IR location table. The internal region-analysis `pp` type and the
pretty-printer's generic `mark` name remain, but are no longer separate allocation
identities. Older profiles must be regenerated; rpview accepts version 10 only.
The site metadata ABI changes (capability 4), so rebuild
compiled ML units and the runtime together. Object descriptor packing and the
allocation fast path are unchanged.

## IR2: saved call-explicit IR and location tables

Native `-rp` builds save a companion `<object>.ir` file for every emitted ML
object, including cached library units and separate functor-generated objects.
The file contains the call-explicit IR with allocation specifiers marked only
in the trailing location table, not with inline program-point numbers. The
location-aware layout keeps explicit allocation forms and K-normal bindings:
list and infix shorthand must not collapse distinct allocation program points.
It otherwise uses the compact `-abbrev` layout, omitting redundant call labels,
empty region argument lists, and unnecessary zero-size region bindings. Unlike
ordinary abbreviation, it retains allocation specifiers needed for navigation.
IR3 connects attribution sites to these program points and packages the IR in
HTML reports. IR4 provides interactive site navigation in the viewer.

Ordinary `-Pcee` diagnostic output is unchanged. Add `-Pcee_locations` (long
name `-print_call_explicit_locations`) to request the location-aware IR and its
trailing table. The flag alone does not request printing. These diagnostics go
to the usual output/log stream; unrelated printing flags never add other passes
to the automatic `.ir` artifact. `-Ppp` remains available for ordinary diagnostic
program-point annotations; location-aware output keeps these numbers in its
table instead.

### Companion file format (version 2)

The file starts with seven newline-terminated header lines:

```text
MLKIT-IR 2
identity<TAB><identity minted for this compilation>
unit<TAB><compilation-unit identity>
source<TAB><source filename>
object-md5<TAB><digest of the exact object bytes>
content-md5<TAB><digest of this IR artifact>
code-bytes<TAB><number of bytes in the following IR text>
```

Unit and source fields use Standard ML string escapes (`String.toString`, without
surrounding quotes), so tabs and newlines in names cannot split header fields.
The unit is the same printed main label used in sampled-profiler metadata.
Exactly `code-bytes` bytes of IR follow, then this table:

```text
MLKIT-IR-LOCATIONS 1
mark<TAB>start<TAB>length<TAB>line<TAB>column
<program point><TAB><byte offset><TAB><byte count><TAB><line><TAB><column>
...
MLKIT-IR-LOCATIONS-END
```

A newline separates the code from the table marker. `start` is a zero-based
absolute byte offset in the `.ir` file, and `length` is a byte count. Lines and
byte columns are one-based. Repeated program points have separate rows; dummy
parameter points have no rows. In diagnostic output, offsets/lines instead start
at the first byte after `MLKIT-IR-BEGIN` and its newline, so multiple IR blocks
can coexist in a general compiler log.

The content digest covers the complete file with the 32-character `content-md5`
value replaced by 32 zeroes. It detects changes to the code, metadata, or table;
the object digest binds the artifact to its emitted object. These are consistency
checks, not authentication. The writer closes a temporary companion file and
renames it into place after the object has been assembled successfully.

Cache reuse for `-rp` checks both digests. Missing, truncated, corrupt, or
mismatched companions require recompilation; they are not regenerated from a
potentially different IR while retaining the old object. Existing profiling
caches predating IR2 are therefore rebuilt as needed. The Basis installer copies
`.o.ir` files alongside objects; relocating an intact pair preserves validity.
Validation reads the object and companion bytes at build time, adding cache-check
I/O. IR3 additionally extends the static allocation-site descriptor as described
below; it does not add work to allocation-counter updates.

## IR3: site references and standalone report data

Both native backends use the originating program point as the site ID, qualified
by compilation unit. Duplicated allocations share that site and retain all its IR
spans. `location_kind` is zero for allocation specifiers, one for initiating
foreign calls, and two for generated allocations without an IR location.

Allocating foreign calls use the existing `REG_POLY_FUN_HDR` / `REG_POLY_CALL`
macros: the `Prof` variant receives a final `pPoint` argument and forwards it
unchanged through C helpers to `allocProfiling` or the allocation macros. This
argument is now the site metadata address shifted right by three, ready for the
packed descriptor, rather than a bare numerical program point. The backends
materialize it directly when placing C arguments, including stack arguments.
Calls without infinite result regions receive no extra argument. Automatically
converted C calls retain their C signatures; their ML result storage is allocated
by the compiler with its own descriptor. ML callbacks use their own sites, while
C allocations before and after callbacks retain the explicitly passed token.
There is no foreign-origin stack or enter/leave/unwind bookkeeping.

Each compiled unit has an IR identity shared by its `.o.ir` header and static site
descriptors. At link time, a small generated object records the actual absolute
paths of the linked ML objects, including installed library objects. Profile-site
definitions carry the matching object path. `rpview` opens `<ir_object>.ir`
directly, reads each needed file once, validates the complete content digest,
unit/compilation identity, table bounds and line/byte columns, and caches the
mark-to-span mapping. It does not infer object paths from source filenames.
The object itself need not be present when generating a report: matching profile
identity and validated companion contents suffice.

If files have moved after linking, `rpview --ir-dir DIR` optionally searches that
directory recursively for missing companions; the option is repeatable. These
fallbacks must pass the same identity checks. Normal use requires no search path.

HTML metadata includes `ir_documents` (the complete IR text and file-relative
spans) and `ir_sites` (one mapping per allocation definition). Status is
`available`, `generated`, `missing-location`, `missing-or-mismatched-ir`, or
`missing-mark`. Missing, malformed, truncated, changed, or wrong-build companions
leave object counts usable. The resulting HTML contains its IR and mappings and
needs no local files or network access. IR4 provides the site-selection interface described below. Allocation-range
filtering is unchanged.

Static site descriptors contain the unit, function, source, site ID, location kind
and IR identity. The linker supplies an object-path table. This metadata is read
when serializing site definitions, rather than on each allocation. The current
format is version 10 and site-metadata capability 4. Profiling cache directories
use the normal mode names, such as `RI_PROF` or `ARM64_RI_GC_PROF`,
without ABI-version suffixes.
Rebuild runtime and profiling objects together; older profiles must be regenerated.

## IR4: navigating allocation sites

The Allocation sites table shows allocation counts and bytes summed across
threads. Select a site button to open the IR
panel below the table. It shows the corresponding allocation specifier or
initiating foreign-call token, highlighted with eight surrounding lines on each
side. File line numbers are preserved, and hovering the source shows its full path.

If a site has multiple printed occurrences, use **Location** to choose one.
**Show full IR** displays the whole compilation unit while keeping the selected
location highlighted. The IR filename has a copy icon beside it; hover over the filename to see the
full companion-file path, or use the icon to copy that path for an editor such as
Emacs. The path refers to the file found when the report was generated.
The panel follows the **Show base names** setting and
preserves disambiguating function suffixes. **Close IR** returns keyboard focus
to the originating site button when it remains in the page.

Sites lacking navigable IR remain selectable: the panel explains whether the
profile is older, the allocation is generated, or the matching file/mark was
unavailable at report generation. No files are loaded by the browser. Byte offsets
are applied to UTF-8 data before decoding highlighted text, and code is inserted
as text rather than HTML. The exported report works offline with its embedded IR.

`test/region_profile/check-ir-viewer.sh` runs the actual report script in a small
DOM test harness using Node.js. It covers grouping, counts across threads,
occurrence selection, UTF-8, HTML-looking text, context/full-code views, unavailable
and malformed mappings, and focus handling. Native allocation regressions also
run these checks on inline and foreign-call reports on each CI backend.

## IR7: allocation-site view

The allocation section now has a single allocation-site table, ordered by
allocated bytes. Each row shows its allocating function, site, source, count,
and volume, aggregating threads without merging distinct units or sites.
Select a site to open its saved IR. Older profiles still show their allocation
volumes and explain when IR navigation is unavailable.

The Static call graph, Calls and closure creators, and separate Function views
have been retired. IR8–IR10 below add Region flow as the default, with Allocation sites as the only
alternative.

The compiler's call and closure-creator metadata remains in `.ir`
companions for the upcoming region-flow work. The reader continues to accept
versions 2 and 3. This UI change adds no runtime instrumentation and does not
change profile or companion formats.

## IR8–IR10: region flow

**Region slice** is the default allocation view when matching metadata is available.
**Allocation sites** is the only alternative. Older profiles or companions without
region-flow data display the site table with an explanation.

The compiler records local bindings and formal regions, per-call actual/formal
relationships and storage modes, and program-point destination regions during
closure conversion. Captured regions retain their lexical identity. Version 7
IR companions append a digest-covered `MLKIT-IR-REGIONS` table after the calls
table. A `region` row contains region ID, role, owner native label, and formal
position (empty for a local binding). A `flow` row contains caller, callee,
formal position, actual region ID, storage mode, program point (zero when
unavailable), and a per-compilation-unit call-occurrence number. All argument
rows from one call share this number. A `function` row records native label,
lexical parent, and whether the function is named or anonymous. A `point` row associates a positive program point with a region.
Fields use the existing SML string escaping. Positions are zero-based.

At launch, the profiler writes the linker-provided object manifest once. rpview
validates adjacent companions by identity and digest, including compilation units
with no measured allocation sites; `--ir-dir` remains a relocation fallback.
Imported formals resolve by native function label and parameter position. Local
region IDs are scoped by compilation unit; global regions share one namespace.
Missing or ambiguous connections remain explicit rather than being guessed.

Starting at the selected binding, the viewer traverses formal-to-actual edges
backwards and retains paths to measured allocation sites. The slice shows each
`fun` definition once, nested under its lexical parent, and each `LETREGION`
inside its owning function. Callees precede callers where possible; recursive
and shared calls link to definitions. Parameter lists retain positions with
ellipses for omitted regions; complete singleton lists need no ellipses.
Arguments are grouped only when they share a call occurrence. Older version 5
companions remain readable and show separate argument relationships.
Call annotations navigate to their
IR region arguments when a corresponding marked span exists. Allocations in
closure bodies are grouped under their creators, with the bodies and sites kept
visible. Several creators are listed, but each site contributes its totals once.
Unresolved or ambiguous site destinations remain visible separately.

Edges describe possible static region flow, not measured execution paths. Volume
is never divided among possible paths or counted again at shared references.
There is no extra work on the allocation hot path. The costs are compiler metadata,
a startup manifest record per linked IR artifact, and report generation/size.
The HTML remains standalone. Rebuild profiling objects and runtime together;
older companions trigger cache recompilation, while old saved profiles remain
readable. Snapshot-range allocation filtering is unchanged.

Saved IR uses an 80-column layout target and literal indentation spaces, including
deeply nested code. Version 7 invalidates older cached companions so byte spans
are regenerated for this layout; rpview still accepts earlier formats.
Non-allocating primitive calls use the ordinary expression printer, including
infix notation and precedence. Calls with result regions retain their marked
call tokens until the location protocol can identify them independently of
printed spelling.

## IR11: validating region slices and occupancy

Run `sh test/region_profile/check-ir11-examples.sh` with a matching compiler,
Basis and runtime to build fresh msort and kkb_eq reports without GC and an
mlyacc report with GC (processing `src/Parsing/Topdec.grm`). The script records
all infinite regions at 1ms intervals and retains its artifacts in the printed
temporary directory. Override `MLKIT`, `RPVIEW` and `SML_LIB` when validating a
separate backend build. Snapshot counts and sampled peaks depend on timing;
the check compares each report with its own recording, not a historical total.

To validate existing all-region recordings, run:

```sh
sh test/region_profile/check-ir11.sh msort.rp kkb_eq.rp mlyacc.rp
```

The checks reconcile per-instance objects, payload, descriptor overhead and
unused page space; compare the table and slice with the selected peak snapshot
for each binding, thread/worker scope and first/last/full range; and exercise
native site highlights and copyable IR paths. The HTML must expose only Region
slice and Allocation sites, with the slice selected by default and a clear
all-regions fallback. Its embedded script is tested in an environment without
filesystem or network APIs. msort's recursive result-region chain is derived
from the matching metadata rather than fixed region numbers.

The small all-region CI fixture runs the same checks without relying on a timer.
The existing graph/IR tests additionally cover captured regions, multiple
closure creators, recursion, lexical nesting, distinct call occurrences,
unresolved sites and metadata fallback. `test/prettyprint/check.sh` covers
cross-unit resolution (including manifest-only connecting units), relocation,
and missing/corrupt/mismatched companions; `check-site-svg.sh` checks exact SVG
site values and scope filters. This is validation of sampled occupancy, not
cumulative allocation counts or exact maximum residency.
