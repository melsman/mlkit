# T5: offline sampled-time report

[T6a](t6a-ordinary-builds.md) adds ordinary-build reports, named runtime/C
self-time and shared Allocation/Reset categories alongside GC.

`pview recording.rp -o report.html` embeds all code metadata, resolved sample
attribution and report logic in one standalone HTML file. No server or external
resources are required. Time-only recordings are supported.

The flat table includes horizontal bars sorted by sample count, function/source
labels, counts, percentages and estimated wall time. GC, recorded recorder states,
unresolved PCs and unavailable metadata remain separate visible categories.
Resolution reuses the exact IntInf PC resolver: GC/recorder states take precedence,
then recognized interrupted ML code, then the initiating ML call PC for a foreign
extent. Functions reached through callbacks therefore receive their own samples.
Build mismatches reported by the resolver remain visible rather than being omitted.

With time sampling enabled, the double slider selects elapsed time in 10,000
steps across the recorded session. Comparisons use exact BigInt timestamps;
region snapshots and time samples are filtered independently, with inclusive
boundaries. Region-only reports retain their snapshot selection controls.
The execution-stream selector filters the time denominator independently of region
lifetime ownership. Single-thread recordings use thread/stream 0:0; [T7](t7-parallel-sampling.md)
adds real pthread lifetime and execution-stream identities.

The denominator is every recorded time sample in the chosen window and stream,
including unknowns. Estimates multiply sample count by the requested interval;
they are not measured CPU time, elapsed duration, or corrected for sampling loss.
The report describes the monotonic wall clock, blocking and excluded machine
sleep. Buffer and routing loss counters are session-wide because the input reader
retains only the latest cumulative status. Timer coalescing remains unobservable.
Incomplete recordings and missing final status are marked.

Validation: rebuild `pview`, run `test/region_profile/check-viewer.sh`, and run
`test/time_profile/check-viewer.sh TIME_ONLY.rp COMBINED.rp`. The latter uses the
actual generated markup/script with the existing dependency-free DOM harness and
checks categories, percentage denominators, time filtering and empty intervals.

The Metric menu offers **Time profile** when a time session is present. Switching
between time and memory preserves the double-slider interval. Time-only reports
select it initially. Supporting clock/loss/attribution text lives in the time
heading's info popover. Function labels reuse allocation-view naming and Show
base names (including function names). Matching IR functions are clickable in
the shared code viewer and retain detailed function/source/unit hover information;
unavailable code remains a plain name with an explanation on hover. For ordinary
builds without a linked-object manifest, matching `.o.ir` files are automatically
searched for in `MLB` caches beside recorded sources and in enclosing directories.
`--ir-dir DIR` adds fallback locations for moved files. Both IR identity and
compilation unit must match. Region-only choices and controls are omitted when
there are no region snapshots.

Repeated printed function names can use the IR's named-function identity rows.
The fallback reconstructs lexical order from function-parent rows, then requires
equal counts for the complete repeated-name group. Optimized-away declarations
with other names do not block navigation; incomplete or inconsistent groups
remain unavailable. Ordinary builds preserve these rows without `-rp`.
Life's seven specialized `exists` functions are checked to
navigate to seven distinct declaration offsets. The example also searches Basis
IR companions, allowing the separate `List.exists` to link.

The contributions table uses compact rows, sticky column headings and a scroll
area capped at 480 pixels or 65% of the viewport height, whichever is smaller.
In time view the shared double slider sits directly below the table and above
the code viewer; in region view it sits beneath the graph. It is moved only
when switching views, so native handle dragging is not interrupted by redraws.

Passive GC samples store the `gc` address whenever the atomic `doing_gc` flag
is set. This includes GC helpers and GC calls to shared allocation functions.
Detailed samples retain actual PCs and GC state. Ordinary allocation samples
remain separate; the table reports sampled contributions, not call counts.
