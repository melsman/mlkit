# T5: offline sampled-time report

`rpview recording.rp -o report.html` embeds all code metadata, resolved sample
attribution and report logic in one standalone HTML file. No server or external
resources are required. Time-only recordings are supported.

The flat table includes horizontal bars sorted by sample count, function/source
labels, counts, percentages and estimated sampled wall time. GC, recorder drains,
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
lifetime ownership. Current recordings carry logical thread/stream 0:0.

The denominator is every recorded time sample in the chosen window and stream,
including unknowns. Estimates multiply sample count by the requested interval;
they are not measured CPU time, elapsed duration, or corrected for sampling loss.
The report describes the monotonic wall clock, blocking and excluded machine
sleep. Buffer and routing loss counters are session-wide because the input reader
retains only the latest cumulative status. Timer coalescing remains unobservable.
Incomplete recordings and missing final status are marked.

Validation: rebuild `rpview`, run `test/region_profile/check-viewer.sh`, and run
`test/time_profile/check-viewer.sh TIME_ONLY.rp COMBINED.rp`. The latter uses the
actual generated markup/script with the existing dependency-free DOM harness and
checks categories, percentage denominators, time filtering and empty intervals.

The Metric menu offers **Time profile** when a time session is present. Switching
between time and memory preserves the double-slider interval. Time-only reports
select it initially. Supporting clock/loss/attribution text lives in the time
heading's info popover. Function labels reuse allocation-view naming and Show
base names. Matching IR functions are clickable in the shared code viewer;
unavailable code remains a plain name with an explanation on hover. Supply
`--ir-dir DIR` to discover matching `.o.ir` companions for time-only profiles
without a linked-object manifest. Both IR identity and compilation unit must match.

Repeated printed function names can use the IR's named-function identity rows.
This fallback requires equal declaration counts and agreement of every printed
name with its corresponding compiler label; inconsistent or incomplete mappings
remain unavailable. Life's seven specialized `exists` functions are checked to
navigate to seven distinct declaration offsets. The example also searches Basis
IR companions, allowing the separate `List.exists` to link.
