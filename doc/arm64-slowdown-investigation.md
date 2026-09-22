# ARM64 slowdown investigation

The static-pointer classifier in the ARM64 GC runtime explains the entire
nucleic slowdown, approximately half of the mlyacc gap, and a small part of
the professor gap in this experiment. The remaining mlyacc and professor
cost is mostly outside collection.

## Isolated runtime experiment

`GC.h` implements `points_into_dataspace` with two address comparisons on X64.
On native Darwin it calls `mlkit_arm64_static_pointer` in `Arm64GC.c`, which
linearly searches the registered static images. GC evacuation and forwarding
invoke this frequently. A heap pointer outside every image still traverses
the entire list. This registry classifies static data and visits global roots;
stack livesets continue to use descriptors immediately before return PCs.

The experimental change caches the minimum beginning and maximum end of all
registered images. A pointer outside that envelope returns false immediately.
Pointers inside it still undergo the original exact search, preserving holes
between images. Registration and unregistration recompute the bounds.

The experiment relinks the same ARM64 ML objects with this runtime change.
It does not modify the production runtime. Median wall-clock seconds:

| Benchmark, GC enabled | X64 / Rosetta 2 | ARM64 current | ARM64 experimental bounds check |
| --- | ---: | ---: | ---: |
| nucleic | 0.951 | 1.437 | 0.822 |
| mlyacc | 0.195 | 0.382 | 0.288 |
| professor | 0.240 | 0.377 | 0.359 |

Median collection CPU time, seconds, from `-report_gc`:

| Benchmark | X64 | ARM64 current | ARM64 experimental | Collections in every run |
| --- | ---: | ---: | ---: | ---: |
| nucleic | 0.1952 | 0.8024 | 0.1928 | 3106 |
| mlyacc | 0.0084 | 0.0940 | 0.0062 | 28 |
| professor | 0.0057 | 0.0226 | 0.0045 | 263 |

Separate, untimed instrumented executions of the original classifier explain
the scale of the unnecessary work:

| Benchmark | Classifier calls | Image entries examined | Static hits | Images registered |
| --- | ---: | ---: | ---: | ---: |
| nucleic | 30,857,737 | 837,178,726 | 13,444,636 | 45 |
| mlyacc | 709,177 | 81,180,045 | 10,958 | 116 |
| professor | 548,132 | 21,412,933 | 54,757 | 43 |

The bounds check removes about 0.615 seconds from nucleic, making it about
14% faster than X64 here. It removes about 0.094 seconds from mlyacc and
0.018 seconds from professor. Collection counts and expected output remain
identical; the difference is collection cost rather than collection frequency.

## Remaining code-generation opportunities

After the runtime experiment, median CPU time outside collection is 0.627
versus 0.751 seconds for nucleic (ARM64 versus X64), 0.278 versus 0.182 for
mlyacc, and 0.352 versus 0.230 for professor. These differences warrant
separate generated-code experiments.

Three short native sampling runs per benchmark identify mlyacc's recursive
set `union` and professor's `findSol` search loop as useful targets. Inspection
of their generated assembly and both generators finds:

1. **GC polling in non-allocating functions.** X64 uses `LS.allocating lss`
   (unless extra checks are requested) to decide whether to emit an entry
   check. ARM64 emits a check for every GC-enabled function. In mlyacc, small
   comparator wrappers called from `union` consequently load and test GC
   state and set up a frame before tail branching. The corresponding X64
   wrappers have no GC check. Reuse the shared allocating analysis, preserving
   forced-check behavior, and then assess unnecessary wrapper frame work.
2. **List-constructor tests.** X64 specializes `NIL`/`CONS` tests to extracting
   the low tag bits. ARM64's `SWITCH_C` uses the general unboxed-constructor
   sequence, including an extra compare and conditional select before the
   branch comparison. This occurs repeatedly in both hot functions. Port the
   existing X64 specialization and consider direct bit-test branches where
   the representation permits them.
3. **Spills and region-helper preservation.** The hot functions retain
   substantial stack traffic. Region helpers can additionally save the broad
   register set, including FP registers, even where allocation has already
   forced values to spill. Review preservation at these specific call sites
   and field-selection scratch-register moves after the simpler changes.

These are observed instruction differences, not independently measured shares
of the residual slowdown. Rosetta samples were unsymbolicated, so the sampling
does not establish comparable per-function percentages. No instruction
scheduling experiment was performed.

## Method and scope

Same M2 Max machine; one warmup and three measured runs per configuration,
alternating configuration order. All 36 executions used `-report_gc` and
matched expected output, as did the three untimed counter executions. The
existing ARM64 metadata/root-relocation test also passed against the modified
runtime. This is a focused diagnostic experiment, not comprehensive runtime
validation.

The ARM64 benchmark units use `91b950f`; the unchanged GC Basis cache comes
from the earlier inline-descriptor build (`0452e87`). Both ARM64 variants use
exactly the same ML objects. X64 uses the milestone 11 binaries; its backend
has not changed. These are fresh side-by-side measurements, not the earlier
separately measured wall times. They do not cover a freshly rebuilt Basis,
non-GC configurations, or other workloads. CPU time is user plus system time;
outside-GC time subtracts the runtime's reported collection CPU time. Each
column is independently medianed.

[Raw measurements](arm64-performance/slowdown-investigation/comparison.json)
include individual samples, counters, executable hashes and output hashes.
The [experimental patch](arm64-performance/slowdown-investigation/static-range-reject.patch)
is preserved without applying it to the runtime. The captured
[preparation script](arm64-performance/slowdown-investigation/prepare.py) and
[comparison script](arm64-performance/slowdown-investigation/compare.py)
record the commands and local cache paths used; those paths must be adapted
for another checkout.

The first production change to pursue is the bounds rejection, retaining exact
membership checks and validating image registration/unregistration. Next,
measure allocation-aware GC polling and specialized list tests independently
on mlyacc and professor before attributing the remaining gap to register
allocation or the internal call ABI.
