# ARM64 JumpTables integration

Milestone 12 now uses `JumpTables.binary_search_new`, the shared support used
by X64, instead of emitting an equality chain for every switch. Existing
BackendInfo thresholds select linear search below five cases, binary search
for sparse selections, and tables for groups of at least five nearby selectors
(adjacent distance at most ten). Table holes lead to the default branch.

The ARM64 callbacks compare the selector in x16 against constants in x17.
Once bounds are checked, dispatch subtracts the group start, loads a 64-bit
offset from an eight-byte-aligned table, adds its base, and branches through
x16. Entries are `.quad target - table`, avoiding absolute code pointers and
supporting separately loaded REPL images. No additional ML registers are used.

Selectors are ordered by their signed 64-bit machine representation before
calling JumpTables. This preserves matching for unsigned Word64 and tagged
Word63 values across both the sign bit and wraparound. Existing narrow-value
extension, numeric unboxing, constructor-tag extraction, flow switches, and
inline GC descriptors remain in use. Instruction scheduling is still deferred.

## Validation

MLKit `-gc` builds of the compiler, ReML, and emitter harness pass. The native
integration suite passes with the new switch fixture in no-GC, tagged, GC,
generational-GC, and profiling configurations, including forced collection.
Calls, exceptions, callbacks, long branches, and REPL recovery also pass.

`check-switches.sh`, now included in `make -f Makefile.arm64 check`, checks:

- Dense groups, holes, sparse search, small linear switches, and defaults
  below, within, and above selection ranges.
- Enumeration and payload-carrying constructor switches.
- Int31/32/64 and Word32/64 boundaries, tagged words, unsigned wraparound,
  and packed Int31 array loads, with and without GC.
- Table dispatches in a separately loaded REPL image, with and without GC.
- Actual indexed loads and relative table entries in emitted assembly.

## Small runtime comparison

Baseline is `0452e87` (inline GC descriptors, before JumpTables). Both compilers
use the same existing GC Basis cache; benchmark source units are freshly built
for each version. Thus this comparison measures the changed benchmark units,
not rebuilding the entire Basis with the new switch generator. Both compiler
hosts use MLKit `-gc` and the already documented 1 GiB host-stack workaround;
benchmark executables run natively on the same M2 Max machine.

Each version receives one warmup and three timed runs, alternating order. All
24 runs match the expected output. Times are median wall-clock seconds.

| Benchmark, with GC | Before | JumpTables | Time change | Table dispatches |
| --- | ---: | ---: | ---: | ---: |
| nucleic | 1.666 | 1.666 | +0.0% | 0 |
| mlyacc | 0.482 | 0.480 | -0.3% | 7 |
| professor | 0.462 | 0.460 | -0.6% | 17 |

These differences are too small to claim a runtime improvement from this
sample. The generated tables are present in mlyacc and professor; nucleic has
no eligible table groups. No broader performance analysis was performed.

[Raw samples, executable hashes, and compiler commands](arm64-performance/jump-table-results/measurements.json)
are retained with this report.
