# Static-data bounds, GC polling and list tests

This implements the follow-up to the
[slowdown investigation](arm64-slowdown-investigation.md).

- **Executable static data:** after registering the initial executable's units
  and link data, the generated entry code calls `mlkit_arm64_seal_main_image`.
  The runtime freezes their enclosing address range. GC classifies pointers
  inside it with inline bounds comparisons; outside pointers also return
  immediately when no dynamic images have been registered. Root cells remain
  registered and are still visited. ReML registrations after sealing keep
  exact, separate ranges and cannot enlarge the executable's envelope.
- **Allocation-aware polling:** function entries use `LS.allocating`, as X64
  does, to omit GC checks from non-allocating functions. Allocating callees
  retain their own checks. `-extra_gc_checks` overrides the omission; callback
  GC deferral and the frame-descriptor ABI are unchanged.
- **List tests:** unboxed `NIL`/`CONS` switches extract the low two bits directly,
  avoiding the general constructor compare and conditional select. Other
  constructors retain the existing sequence, including full selectors for
  datatypes with multiple nullary constructors.

Only registrations belonging to one linked executable may be sealed together.
The linker places these ML objects in its data segment; this does not assume
that separately loaded images are contiguous. A second seal is rejected.
Unregistration updates the executable bounds or removes the exact dynamic
range, as applicable. Rebuild the ARM runtime with this compiler: generated
link code now references the new sealing entry point. Existing compiled ML
units and inline GC descriptors remain compatible.

## Focused validation

The compiler, ReML and emitter harness build with MLKit `-gc`. All ARM runtime
variants build. The native MLKit/ReML suite passes, including allocation,
generational GC, forced collection, profiling, foreign callbacks, constructor
switches, and REPL exception recovery. Additional focused checks verify:

- Non-allocating list recursion omits polling, allocating list construction
  still polls, and forced checks remain in the non-allocating function.
- The list switch has the simplified mask and no general conditional select.
- Multiple nullary constructors retain distinct results, with GC, tagged
  pairs, generational GC and without GC.
- Static membership before/after sealing, the executable envelope, rejection
  of gaps before dynamic images, exclusive upper bounds, root relocation from
  both lists, and correct removal of main and dynamic registrations.

## Three-benchmark comparison

Fresh side-by-side runs on the same M2 Max, with one warmup and three measured
runs per configuration, alternating order. All 36 executions matched expected
output. Every execution used `-report_gc`; no compilation or integration tests
ran concurrently with measurement. Values are median wall-clock seconds.

| Benchmark | X64 / Rosetta 2 | ARM64 before | ARM64 after | ARM64 change |
| --- | ---: | ---: | ---: | ---: |
| nucleic | 1.003 | 1.488 | 0.814 | -45.3% |
| mlyacc | 0.206 | 0.402 | 0.312 | -22.4% |
| professor | 0.255 | 0.404 | 0.381 | -5.7% |

The resulting ARM64/X64 ratios are 0.81, 1.51 and 1.50 respectively. These
are combined effects, not isolated timings of each compiler optimisation.

| Benchmark | GC CPU before (s) | GC CPU after (s) | Collections, both versions |
| --- | ---: | ---: | ---: |
| nucleic | 0.8273 | 0.1570 | 3106 |
| mlyacc | 0.0993 | 0.0061 | 28 |
| professor | 0.0231 | 0.0048 | 263 |

Most of the measured improvement comes from static-pointer classification.
CPU time outside collection is approximately unchanged for nucleic (0.650
seconds in both), slightly higher for mlyacc (0.296 to 0.301), and slightly
lower for professor (0.374 to 0.369). This small comparison does not establish
a separate runtime benefit from polling and list-test simplification despite
their verified reduction in generated instructions.

The baseline is `95e7c2e` (code unchanged from `91b950f`); X64 uses the existing
milestone 11 executables. ARM64 benchmark units were freshly compiled for the
candidate, while both versions reuse the unchanged `M12Inline` GC Basis from
the earlier inline-descriptor build. This is not a full Basis rebuild or a
broad performance survey. Medians in different columns are computed
independently.

[Raw samples and executable/output hashes](arm64-performance/static-data-results/measurements.json)
and the [measurement script](arm64-performance/static-data-results/measure.py)
are retained. The script records the local benchmark paths used for this run.
