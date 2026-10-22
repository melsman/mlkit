# ARM64 single-page reset coverage

Investigation confirms that eligible compiler-generated resets avoid C.
Sequential, non-profiling code already has a site fast path; parallel/profiling
code uses the new shared fast helper. Both check for no large objects and one
page per generation. An entirely full first page remains eligible.

Separate untimed diagnostic builds of the earlier ten sequential test programs
recorded 7,950,634 successful inline resets and 896 C fallbacks. Every observed
compiler-generated C fallback had additional pages; none was an eligible
single-page reset. The shared helper and legacy C-ABI reset wrapper were not
entered in these sequential runs. Outputs matched the original references.

Examples of corrected inline counts: Life 397,393; Knuth–Bendix 2,938,731;
FFT 1,048,064; ray tracing 3,145,900. A preliminary counter also matched allocation
accounting updates and was rejected. The retained instrumenter requires the
complete five-instruction descriptor/first-page/rewind signature, so those
allocation updates do not count as resets. These are not timing measurements.

A controlled 100,000-reset ReML program recorded:

| Mode | Inline successes | Shared helper successes | C fallbacks |
| --- | ---: | ---: | ---: |
| Sequential | 100,000 | 0 | 0 |
| Parallel | 0 | 100,000 | 0 |
| Profiling | 0 | 100,000 | 0 |

The instrumentation preserves x16/x17/x30 and flags and inserts no C calls at
counter sites. Generated references to C reset are redirected through a counting
wrapper that checks page chains and large-object eligibility. Runtime-internal
C calls (for example `maybeResetRegion` in string/IO operations) are outside
these generated-call counters and remain C operations on both architectures.
The legacy emitted C-ABI wrappers also still delegate to C, but the current
allocation/reset lowering does not use them in these audited programs.

A new native correctness guard compiles ten eligible resets in each of the
three modes, redirects generated C-reset targets to `abort`, and checks the
result. All three pass. This bounded guard joins the native suite; it is not a
performance test. The existing eight-configuration C/reference matrix covers
full pages, marks, large-object and multi-page fallback, and generational layouts.

The unchanged performance of ordinary sequential ARM64 programs is therefore
consistent with retaining an existing fast path, rather than missing single-page
coverage. No production compiler change was needed. Raw diagnostic results and
the strict counter implementation are in
[native-performance/reset-coverage-arm64](native-performance/reset-coverage-arm64/).
These diagnostics concern the earlier test-tree programs; the requested larger
`mlkit-bench` comparison is a separate assessment.
