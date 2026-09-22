# UF and DLX production fixes

This implements the [UF/DLX investigation](arm64-uf-dlx-investigation.md).
The ARM code generator now recognizes the audited `__mod_word63` and
`__mod_word31` runtime helpers as nonallocating calls without callbacks.
Already placed arguments go directly to the call; other argument layouts
retain the staging fallback. Arbitrary foreign calls and automatic foreign
conversions retain GC deferral. Modulo-by-zero still raises `Div`.

Unsigned tagged 63-bit arithmetic avoids redundant normalization: logical
right shift already normalizes its inputs, and retagging discards the high
result bit. Constants are materialized decoded. For operands that can be
decoded into x17 without clobbering x16, the temporary stack round trip is
removed. UF's multiplication sequence is now six instructions rather than
14, and its three modulo sites have bare calls.

The shared allocator now records threshold requests even when GC is
disabled. ARM safe points still check whether collection is enabled. The
collector itself also checks `disable_gc` before inspecting roots, since
X64 safe points only check `time_to_gc`. This preserves disabled collection
and shutdown behavior while retaining requests across foreign callbacks.

## Measured results

September 23, 2026, Apple M2 Max, 32 GiB RAM, macOS 26.5.1. Both compilers
use `-gc`; X64 runs through Rosetta 2. The new compiler is built using MLKit
`-gc`, with a 1 GiB host stack. No MLton is used. The X64 code generator is
unchanged. Both target GC runtimes and both complete Basis libraries are
rebuilt from the updated source; the Basis caches are initially absent
`UfDlxFixedarm64` and `UfDlxFixedx64` directories.

The previous ARM executables from the investigation are retained as the
before comparison. They contain the old runtime and use its previous freshly
rebuilt Basis. Each executable gets one warmup and five measured runs in
shuffled order; all builds finish before measurement. Every output matches
the expected output byte for byte. Values below are medians.

| Benchmark | Previous ARM (s) | Updated ARM (s) | X64 / Rosetta 2 (s) | Updated ARM / X64 |
|---|---:|---:|---:|---:|
| uf | 0.1788 | 0.1066 | 0.1399 | 0.762 |
| DLX | 0.2541 | 0.3632 | 0.3798 | 0.956 |

| Benchmark | Previous ARM RSS (MiB) | Updated ARM RSS (MiB) | X64 RSS (MiB) |
|---|---:|---:|---:|
| uf | 7.6 | 8.3 | 11.9 |
| DLX | 116.0 | 26.8 | 33.5 |

UF is 40.4% faster than previous ARM and 23.8% faster than X64. DLX uses
76.9% less peak RSS than previous ARM. Its time increases by 42.9%, because
it now performs 683 collections rather than eight; X64 also performs 683.
DLX GC time is 131.6 ms on ARM versus 168.9 ms on X64. Updated ARM remains
4.4% faster overall. UF performs seven collections on both updated targets.

Peak RSS is per-process `wait4`/`getrusage` memory in bytes, divided by
1,048,576, including resident code, stack, heap and process-associated
Rosetta memory. Baseline DLX RSS varies between runs and executable layouts;
the earlier report's 74.7 MiB is a different baseline. The reliable result
is that regular collection is restored and updated ARM stays near 27 MiB.

## Validation

- The native MLKit/ReML suite passes: calls, closures, regions, exceptions,
  floats, spilling, large frames, scalar C ABI, GC metadata, ordinary and
  generational GC, and profiling configurations.
- New arithmetic checks cover wrapping multiplication, decoded constants,
  normal modulo, both word-modulo exception paths, and allocation afterward.
- Allocation tests verify that page and large-object requests remain pending
  while disabled. A disabled call to the collector with a null root image
  returns without inspecting it or consuming the request.
- A nested C-to-ML-to-C-to-ML callback test verifies that a pending request
  survives both callbacks without collection, then collects after returning
  to ordinary ML execution. Both GC and generational GC pass.
- An X64 `-gc` executable running the arithmetic test with runtime
  `-disable_gc -report_gc` succeeds with zero collections.

[Raw samples, hashes and build/measurement scripts](arm64-performance/uf-dlx-fixes/)
are retained. The scripts contain this machine's temporary paths. The full
20-program suite was not rerun; measurements are limited to UF and DLX.
