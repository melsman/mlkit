# ARM64: simple, mpuz and uf improvements

This implements the compiler changes motivated by the
[simple/mpuz investigation](arm64-simple-mpuz-investigation.md) and
[no-GC uf investigation](arm64-uf-nogc-investigation.md).
The measurements compare the previous compiler (code at `052ee41`) with this
implementation and X64 under Rosetta 2.

## Compiler changes

- Recognize `__mod_word64ub` and `__mod_word32ub` as non-allocating runtime
  helpers, alongside the tagged-word variants. Already-placed arguments use a
  direct call. Other argument layouts retain scalar marshalling. Division by
  zero still raises `Div`; arbitrary foreign calls retain GC protection.
- Avoid pairing loads when the second destination overwrites their base
  register. This preserves separate loads in simple's list traversal, while
  keeping mpuz's independent tuple-field loads paired.
- Reuse frames for no-GC self-recursion with spill slots and nested calls.
  Incoming stack arguments, addressable local regions, handlers, profiling and
  forced polling retain the existing path. GC retains its non-allocating,
  zero-local-frame restriction. Add untagged integer comparisons to the
  existing inline-primitive whitelist.
- Delay private spill-slot assignments in eligible no-GC loops. Register copies
  and record-field projections can be used directly until a source register
  changes or an operation requires a committed stack value. Small branches can
  receive a short copy of their continuation, avoiding stores on paths without
  calls. Duplication is bounded to 32 branches and four continuation statements;
  at most eight assignments are pending. Calls, mutations, allocation and
  unsupported operations commit needed values. In mpuz this moves the closure
  and list-tail spills off the skip branch. The reference-pointer spill remains
  because its source register is overwritten.
- Eliminate temporary stack staging for numeric operands that can be decoded
  into x17 without clobbering x16, and for reference/array updates whose value
  can be read without clobbering the address. Reserved-register and large-offset
  fallbacks remain. Signed multiplication retains its overflow checks.

## Measurement method

Apple M2 Max, macOS 26.5.1. Each compiler and runtime mode has a separate, newly
compiled Basis cache. All host compilers were built using MLKit with `-gc`;
MLton was not used. Runtime measurements explicitly select `-no_gc` or `-gc`.
The benchmark sources and workloads are unchanged from the preceding reports.

One warm-up and seven measured runs per executable, with deterministic shuffled
ordering of old ARM64, new ARM64 and X64. All stdout is compared byte-for-byte
with the benchmark's expected output. Timed runs start after builds and tests
finish. Wall time includes process startup; peak RSS comes from `wait4` and
includes runtime/native-code/Rosetta overhead, not just the ML heap.

[Build script](arm64-performance/simple-mpuz-uf-improvements/build.py),
[measurement script](arm64-performance/simple-mpuz-uf-improvements/measure.py)
and raw measurements accompany this report.

## Execution time

Seconds; medians of seven runs. Reduction is relative to the old ARM64 compiler.

### `-no_gc`

| Benchmark | Old ARM64 | New ARM64 | Reduction | X64/Rosetta 2 | New ARM / X64 |
|---|---:|---:|---:|---:|---:|
| simple | 0.5579 | 0.4610 | 17.4% | 0.4714 | 0.978× |
| mpuz | 0.3871 | 0.3078 | 20.5% | 0.2782 | 1.106× |
| uf | 0.1574 | 0.0848 | 46.1% | 0.1126 | 0.753× |

### `-gc`

| Benchmark | Old ARM64 | New ARM64 | Reduction | X64/Rosetta 2 | New ARM / X64 |
|---|---:|---:|---:|---:|---:|
| simple | 0.5513 | 0.4699 | 14.8% | 0.6216 | 0.756× |
| mpuz | 0.4248 | 0.4113 | 3.2% | 0.4244 | 0.969× |
| uf | 0.1099 | 0.1076 | 2.1% | 0.1427 | 0.754× |

With `-no_gc`, simple is 2.2% faster than X64 and uf is 24.7% faster; mpuz
remains 10.6% slower. The GC results improve too, but the 2–3% changes for
mpuz and uf are small and should not be treated as precise estimates from one
short measurement session. The larger no-GC gains match the generated-code
changes identified in the investigations.

## Peak resident memory

MiB, median process peak RSS. Small old/new differences (at most 80 KiB here)
are not evidence of a change in ML heap requirements.

### `-no_gc`

| Benchmark | Old ARM64 | New ARM64 | X64/Rosetta 2 |
|---|---:|---:|---:|
| simple | 4.562 | 4.609 | 6.082 |
| mpuz | 2.719 | 2.734 | 3.988 |
| uf | 6.672 | 6.625 | 10.188 |

### `-gc`

| Benchmark | Old ARM64 | New ARM64 | X64/Rosetta 2 |
|---|---:|---:|---:|
| simple | 5.625 | 5.688 | 7.055 |
| mpuz | 2.922 | 2.922 | 4.098 |
| uf | 8.328 | 8.406 | 11.926 |

[Raw samples and executable hashes](arm64-performance/simple-mpuz-uf-improvements/measurements.json)
and [compiler metadata](arm64-performance/simple-mpuz-uf-improvements/metadata.json).

## Validation

The native MLKit/ReML suite covers calls, closures, regions, exceptions, floating
point, spills, large frames, GC/generational GC, profiling, callbacks and inline
GC descriptors. New tests cover spill-heavy loops with nested calls and
reference updates, exception exits, signed multiplication overflow, and 32-bit
word modulo including `Div`. Emitter assertions check local-frame reuse, its GC
fallback, and the pointer-chasing load-pairing boundary.
