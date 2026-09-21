# ARM64 allocation and reset paths

The ARM64 emitter now follows the X64 strategy of short allocation sites,
inline page-space checks, and shared slow paths. This builds on the complete
code-suffix conversion at `06477d9`.

## Generated paths

Known finite regions return their storage address directly. Unknown region
kinds test the infinite-region bit. Ordinary infinite-region allocations compare
`a - 1 + bytes` with `(a - 1) | (page_size - 1)`, matching the runtime's page
boundary calculation even when `a` is exactly at the end of a page. A successful
allocation advances `g0.a` and returns the original pointer. GC allocations also
update `alloc_period`, exactly once, as the C allocator does. Tag-free values
retain the existing one-word pointer adjustment.

Page expansion and large objects use shared preserving stubs. Allocation sites
pass the region in `x16`, the word count in `x17`, and receive the result in `x16`.
The stubs save the same 24 integer and 30 floating-point registers (including
the FP spill temporaries), plus LR, in a 448-byte aligned frame in shared code,
then call the existing runtime helper. The register-save sequences are no longer
repeated at every object allocation or reset site. `x16`, `x17`, and `x30` remain
scratch registers; the ML function's incoming LR is already saved in its frame.
The general ML/C ABI and register allocation policy are unchanged.

Reset sites honor finite/infinite and at-top/at-bottom modes. When each generation
has a single page and the region has no large objects, the inline path restores
its allocation pointer to the first payload word. Generational GC also restores
each page's color pointer and preserves the flags in its first-page pointer.
All preconditions are checked before changing either generation. Other cases use
the shared reset stub and `resetRegion` for page recycling and large-object cleanup.

Profiling continues through runtime allocation/reset routines so their counters
and object descriptors remain correct. Profiling program points use an aligned
caller stack slot. Protected parallel allocation retains the runtime's locking
and atomic operations; explicitly unprotected parallel allocation can use the
page fast path. Parallel resets retain the runtime path.

The original C-ABI `mlkit_arm64_alloc` and `mlkit_arm64_reset` helpers remain
available for previously cached objects. New preserving stubs are emitted in the
program/REPL initialization code; incremental REPL images reference them there.

## Nucleic comparison

Both versions compile `test/nucleic.mlb` with `-gc`, the same warmed Basis cache,
and cleared test-local `MLB` directories, including `nucleic/MLB`. The baseline
is the full suffix-passing compiler at `06477d9`. All host compiler builds use
MLKit with `-gc`. No builds or correctness suites ran during the final comparison.

| Metric | Before | New allocation/reset paths |
| --- | ---: | ---: |
| CG user CPU time | 0.537 s | 0.466 s |
| Emitted instructions in the two nucleic units | 243,120 | 102,765 |
| Executable user CPU time, median of seven runs | 0.029097 s | 0.014671 s |

CG time falls **13.2%**, the static instruction count falls **57.7%**, and
executable user CPU time falls **49.6%** (about **2× faster** in this small test).
The latter runs alternate which executable runs first; all fourteen outputs
match the expected result. This is a short microbenchmark, not a broader runtime
performance claim. Both executables use the same previously compiled Basis.

CG is a single compilation per version, summed over `nucleic.sml` and `main.sml`;
it includes compiler GC and excludes assembly/linking and execution. Instruction
counts include each unit's out-of-line slow blocks and exclude the separate link
unit and cached Basis. They count emitted instructions, not dynamic execution.
Runtime measurements use the child process's user CPU time, including program GC.

Raw data: [old CG timings](arm64-performance/nucleic-allocation/old.KITtimings),
[new CG timings](arm64-performance/nucleic-allocation/new.KITtimings), and
[all execution samples and counts](arm64-performance/nucleic-allocation/measurements.json).

The compilation command, from a temporary copy of `test`, was:

```sh
SML_LIB=/private/tmp/mlkit-m2-source "$compiler" \
  --mlb-subdir M8Timingbase --no_delete_target_files \
  --timings --log_to_file -gc nucleic.mlb
```

## Validation

- **130/130 developer tests**, **181/181 GC tests**, and **79/79 explicit-region
  tests** pass, with fresh regression caches.
- The native MLKit/ReML suite passes, including forced GC, generational GC,
  tag-free/tagged pairs, profiling, callbacks, large finite records, exceptions,
  floating-point spills, and REPL collection/recovery.
- Pthread and Argobots validation passes, including protected and explicitly
  unprotected allocation, private regions, shared regions, and callbacks.
- Direct emitter probes pass in plain, GC, and generational-GC modes. They cover
  exact page fills, expansion, large objects, dynamic finite regions, at-bottom
  allocation, tag-free values, and single-page, multi-page, and large-object
  resets. Integer and floating-point sentinels verify register preservation.
- C layout assertions compile in plain, GC, generational-GC, profiling, and
  parallel configurations. They check the page size, payload, next-page, and
  color-pointer offsets used by the inline paths.

The broad regression invocation uses `check-regressions.sh` with
`REGRESSION_SUITES='dev gc explicit'`; mode-specific GC/profiling/parallel coverage
comes from `check-native.sh` and `check-parallel.sh` (with Argobots enabled).
