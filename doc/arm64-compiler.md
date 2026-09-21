# Experimental Darwin ARM64 compiler

Milestone 3 of #223 introduces ARM64-emitting MLKit and ReML executables.
They run on the architecture of the MLKit used to build them; this is not a
native bootstrap. Use MLKit for all compiler builds and checks.

## Build and run

Start with a configured checkout and an installed working MLKit. Build both
runtime targets as described in [arm64-runtime.md](arm64-runtime.md). Keep
the X64 compatibility archives when using an X64 bootstrap compiler. ARM
programs link only `lib/darwin-arm64/runtimeSystem.a`.

```sh
make -f Makefile.arm64 MLKIT_BOOTSTRAP=/usr/local/bin/mlkit
make -f Makefile.arm64 check MLKIT_BOOTSTRAP=/usr/local/bin/mlkit
```

`make arm64_compilers` is also available after regenerating the configured
Makefile. `MLKIT_BOOTSTRAP_FLAGS` defaults to `-gc` and can be overridden. The
GC-enabled host build avoids a crash observed with the no-GC host build
on wrapper-function samples; generated ARM programs still run without GC.
These explicit targets do not replace the existing X64 compiler executables
or enable a full ARM install/bootstrap.

The installed bootstrap compiler's linker does not reliably quote paths
containing spaces. If needed, create a stable, space-free symlink to this
checkout and set `SML_LIB` to that absolute path for the commands above.
Keep that alias stable to retain incremental compilation caches.

For example, compile the included two-unit smoke program with:

```sh
SML_LIB=/absolute/path/to/mlkit bin/mlkit-arm64 --no_basislib \
  -o /tmp/native-arm64 src/Compiler/Backend/Arm64/tests/native.mlb
printf '@' | /tmp/native-arm64
# A
```

The same command works with `bin/reml-arm64`. Both start in untagged,
non-parallel, no-GC mode. ReML's existing blocked-option policy means that
an explicit `-no_gc` flag is unnecessary and rejected; omit it.

## Current implementation

- `InstsArm64` implements the ARM register interface and an initial assembly
  representation/emitter. x18, FP, LR, context, exception, and scratch registers
  are excluded from integer allocation.
- `CodeGenUtilArm64` materializes 64-bit constants, performs aligned stack
  adjustments, and emits Mach-O GOT-based symbol addresses.
- `CodeGenArm64` handles unboxed 64-bit word arithmetic, selected comparisons,
  stack locals, global loads/stores, and raw fixed integer-register C calls.
  It includes register-only direct ML calls/tail transfers and integer/word
  flow comparisons. Broader call and
  exception coverage belongs to milestone 4.
- ML calls use `bl`, callee-owned FP/LR saves, and `ret`. The C-facing main
  stub preserves x19-x29 and d8-d15. Return-address header space remains
  reserved according to the ABI design.
- `ExecutionArm64` assembles with `gcc -arch arm64 -c` and links with
  `gcc -arch arm64`. The output cache is `MLB/ARM64_<variant>`, distinct from
  X64 while preserving the manager's two-component cache-directory shape.
- `nativearm64.mlb`, `mlkitarm64.mlb`, and `remlarm64.mlb` select the backend
  explicitly. The X64 configurations continue to select X64.

`ExecutionArm64` initially follows ExecutionX64's driver interface. Shared
command-line/driver code can be factored once both implementations stabilize.

## Limits and validation

This is a minimal backend, not full Standard ML/ReML execution support.
Unsupported instructions and modes fail explicitly. In particular, region
allocation, closures/indirect ML calls, stack-passed call arguments/results,
FP operations, signed arithmetic with overflow exceptions, exceptions,
profiling, GC, parallelism, automatic FFI conversions, callbacks, shared
libraries, and REPL loading are not enabled. Full Basis Library compilation
and native compiler bootstrap remain later milestones.

The native test compiles two units with MLKit and ReML, passes runtime input
through C `getchar`, computes `(input + 3) * 2 - 69`, and calls C `putchar`.
Additional cases exercise both comparison branches, non-inlined nested ML
calls and a tail transfer, checking that real call/branch instructions were
emitted. Tests check exact output, cross-unit data addressing, executable/object
architecture, X64 cache isolation, and explicit GC rejection. It requires
Apple Silicon for execution. Run it directly with `SML_LIB`, `MLKIT_ARM64`,
and `REML_ARM64` set to absolute paths.

The source-built compiler limitation in [#225](https://github.com/melsman/mlkit/issues/225)
also reproduces on a sample containing wrapper functions when the clean
pre-refactor compiler is built without GC. The GC-enabled host build handles
that sample. Minimal native execution still does not establish bootstrap
correctness or support for profiling in the ARM backend.

The X64 compiler built with MLKit also passes all 130 default `test_dev`
checks, retaining coverage for no-GC and generational-GC execution.
