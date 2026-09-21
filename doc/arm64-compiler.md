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
- `CodeGenArm64` supports direct/indirect ML calls and tail transfers,
  integer and FP argument banks, spilled arguments/results, closures,
  records, references, constructors, and exception handlers. Allocation uses
  the existing region runtime variants, preserving live ML registers across
  internal helper calls. ReML explicit regions use the same representation.
- Floating-point arithmetic and comparisons use ARM FP instructions;
  integer addition/subtraction check overflow and raise the ML exception.
  Large offsets are materialized through scratch registers, and large stack
  frames use aligned adjustments that fit the instruction immediates.
- ML calls use `bl`/`blr`, callee-owned FP/LR saves, and `ret`. Tail transfers
  preserve the original LR and result block while changing argument counts.
  The runtime's C `main` enters generated `code`, which installs the context
  and global regions and exits via `terminateML`. This entry does not return
  to C. Exported callbacks and shared-library entries use a separate bridge
  preserving x19–x30 and d8–d15.
- `ExecutionArm64` assembles with `gcc -arch arm64 -c` and links with
  `gcc -arch arm64`. The output cache is `MLB/ARM64_<variant>`, distinct from
  X64 while preserving the manager's two-component cache-directory shape.
- `nativearm64.mlb`, `mlkitarm64.mlb`, and `remlarm64.mlb` select the backend
  explicitly. The X64 configurations continue to select X64.

`ExecutionArm64` initially follows ExecutionX64's driver interface. Shared
command-line/driver code can be factored once both implementations stabilize.

## Limits and validation

Milestone 5 adds GC and generational GC, tagged and tag-free pairs, region
profiling, automatic FFI conversion, exported `int -> int` hooks, and shared
libraries/REPL loading. ReML retains its existing no-GC policy; plain and
profiling modes are supported. Tagged no-GC MLKit programs use a separate
`_TAG` cache variant. Generational GC with tagged pairs and tagged no-GC
profiling have no corresponding runtime archive and are rejected.

This remains an experimental language subset. Some Basis primitives and
colon-based dynamic foreign-symbol resolution remain unimplemented. Parallel
code generation, full Basis Library compilation, and native compiler bootstrap
remain later milestones. The REPL tests use `--no_basislib`; full Basis-based
pretty printing is not established by these tests.

The source automatic FFI retains MLKit's word-sized integer/boolean/pointer
interface. Both raw and automatic calls now spill arguments beyond x7. The
underlying scalar emitter also supports independent FP arguments, narrow
integers, packed stack arguments, and Darwin variadic promotion/placement;
these paths are tested through the emitter harness, without introducing new
source syntax. Exported hooks use ML integer representation, matching the
existing X64 interface, and preserve all Darwin callee-saved registers.

GC is deferred over foreign calls and exported hooks, since the existing C-call
IR has no root map. The previous `disable_gc` policy is restored on return,
leaving pending collection for the next ML entry. Callbacks must handle their
exceptions locally; unwinding across arbitrary foreign C frames is not
supported. Long-running callbacks can therefore retain allocations until they
return. Parallel use of the image registry is not yet supported.

The permanent native suite compiles and executes MLKit and ReML programs
covering cross-unit data, comparison branches, captured closures, indirect
calls, one million tail calls with stack arguments, mixed integer/FP calls,
NaN comparisons, references, lists, overflow, and nested exception handlers.
A C probe checks that unwinding restores the region chain. ReML additionally
runs an explicit-region program. Forty live arguments across a C call
exercise spilling; a generated finite 4,200-word record exercises a frame
larger than 32 KiB, large-offset loads/stores, and C-call SP alignment.

An MLKit-built emitter harness tests four through seven return values,
including odd/even result padding and tail calls that enlarge or shrink the argument
area. These use production CallConv and code emission directly, since
source-level tuple returns can remain boxed. The separate ABI suite checks
all combinations of zero through seven spilled arguments and results.

The suite checks exact output, ARM64 executable/object architecture, X64
cache isolation, and explicit parallelism rejection. It requires Apple Silicon.
`make -f Makefile.arm64 check` builds the compilers and emitter harness using
MLKit with `-gc`. To run the shell test directly, set `SML_LIB`, `MLKIT_ARM64`,
`REML_ARM64`, and `ARM64_EMITTER` to absolute paths.

The source-built compiler limitation in [#225](https://github.com/melsman/mlkit/issues/225)
also reproduces on a sample containing wrapper functions when the clean
pre-refactor compiler is built without GC. The GC-enabled host build handles
that sample. Minimal native execution still does not establish bootstrap
correctness or full native compiler coverage.

The X64 compiler built with MLKit also passes all 130 default `test_dev`
checks, retaining coverage for no-GC and generational-GC execution.

## Runtime integration checks

The native suite tests 40 live list roots, multiple bitmap words, spilled ML
pointer and FP arguments, captured handler roots, references updated across
collections, static data, and per-image global roots. MLKit runs plain,
tagged no-GC, profiling, GC, tagged-pair GC, and generational GC, including all
three GC/profiling combinations. Collection is forced at every ML function
entry; profiling samples are forced with `-notimer`. ReML runs the applicable
plain/profiling cases. A portable optimized C walker test checks relocated
register/stack/global slots and rejects missing return-PC metadata.

Foreign-call tests cover negative/tagged and boxed integers, boolean conversion,
more than eight arguments, captured exported closures, and assembly checks of
all Darwin callee-saved GPRs and low 64 bits of d8–d15. Generated scalar probes
call C with mixed integer/FP arguments, packed narrow stack fields, and
promoted variadic arguments. REPL checks load successive shared images, retain
and traverse list values across images, and continue after an uncaught
exception, including GC, tagged-pair GC, and generational GC.
