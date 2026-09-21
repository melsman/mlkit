# Experimental Darwin ARM64 compiler

The ARM64 backend for #223 supports native MLKit and ReML, the Basis Library,
REPL, tools, and a separate staged installation on Apple Silicon. Native
MLKit reaches a bootstrap fixed point. Use MLKit with `-gc` for compiler builds
and checks.

## Build and run

Start with a configured checkout and an installed working MLKit. Build both
runtime targets as described in [arm64-runtime.md](arm64-runtime.md). Keep
the X64 compatibility archives when using an X64 bootstrap compiler. ARM
programs link the selected variant under `lib/darwin-arm64/`.

```sh
make -f Makefile.arm64 MLKIT_BOOTSTRAP=/usr/local/bin/mlkit
make -f Makefile.arm64 check MLKIT_BOOTSTRAP=/usr/local/bin/mlkit
```

`make arm64_compilers` is also available after regenerating the configured
Makefile. `MLKIT_BOOTSTRAP_FLAGS` defaults to `-gc` and can be overridden. The
GC-enabled host build avoids a crash observed with the no-GC host build
on wrapper-function samples; generated ARM programs can independently select their supported GC mode.
These initial targets produce ARM-emitting compilers on the host compiler's
architecture. The native build, installation, and bootstrap targets below
produce separate ARM64 executables.

The existing driver writes unquoted Basis paths for direct `.sml` inputs,
REPL startup, and dependency processing. Use a stable, space-free symlink to
the checkout or installed prefix for `SML_LIB` and `ARM64_PREFIX`. The older X64
bootstrap compiler also has linker quoting limitations. Keep the alias stable
to retain incremental compilation caches. Rerun `native-install` when changing
the prefix: it invalidates Basis caches recorded at a different location.

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

The full Basis Library and Basis-based REPL tests pass. Colon-based dynamic
foreign-symbol resolution remains unimplemented. The backend remains
experimental; the supported runtime combinations and foreign-call boundaries
below still apply.

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
cache isolation, and rejection of unsupported parallel GC combinations. It requires Apple Silicon.
`make -f Makefile.arm64 check` builds the compilers and emitter harness using
MLKit with `-gc`. To run the shell test directly, set `SML_LIB`, `MLKIT_ARM64`,
`REML_ARM64`, and `ARM64_EMITTER` to absolute paths.

The source-built compiler limitation in [#225](https://github.com/melsman/mlkit/issues/225)
also reproduces on a sample containing wrapper functions when the clean
pre-refactor compiler is built without GC. The GC-enabled host build handles
that sample. Use the separate bootstrap and regression checks below to validate
native compiler coverage beyond the minimal execution tests.

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

## Parallel compilation

Milestone 6 adds native pthread and optional Argobots execution for MLKit and
ReML (`-par`, or `-par -argo`). These use the existing no-GC, untagged,
non-profiling runtime configurations. `-argo` and `-par0` require `-par`.
`-par0` disables allocation protection and is only appropriate when concurrent
allocations cannot target the same region. See [arm64-runtime.md](arm64-runtime.md)
for the synchronization design, Argobots setup, and validation coverage.

`make -f Makefile.arm64 check` also runs `tests/check-parallel.sh`; set
`ARGOBOTS_ROOT` to include its optional Argobots cases. The fixtures include
the production `THREAD.sig`/`Thread.sml` wrapper with a minimal prelude, so these
checks do not depend on full Basis compilation.

## Native compilers, tools, and installation

The full Basis and native compiler builds use the same ARM backend. Build the
native MLKit, ReML, and their tools into a separate directory:

```sh
make -f Makefile.arm64 native native-tools
make -f Makefile.arm64 regressions
make -f Makefile.arm64 bootstrap
make -f Makefile.arm64 native-install
```

These targets default to `bin/mlkit-arm64` as the ARM-emitting compiler; set
`ARM64_COMPILER` to another ARM-emitting or native MLKit if needed. Compiler
and SML tool builds explicitly use `-gc`. `ARM64_NATIVE_BIN` defaults to
`bin/darwin-arm64`, and `ARM64_PREFIX` to `stage/darwin-arm64`. The installation
copies only verified ARM binaries and runtime archives and rebuilds Basis
caches using the installed compiler. Set `SML_LIB` to the installed prefix when
using its compiler. X64 binaries, runtime archives, and caches remain separate.
The native tools are `kittester`, `rp2ps`, `mlkit-mllex`, and `mlkit-mlyacc`.
Installation checks generate, compile, and run a calculator parser with the
installed generators and parser library. `install_src` also packages the
backend, generator sources, and regression fixtures.

The regression target runs fresh copies of `test_dev`, the full `test` matrix
(no-GC, GC, generational GC, no-GC/GC profiling, and pthreads), explicit-region
tests, parallel tests, and the full-Basis REPL suite. It retains logs and
outputs in a temporary directory printed at startup. Set `REGRESSION_SUITES`
to a space-separated subset of `dev plain gc gengc prof gcprof par explicit
parallel repl replgc repltagged replgengc` to rerun selected suites. The REPL
variants cover no-GC, GC, tagged-pair GC, and generational GC with full Basis
pretty printing. The focused `check` target additionally
covers foreign callbacks, forced GC, tagged pairs, and optional Argobots.
Add `argobots` to `REGRESSION_SUITES` and set `ARGOBOTS_ROOT` to run the full
parallel suite against the static Argobots library as well.

The bootstrap check uses three stages, each with a fresh cache, verifies each
compiler's architecture and execution, and compares stripped stage-two and
stage-three binaries. Run the same check for the compatibility backend with
`BOOTSTRAP_TARGET=x86_64`, `BOOTSTRAP_COMPILER` set to an X64-emitting MLKit,
and `SML_LIB` set to the source checkout:

```sh
sh src/Compiler/Backend/Arm64/tests/check-bootstrap.sh
```

The X64 check uses the classic Darwin linker, matching the existing bootstrap
rule, to avoid nondeterministic GOT ordering in the newer linker. ARM uses the
default linker. Comparison copies retain the same basename because Apple
`strip` uses it in the ARM ad-hoc signature. Set
`BOOTSTRAP_JOBS` to increase MLKit's compilation parallelism (the default is 1),
or `BOOTSTRAP_LINKER` to test another linker explicitly.

The backend reserves context/exception registers in both allocator palettes,
relaxes conditional branches through unconditional branches for large generated
functions, and retains global regions through exit callbacks. Full-suite tests
exercise packed numeric tables and boxed/unboxed conversions in both tagging
modes; focused probes cover long branches and GC-visible nullary constructors.

Milestone 7 validation on Apple Silicon with Apple Clang 21 and SDK 26.5:

| Suite | Result |
| --- | --- |
| Native and X64 `test_dev` | 130/130 each |
| Native `test`: no-GC, GC, generational GC, no-GC/GC profiling, pthreads | 180/180 in each configuration |
| ReML explicit regions | 79/79 |
| Dedicated pthread and static Argobots suites | 13/13 each |
| Basis-based and minimal REPL cases: no-GC, GC, tagged-pair GC, generational GC | 8/8 each |
| Final native and X64 `test` with GC, including the new regression | 181/181 each |
| Final packed-array `Int31` switch regression | All six native configurations and X64 GC/no-GC |
| Fresh native and X64 bootstrap | Byte-identical stripped fixed points |

The focused native suite also passes forced-GC, FFI/export, tagging, profiling,
and long-branch checks. The shared profiler regression passes on both targets
and a native full-Basis profiling program passes AddressSanitizer. Native
`rp2ps` produces matching graph data when region/stack graphs are requested
together or separately; the graph data also matches X64 output.
The narrow-integer switch regression exposed the corresponding X64 bug as
well; both backends now normalize the loaded representation before comparing
it. Additional X64 probes cover boxed and unboxed `Int32` patterns.
The REPL runtime decodes the tagged ML length returned by the exported
pretty-printer, returns constructor high-bit tags in ML integer representation,
and recognizes `int63` in the minimal printer. The fresh SML bootstrap stages
remain byte-identical after relinking with these final runtime fixes.

## Compiler performance

See the [initial ARM64/X64 timing comparison](arm64-compiler-timings.md) and
the [static-data and code-suffix results](arm64-continuation-results.md).
The latter compares the same ARM64 workloads before and after changing
instruction-list construction, without changing register handling.

The [allocation and reset paths](arm64-allocation-paths.md) use inline page fast
paths and shared preserving slow paths to shorten generated code and avoid
register saves on ordinary allocations.
