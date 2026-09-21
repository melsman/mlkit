# macOS runtime target selection

This describes the runtime portability/build portion of issue #223. An
experimental ARM compiler is described in [arm64-compiler.md](arm64-compiler.md).
The collector selects a target-specific stack/register ABI.

## Build and toolchain

Run `sh autobuild` once to generate configure and config.h.in. The supported
macOS configurations are:

```sh
DARWIN_NATIVE=0 ./configure CC=gcc
make runtime -j4

DARWIN_NATIVE=1 ./configure CC=gcc
make runtime -j4
```

Both use `gcc`. With Apple's command-line tools this is Apple Clang. Configure
adds `-arch x86_64` or `-arch arm64` to CFLAGS and checks the compiler's target
macros. The runtime checks those macros again for every translation unit, so
changing CC/CFLAGS during make cannot silently build the wrong architecture.
DARWIN_NATIVE accepts only 0 or 1; 1 requires macOS. Configure remains native
on non-macOS hosts with DARWIN_NATIVE=0.

Use a compiler supporting Apple's `-arch` option and an installed macOS SDK.
`xcrun --find gcc` and `xcrun --sdk macosx --show-sdk-path` identify the selected
Apple toolchain and SDK. Set DEVELOPER_DIR/SDKROOT before configure and make
when selecting a different Xcode/SDK. Custom CFLAGS must be appropriate for the
selected target. Do not pass multiple architectures; universal runtime
archives are not supported by this configuration.

On Apple Silicon the x86_64 configuration requires Rosetta 2 to run configure
probes, the errno-table generator, and X64 compiler/tools. No translated shell
is required when gcc supports explicit target selection. On Intel Macs,
running arm64 configure probes and smoke tests requires an ARM machine; this
is not a cross-compilation setup.

## Artifact and installation isolation

Objects, generated dependencies, and the errno-table generator/header live in
`src/Runtime/build/darwin-arm64/` or `src/Runtime/build/darwin-x86_64/`, with
separate subdirectories for each runtime variant. CUtils objects are built
there too. Makefile/config/header changes invalidate the affected objects.
Reconfigure sequentially; concurrent builds under different configurations in
the same checkout are not supported.

Archives are copied to `lib/darwin-arm64/` or `lib/darwin-x86_64/`. Only X64
also refreshes `lib/runtimeSystem*.a`, for compatibility with the current X64
compiler. Native builds never overwrite those compatibility archives.

`make install_runtime` installs the configured target's archives below
`$(LIBDIR)/lib/<target>/`; X64 also installs its existing compatibility paths.
For example, stage runtime archives without a system install:

```sh
make install_runtime LIBDIR=/tmp/mlkit-runtime-stage
```

The experimental ARM backend selects the ARM archive directory and a
separate `MLB/ARM64_<variant>` cache namespace. Use `Makefile.arm64` for
host-built ARM-emitting compilers, native compiler/tool builds, staged native
installation, and bootstrap checks; see [arm64-compiler.md](arm64-compiler.md).
The ordinary compiler/tool targets retain their X64 configuration.

`make -C src/Runtime clean` removes the configured target's intermediate
build tree. It intentionally preserves installed/copied archives and the other
target's tree. Running the archive targets again refreshes the copies.

## Runtime changes and layout audit

Profiling reads SP based on architecture macros, not compiler identity. The
protected C allocator uses acquire loads of its allocation pointer and a
release store to publish an initialized page, replacing the compiler-selected
empty barrier/mfence. Its compare/exchange operations remain sequentially
consistent. X64 generated allocation uses locked compare/exchange. ARM generated allocation calls this protected C allocator, including its
acquire/CAS/release page-publication path; there is no separate inline ARM
allocation fast path. `-par0` explicitly selects `alloc_unprotected` and requires
disjoint allocation regions.

`Layout.c` is compiled for every runtime variant. It checks the 64-bit LP64
word/pointer model, integer/double sizes and alignment, generation and region
page sizes, region descriptor fields (including GC/profiling/parallel
variants), context fields, exception layout, and the string data offset.
These correspond to data-layout assumptions in BackendInfo and code
production. ARM GC uses the separate snapshot and return-PC index described
in [arm64-abi.md](arm64-abi.md); the X64 save layout is unchanged. No assumption about long double equivalence is needed by these
runtime layouts.

## Validation

On an Apple Silicon Mac with Rosetta 2, run:

```sh
sh src/Runtime/tests/check-darwin.sh
```

The script builds all ten standard runtime variants for X64 and ARM, checks
archive/object architecture, runs a C-entry allocation smoke test on each,
checks installation and legacy archive isolation, and checks invalid/mismatched
configurations. It leaves the checkout configured for X64. Argobots is optional
and requires a matching architecture build supplied with `--with-argobots`;
its archive is not part of the default matrix.

The C smoke test exercises runtime initialization, multi-page allocation,
large objects, retained contents, and region deallocation. A second test uses
four runtime threads sharing a protected region, verifying 32,768 allocations
across page rollover. It is not an ML ABI
or GC test. The native compiler suite separately validates generated ARM GC
and profiling paths. Generated parallel allocation is covered by the compiler parallel suite;
native bootstrap validation uses the separate `Makefile.arm64 bootstrap` target.

Validation on 2026-09-21 used Apple Clang 21 via `gcc`, SDK 26.5, macOS arm64,
and Rosetta 2 for X64 execution. All ten standard archive variants built for both
targets; the architecture, allocation, concurrent allocation, installation,
legacy archive preservation, and rejection checks passed. An installed X64
MLKit also compiled and ran `test_dev/int_first.sml` against the new runtime
with `-no_gc` and with `-gc -prof`. The checkout was left configured for X64.
Linux execution was not tested in this environment.

The matrix also checks repeated profiling samples on both architectures.
Completed samples are streamed and freed; the runtime no longer links a new
sample through the previously freed sample. The regression detects that write
by reusing the freed allocation. A native full-Basis profiling program also
passes with AddressSanitizer.

## ARM GC and profiling integration

`Arm64GC.c` owns a single-threaded registry of image frame indexes, static-data
ranges, and global root cells. `GC.c` selects the ARM snapshot/frame walker
under `DARWIN_NATIVE`; the X64 walker is unchanged. Descriptors store 32-bit
bitmap words in 64-bit slots and identify the saved LR slot explicitly.
Unknown return PCs abort with a metadata diagnostic instead of scanning code.

Generated allocation selects the matching region kind and profiling helpers.
Finite profiling descriptors and allocation-point metadata follow the existing
runtime layout. Function-entry profiling receives the original ML SP, updates
stack/memory statistics, and invokes `profileTick` when requested. Exception
unwinding also removes finite profiling descriptors.

All supported MLKit GC/profiling combinations are exercised by the native
suite. ReML retains its existing no-GC restriction. Parallel GC, tagging, and profiling have no supported runtime combination
and are rejected. The image registry remains single-threaded. See [arm64-compiler.md](arm64-compiler.md)
for foreign-call GC deferral and current language limits.

## ARM parallel execution (milestone 6)

`-par` supports the existing untagged, no-GC, non-profiling pthread runtime.
Region protection follows the compiler's effect analysis; global regions are
protected. `--alloc_protect_always` forces protection of infinite regions.
The worker bridge initializes thread-local state, installs `ThreadInfo.ctx` in
x28, invokes the captured ML closure with unit, and passes the ML result to
`thread_exit`. `Layout.c` checks the closure/context offsets for both runtimes.
Exported callbacks obtain the calling worker's context through `thread_info`.
Dynamic exception names use an acquire/release exclusive-update loop.

`thread_get` publishes its result and completed cleanup with release ordering;
subsequent readers acquire that publication. Thread IDs use an atomic counter,
and the shared mutex freelist is always inspected under its lock. Argobots
joins use an Argobots mutex so a suspended joiner does not block the execution
stream; completed Argobots thread handles are freed after joining. ML's existing
`Thread` wrapper retains `ThreadInfo` objects because thread values may escape
and be joined again; this milestone does not change that lifetime policy.

For optional Argobots, build an ARM64 copy of Argobots first, then run:

```sh
DARWIN_NATIVE=1 ./configure CC=gcc --with-argobots=/absolute/path/to/argobots
make -C src/Runtime runtimeSystemArPar.a
ARGOBOTS_ROOT=/absolute/path/to/argobots \
  make -f Makefile.arm64 check MLKIT_BOOTSTRAP=/usr/local/bin/mlkit
```

`ARGOBOTS_ROOT` is a configured source build containing `src/include/abt.h` and
`src/.libs/libabt.a`, in a path without spaces. For application builds use
`-par -argo` and supply the matching Argobots library to the linker, for example
`-ldexe 'gcc -arch arm64 /absolute/path/to/argobots/src/.libs/libabt.a'`.
Argobots caches have an additional `_ARGO` suffix. Programs accept `-p N` to
select the number of execution streams.

The permanent parallel suite tests MLKit and ReML, inferred/forced protection,
private-region `-par0`, nested spawning, shared lists across page boundaries,
repeated joins, worker/parent exceptions, and callbacks on worker contexts.
Dynamic exception constructors are also created concurrently and checked for
distinct identities. C stress tests validate page publication and concurrent result readers, thread
ID uniqueness, and freelist reuse. Argobots runs with one and four execution
streams. The standard runtime matrix also runs both C tests on X64/Rosetta.

Milestone 6 validation used Argobots source commit
`bffdf56916879f9321a1d7983fa486736cd1d722`, built with Apple Clang for ARM64.
The optional test is explicitly reported as skipped when no Argobots build is
provided; building the archive alone does not count as validation.
