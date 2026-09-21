# macOS runtime target selection

This implements the runtime portability/build portion of issue #223. It does
not implement an ARM compiler backend or change the GC stack/register ABI.

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

The ARM backend must select the ARM archive directory and its own compilation
cache namespace when introduced. Until then, native-mode compiler, Basis
cache, bootstrap, and full-install targets are rejected. This prevents the
existing X64 compiler entry points from populating apparent ARM outputs.
Use the X64 configuration for ordinary compiler/tool builds. Tool porting and
ARM cache generation belong to later milestones.

`make -C src/Runtime clean` removes the configured target's intermediate
build tree. It intentionally preserves installed/copied archives and the other
target's tree. Running the archive targets again refreshes the copies.

## Runtime changes and layout audit

Profiling reads SP based on architecture macros, not compiler identity. The
protected C allocator uses acquire loads of its allocation pointer and a
release store to publish an initialized page, replacing the compiler-selected
empty barrier/mfence. Its compare/exchange operations remain sequentially
consistent. X64 generated allocation uses locked compare/exchange. Future ARM
generated allocation must provide acquire ordering before consuming published
pages; building the parallel C archive is not validation of that future path.

`Layout.c` is compiled for every runtime variant. It checks the 64-bit LP64
word/pointer model, integer/double sizes and alignment, generation and region
page sizes, region descriptor fields (including GC/profiling/parallel
variants), context fields, exception layout, and the string data offset.
These correspond to data-layout assumptions in BackendInfo and code
production. ARM GC register saves, root numbering, and frame traversal remain
an explicit later ABI task; the existing 16-integer/8-float save layout is
unchanged. No assumption about long double equivalence is needed by these
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
or GC test. Generated ARM programs, GC roots/frames, profiling output, generated parallel
allocation tests, and bootstrap validation remain later milestones.

Validation on 2026-09-21 used Apple Clang 21 via `gcc`, macOS arm64, and
Rosetta 2 for X64 execution. All ten standard archive variants built for both
targets; the architecture, allocation, concurrent allocation, installation,
legacy archive preservation, and rejection checks passed. An installed X64
MLKit also compiled and ran `test_dev/int_first.sml` against the new runtime
with `-no_gc` and with `-gc -prof`. The checkout was left configured for X64.
Argobots and Linux execution were not tested in this environment.
