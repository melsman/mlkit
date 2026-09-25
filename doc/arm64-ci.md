# macOS ARM GitHub Actions

CI covers Linux X64 with MLKit and MLton host compilers, and macOS Arm64
with MLKit. Intel macOS is no longer built, tested, or packaged. The macOS
job uses the Apple Silicon `macos-26` runner because the published native
v4.7.23 seed declares macOS 26.0 as its minimum OS.

The macOS job downloads the native v4.7.23 Darwin release. Bootstrap
compilation uses its prepared Basis cache and matching ARM64 runtime;
Rosetta is not required. Configure selects ARM64 on macOS by default.
The generated seed compilers use a 512 MiB stack for self-compilation, and CI
verifies their ARM64 architecture and Mach-O stack size.

The phases in `.github/scripts/arm64.sh` perform:

1. Explicit runtime target configuration and archive architecture checks.
2. Seed compiler builds and architecture/version checks.
3. Native MLKit, ReML, emitter harness, kittester, lexer/parser generators, and
   rp2ps builds, with ARM64 architecture checks for every executable.
4. Native integration and pthread tests, followed by all default regression
   suites: developer, no-GC, GC, generational GC, profiling, GC/profiling,
   pthreads, explicit regions, parallelism, and four REPL configurations.
5. Three fresh native bootstrap stages and a stripped stage-two/stage-three
   fixed-point comparison.
6. Staged native installation tests across runtime modes, installed generator
   checks, and profiling-tool checks.
7. Standard `make all` and `make mlkit_bin_dist` packaging, including native
   SMLtoJs and precompiled Basis libraries. The archive must contain exactly
   one top-level `mlkit-bin-dist-darwin/` directory. CI extracts that archive,
   installs it into a fresh prefix, checks executable/runtime architectures,
   and runs GC/non-GC programs, SMLtoJs compilation, and the default-GC REPL
   with read-only libraries.

Argobots remains optional and is not provisioned by these jobs. The ARM job does
not run the JS/PhantomJS tests; Linux jobs retain that coverage. On a pushed
`v*` tag, the MLKit-hosted ARM job publishes
`dist/mlkit-bin-dist-darwin.tgz` to the release. Linux retains
`dist/mlkit-bin-dist-linux.tgz`. Linux MLton jobs validate distributions but
do not publish release assets. No Darwin X64 package is produced.

Jobs use separate VMs and host-specific output directories, compiler-cache
names, installation prefixes, and artifact names. They do not restore caches
from another run. Test scratch directories respect `TMPDIR`; CI retains native
fixture output with `ARM64_KEEP_TEST_OUTPUTS=1`. Logs and HTML test reports are
uploaded even on failure, under `logs-darwin-arm64-mlkit`. Successful
installations are uploaded as `mlkit-bin-dist-darwin-arm64-mlkit`.
The archive includes its installation Makefile and Basis caches. Unpack it,
enter `mlkit-bin-dist-darwin/`, and run `make install` (optionally with
`PREFIX=/path/to/install`). Use a space-free prefix; for a custom prefix,
follow the installer's library-path instructions. The development installation
checked in phase 6 is separate from the release archive tested in phase 7.

Local workflow validation uses actionlint, shell syntax checks, and the native
suite with CI log retention enabled. Hosted run outcomes are tracked on PR #224;
configuration validation alone does not establish that the hosted job passes.
