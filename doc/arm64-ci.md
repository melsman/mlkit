# Native GitHub Actions

`.github/workflows/main.yml` defines one build-and-test job with three matrix
entries:

| Platform | Runner | Host compiler | Released seed |
| --- | --- | --- | --- |
| Linux X64 | `ubuntu-24.04` | MLKit | v4.7.22 |
| Linux X64 | `ubuntu-24.04` | MLton | v4.7.22 |
| macOS ARM64 | `macos-26` | MLKit | v4.7.23 |

The workflow contains the CI orchestration, archive checks, and log collection
inline. Compiler and runtime regression scripts remain reusable under `src/`
and `test/`. Intel macOS is no longer built, tested, or packaged.

All entries configure with `./autobuild` and `./configure`, then use the normal
Makefile targets to build MLKit, ReML, tools, Basis libraries, and SMLtoJs.
They install the compilers, run regression and bootstrap checks, and build the
release archive with `make mlkit_bin_dist`. The MLton entry uses its newly built
MLKit to compile SMLtoJs.

The macOS entry downloads the native v4.7.23 Darwin release. That seed requires
macOS 26.0, hence the `macos-26` runner. Bootstrap compilation uses the seed's
prepared Basis cache and matching ARM64 runtime; Rosetta is not required.
Configure selects ARM64 by default. CI verifies the bootstrap compiler's
512 MiB stack, the architecture of native tools and runtime archives, and the
absence of checkout-built X64 runtime archives.

Platform-specific checks remain explicit in the shared job:

- Linux runs developer, profiling, GC/non-GC, explicit-region, REPL, parallelism,
  region-info, JavaScript/PhantomJS, and Barry tests, followed by `make bootstrap`.
- macOS runs the native emitter/integration and pthread tests, then the ARM64
  regression suites covering developer, GC/non-GC, generational GC, profiling,
  GC/profiling, pthreads, explicit regions, parallelism, and four REPL modes.
  It also runs three native bootstrap stages with a stripped stage-two/stage-three
  fixed-point comparison, and staged installation checks across runtime modes.

Argobots remains optional and is not provisioned. JavaScript execution tests
run on Linux; both platforms compile SMLtoJs and test its packaged compiler.

Every matrix entry validates its release archive. The archive must extract
into exactly one `mlkit-bin-dist-linux/` or `mlkit-bin-dist-darwin/` directory
and include its installation Makefile and precompiled Basis caches. CI installs
it into a fresh prefix, checks executable architectures (and ARM64 runtime
archives), and runs GC/non-GC programs, SMLtoJs compilation, and the default-GC
REPL with read-only libraries.

On a pushed `v*` tag, the MLKit-hosted entries publish
`dist/mlkit-bin-dist-linux.tgz` and `dist/mlkit-bin-dist-darwin.tgz`.
The MLton entry validates its archive but does not publish a release asset.
No Darwin X64 package is produced.

Each entry runs in a separate VM with its own temporary directory. Logs and
test reports are collected without following test symlinks and uploaded even
on failure as `logs-<platform>-<host compiler>`. Successful archives are also
uploaded as `mlkit-bin-dist-<platform>-<host compiler>`. No caches are restored
from another run. Native fixture output is retained with
`ARM64_KEEP_TEST_OUTPUTS=1`.

To install a release, unpack it, enter its top-level directory, and run
`make install`, optionally with `PREFIX=/path/to/install`. Use a space-free
prefix; for a custom prefix, follow the installer's library-path instructions.
