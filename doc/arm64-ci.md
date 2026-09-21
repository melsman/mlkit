# macOS ARM GitHub Actions

Milestone 9 adds `macos-arm` jobs for both `mlkit` and `mlton` host compilers
in `.github/workflows/main.yml`. Existing Linux and Intel macOS coverage remains.
The logical `macos-arm` platform selects `macos-15`, an Apple Silicon runner
listed in [GitHub's runner reference](https://docs.github.com/en/actions/reference/runners/github-hosted-runners).

The MLKit job downloads the existing v4.7.13 Darwin release and uses Rosetta
for that X64 seed only. It builds separate X64 compatibility runtime archives
before configuring `DARWIN_NATIVE=1` and building the ARM64 runtime variants.
The MLton job uses [Homebrew's native package](https://formulae.brew.sh/formula/mlton)
and its own Basis to build the ARM-emitting seed compilers. MLton is used only
in its dedicated CI host entry; local development continues to use MLKit.
Both jobs then use the seed MLKit to produce native ARM64 MLKit, ReML, and tools.
All MLKit compiler/tool builds explicitly use `-gc`.

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
   checks, and an archive of the tested installation.

Argobots remains optional and is not provisioned by these jobs. The ARM jobs do
not run the X64 distribution's JS/PhantomJS tests; the existing jobs retain
that coverage. On a pushed `v*` tag, the MLKit-hosted ARM job publishes
`dist/mlkit-bin-dist-darwin.tgz` to the release. Darwin X64 jobs build and test
`dist/mlkit-bin-dist-darwin-x64.tgz`, and the MLKit-hosted X64 job publishes
that distinct asset. Linux retains `dist/mlkit-bin-dist-linux.tgz`. MLton jobs
validate distributions but do not publish release assets. Historical seed
downloads keep their original v4.7.13 filenames.

Jobs use separate VMs and host-specific output directories, compiler-cache
names, installation prefixes, and artifact names. They do not restore caches
from another run. Test scratch directories respect `TMPDIR`; CI retains native
fixture output with `ARM64_KEEP_TEST_OUTPUTS=1`. Logs and HTML test reports are
uploaded even on failure, under `logs-darwin-arm64-{mlkit,mlton}`. Successful
installations are uploaded as `mlkit-bin-dist-darwin-arm64-{mlkit,mlton}`.
Basis caches are excluded from the archive so they rebuild at the destination.
Unpack a distribution to a space-free prefix and set `SML_LIB` to that prefix.

Local workflow validation uses actionlint, shell syntax checks, and the native
suite with CI log retention enabled. Hosted run outcomes are tracked on PR #224;
configuration validation alone does not establish that both hosted jobs pass.
