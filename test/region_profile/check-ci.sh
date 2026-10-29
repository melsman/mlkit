#!/bin/sh
# Run after make mlkit_basislibs. Uses the newly built native tools on either host.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
REML=${REML:-$ROOT/bin/reml}
PVIEW=${PVIEW:-$ROOT/bin/pview}
CC=${CC:-${THECC:-cc}}
export MLKIT REML PVIEW CC
export SML_LIB="$ROOT"
# Optional scheduler experiments and timing benchmarks are not release CI gates.
unset ARGOBOTS_ROOT
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-profiler-ci.XXXXXX")
mkdir "$OUT/tmp"
TMPDIR=$OUT/tmp
export TMPDIR
# Build test data tools with the configured seed, not the compiler under test.
make -C "$ROOT" rpfixture
RPENCODER=${RPENCODER:-$ROOT/bin/rpfixture}
export RPENCODER
echo "Profiler CI artifacts: $OUT"
run () {
  name=$1
  shift
  echo "Profiler CI: $name"
  if "$@" > "$OUT/$name.log" 2>&1; then
    cat "$OUT/$name.log"
  else
    cat "$OUT/$name.log" >&2
    find "$OUT/tmp" -type f \( -name '*.build' -o -name '*.log' -o -name '*.report' -o -name '*.out' -o -name stderr \) \
      -exec sh -c 'for file do echo "--- $file"; tail -n 30 "$file"; done' sh {} + >&2
    echo "Profiler CI failed: $name; artifacts retained at $OUT" >&2
    exit 1
  fi
}
run basis-linking sh "$ROOT/test/region_profile/check-basis-linking.sh"
run metadata sh "$ROOT/test/region_profile/check-metadata.sh"
if [ "$(uname -sm)" = 'Darwin arm64' ] || [ "$(uname -sm)" = 'Linux x86_64' ]; then
  run unified-options sh "$ROOT/test/time_profile/check-unified-options.sh"
  run time-parallel-blocking sh "$ROOT/test/time_profile/check-parallel-blocking.sh"
  run time-parallel sh "$ROOT/test/time_profile/check-parallel.sh"
  run time-ordinary sh "$ROOT/test/time_profile/check-ordinary.sh"
  run time-conflicts sh "$ROOT/test/time_profile/check-conflicts.sh"
  run time-attribution-modes sh "$ROOT/test/time_profile/check-attribution-modes.sh"
  run time-attribution sh "$ROOT/test/time_profile/check-attribution.sh"
  run time-recording sh "$ROOT/test/time_profile/check-recording.sh"
  run time-text sh "$ROOT/test/time_profile/check-text.sh"
  if [ "$(uname -sm)" = 'Linux x86_64' ]; then
    run time-x64-abi sh "$ROOT/test/time_profile/check-x64-abi.sh"
    run code-metadata-elf sh "$ROOT/test/time_profile/check-elf.sh"
  else
    run code-metadata sh "$ROOT/test/time_profile/check-code-metadata.sh"
  fi
fi

run runtime-args sh "$ROOT/test/region_profile/check-runtime-args.sh"
run library-builds sh "$ROOT/test/region_profile/check-library-builds.sh"
run ir sh "$ROOT/test/region_profile/check-ir.sh"
run ir-primitives sh "$ROOT/test/region_profile/check-ir-primitives.sh"
run text-flow sh "$ROOT/test/region_profile/check-text-flow.sh"
run auto-call sh "$ROOT/test/region_profile/check-auto-call.sh"
run accounting sh "$ROOT/test/region_profile/check.sh"
run ir5 sh "$ROOT/test/region_profile/check-ir5.sh"
run all-regions sh "$ROOT/test/region_profile/check-all-regions.sh"
run allocation sh "$ROOT/test/region_profile/check-allocation.sh"
run extended sh "$ROOT/test/region_profile/check-extended.sh"
run link-map sh "$ROOT/test/prettyprint/check-link-map.sh"
run binary sh "$ROOT/test/region_profile/check-binary.sh"
run viewer sh "$ROOT/test/region_profile/check-viewer.sh"
run ir-viewer sh "$ROOT/test/region_profile/check-ir-viewer.sh"
run svg sh "$ROOT/test/region_profile/check-svg.sh"
# Viewer checks cover HTML generation and graph/IR behavior in a Node DOM
# harness. Visual layout in a real browser remains a separate manual check.
run install make -C "$ROOT" install_runtime install_basis install_mlkit_basislibs LIBDIR="$OUT/installed"
chmod -R a-w "$OUT/installed"
run installed-api env SML_LIB="$OUT/installed" sh "$ROOT/test/region_profile/check-installed-api.sh"
echo 'Region profiler CI checks passed'
