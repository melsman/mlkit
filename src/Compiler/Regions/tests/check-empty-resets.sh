#!/bin/sh
set -eu
: "${SML_LIB:?Set SML_LIB to the compiler source checkout}"
MLKIT=${MLKIT:-$SML_LIB/bin/mlkit}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-empty-resets.XXXXXX")
echo "Empty-region analysis test outputs: $scratch"
"$MLKIT" --no_eliminate_empty_region_resets --mlb-subdir EmptyResetTests -o "$scratch/tests" \
  "$SML_LIB/src/Compiler/Regions/tests/empty-resets.mlb" > "$scratch/build.log" 2>&1 || {
  cat "$scratch/build.log" >&2
  exit 1
}
"$scratch/tests"
