#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
REML=${REML:-$ROOT/bin/reml-arm64}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-text-flow.XXXXXX")
echo "Text region-flow artifacts: $OUT"
for mode in plain prof; do
  cp "$ROOT/test/region_profile/graph.sml" "$OUT/$mode.sml"
  printf '%s\n' "$mode.sml" > "$OUT/$mode.mlb"
  case "$mode" in
    plain) set -- -Prfg ;;
    prof) set -- -rp -print_region_flow_graph ;;
  esac
  "$REML" -no_par "$@" -log_to_file -o "$OUT/$mode" "$OUT/$mode.mlb" > "$OUT/$mode.build" 2>&1
  case "$mode" in
    plain) log="$OUT/$mode.sml.log" ;;
    prof) set -- "$OUT"/MLB/*/"$mode.sml.log"; log=$1 ;;
  esac
  grep -q 'Begin layout of region flow graph' "$log"
  grep -q 'LETREGION' "$log"
  test -z "$(find "$OUT" -name '*.vcg' -print)"
done
echo 'Both region-flow options print text without VCG output, with and without profiling'
