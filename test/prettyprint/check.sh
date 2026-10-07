#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/prettyprint-spans.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
# The Report adapter changes shared-source types; never reuse compiler caches.
"${MLKIT:-$ROOT/bin/mlkit}" -mlb-subdir PrettyPrintTests -gc -o "$OUT/spans" "$ROOT/test/prettyprint/spans.mlb"
"$OUT/spans"
"${MLKIT:-$ROOT/bin/mlkit}" -mlb-subdir PrettyPrintTests -gc -o "$OUT/locations" "$ROOT/test/prettyprint/locations.mlb"
"$OUT/locations"
"${MLKIT:-$ROOT/bin/mlkit}" -mlb-subdir PrettyPrintTests -gc -o "$OUT/ir-report" "$ROOT/test/prettyprint/ir-report.mlb"
"$OUT/ir-report"

sh "$ROOT/test/prettyprint/check-link-map.sh"
