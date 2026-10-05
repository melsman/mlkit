#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/prettyprint-spans.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
"${MLKIT:-$ROOT/bin/mlkit}" -gc -o "$OUT/spans" "$ROOT/test/prettyprint/spans.mlb"
"$OUT/spans"
"${MLKIT:-$ROOT/bin/mlkit}" -gc -o "$OUT/locations" "$ROOT/test/prettyprint/locations.mlb"
"$OUT/locations"
"${MLKIT:-$ROOT/bin/mlkit}" -gc -o "$OUT/ir-report" "$ROOT/test/prettyprint/ir-report.mlb"
"$OUT/ir-report"
