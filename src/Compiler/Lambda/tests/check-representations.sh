#!/bin/sh
# Reuse the float fixture to exercise several rewrites but one final report.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${REPRESENTATION_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-representations.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
for mode in no_gc gc; do
  for setting in report quiet disabled; do
    name=$mode-$setting
    cp "$TESTS/flatten-floats.sml" "$OUT/$name.sml"
    case $setting in
      report) switches=--report_datatype_representations ;;
      quiet) switches=--no_report_datatype_representations ;;
      disabled) switches='--report_datatype_representations --no_data_flattening' ;;
    esac
    "$MLKIT" "--$mode" --no_par --no_unbox_funargs --data_flattening_floats \
      $switches -o "$OUT/$name" "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
    if [ "$setting" = report ]; then
      test "$(grep -c '^Datatype representations (final specialised declarations):' "$OUT/$name-build.log")" = 1
      grep -q 'sample#[0-9].*inherited' "$OUT/$name-build.log"
      grep -q 'raw f64' "$OUT/$name-build.log"
    elif grep -q '^Datatype representations' "$OUT/$name-build.log"; then
      echo "Unexpected report for $setting" >&2; exit 1
    fi
    "$OUT/$name" > "$OUT/$name.out"
    grep -q '^float data flattening passed$' "$OUT/$name.out"
  done
  cmp "$OUT/$mode-report.out" "$OUT/$mode-quiet.out"
  cmp "$OUT/$mode-report.out" "$OUT/$mode-disabled.out"
done
echo "Datatype representation reporting checks passed ($OUT)"
