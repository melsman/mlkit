#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${REPRESENTATION_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-representations.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
for mode in no_gc gc; do
  for fixture in flatten-floats report-tuple-layouts; do
    for setting in report trace quiet disabled; do
      name=$mode-$fixture-$setting
      cp "$TESTS/$fixture.sml" "$OUT/$name.sml"
      case $setting in
        report) switches=--report_datatype_representations ;;
        trace) switches='--no_report_datatype_representations --report_representation_rewrites' ;;
        quiet) switches=--no_report_datatype_representations ;;
        disabled) switches='--report_datatype_representations --no_data_flattening' ;;
      esac
      "$MLKIT" "--$mode" --no_par --no_unbox_funargs --data_flattening_floats \
        $switches -o "$OUT/$name" "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
      case $setting in
        report|disabled)
          grep -q '^Datatype representations (final declarations):' "$OUT/$name-build.log"
          grep -q 'Constructor instances (final layouts):' "$OUT/$name-build.log"
          if [ "$setting" = report ]; then
            grep -q 'raw f64' "$OUT/$name-build.log"
          fi ;;
        *)
          if grep -q '^Datatype representations' "$OUT/$name-build.log"; then
            echo "Unexpected final report for $setting" >&2; exit 1
          fi ;;
      esac
      if [ "$setting" = trace ]; then
        grep -q 'float flattening at field' "$OUT/$name-build.log"
        grep -q 'spliced positions 1,0' "$OUT/$name-build.log"
      elif grep -q '^Tuple layout changes' "$OUT/$name-build.log"; then
        echo "Unexpected rewrite trace for $setting" >&2; exit 1
      fi
      if [ "$fixture" = report-tuple-layouts ] && [ "$setting" = report ]; then
        test "$(grep -c 'SOME :' "$OUT/$name-build.log")" = 1
        grep -q 'raw f64 \* string' "$OUT/$name-build.log"
      fi
      if [ "$fixture" = report-tuple-layouts ] && [ "$setting" = disabled ]; then
        if grep -q 'raw f64' "$OUT/$name-build.log"; then
          echo "Unexpected flattened payload with flattening disabled" >&2; exit 1
        fi
      fi
      "$OUT/$name" > "$OUT/$name.out"
      grep -q 'passed$' "$OUT/$name.out"
    done
    for setting in trace quiet disabled; do
      cmp "$OUT/$mode-$fixture-report.out" "$OUT/$mode-$fixture-$setting.out"
    done
  done
done
echo "Final datatype, constructor-instance and optional rewrite reports passed ($OUT)"
