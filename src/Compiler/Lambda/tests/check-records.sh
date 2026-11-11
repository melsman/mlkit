#!/bin/sh
# Direct IR coverage complements source regressions: source records are immutable.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=${RECORD_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-records.XXXXXX")}
mkdir -p "$OUT"
echo "Record test outputs: $OUT"
build () {
  "$MLKIT" --mlb-subdir RecordTestsCross -o "$OUT/$1" "$TESTS/$2.mlb" > "$OUT/$1-build.log" 2>&1
}
build ir records
"$OUT/ir" > "$OUT/ir.log" 2>&1
grep -q 'record IR tests passed' "$OUT/ir.log"
build analysis analysis
"$OUT/analysis" "$OUT/barified.sml" > "$OUT/analysis.log" 2>&1
grep -q 'data flattening analysis tests passed' "$OUT/analysis.log"
"$MLKIT" -o "$OUT/barified" "$OUT/barified.sml" > "$OUT/barified-build.log" 2>&1
"$OUT/barified" > "$OUT/barified.log" 2>&1
grep -q 'mutable singleton barification passed' "$OUT/barified.log"
build js records-js
"$OUT/js" "$OUT/mixed.js" > "$OUT/js.log" 2>&1
node - "$OUT/mixed.js" <<'JS'
global.recordCheck = (f,s,n) => {
  if (f !== 3.5 || s !== 'after' || n !== 7) throw Error('mixed record read/store');
  console.log('mixed record read/store passed');
};
require(process.argv[2]);
JS
case "$(uname -s)/$(uname -m)" in
  Darwin/arm64) target=arm64; runtime=$ROOT/lib/darwin-arm64 ;;
  Linux/x86_64) target=x64; runtime=$ROOT/lib ;;
  *) echo 'Native record execution requires Darwin/arm64 or Linux/x86_64' >&2; exit 1 ;;
esac
build native "records-$target"
for mode in plain gc gengc; do
  "$OUT/native" "$mode" "$OUT/$mode" > "$OUT/$mode-emit.log" 2>&1
  case "$mode" in
    plain) defines=; archive=runtimeSystem.a ;;
    gc) defines=-DTAG_VALUES; archive=runtimeSystemGC.a ;;
    gengc) defines=-DTAG_VALUES; archive=runtimeSystemGenGC.a ;;
  esac
  "$CC" $defines -iquote"$ROOT/src/Runtime" "$OUT/$mode.s" "$OUT/$mode-link.s" "$TESTS/record-check.c" \
    "$runtime/$archive" -lm -o "$OUT/$mode" > "$OUT/$mode-link.log" 2>&1
  "$OUT/$mode" > "$OUT/$mode.log" 2>&1
  grep -q 'mixed record read/store passed' "$OUT/$mode.log"
done
# Execute an actual multi-field splice, including mutable raw and pointer
# fields, through the same native and JavaScript backend drivers.
for fixture in 1 float; do
FLATTEN_RECORD_TEST=$fixture "$OUT/js" "$OUT/flatten.js" > "$OUT/flatten-js.log" 2>&1
node - "$OUT/flatten.js" <<'JS'
global.recordCheck = (f,s,n) => {
  if (f !== 3.5 || s !== 'after' || n !== 7) throw Error('flattened mixed record');
  console.log('flattened mixed record passed');
};
require(process.argv[2]);
JS
for mode in plain gc gengc; do
  FLATTEN_RECORD_TEST=$fixture "$OUT/native" "$mode" "$OUT/flatten-$mode" > "$OUT/flatten-$mode-emit.log" 2>&1
  case "$mode" in
    plain) defines=; archive=runtimeSystem.a ;;
    gc) defines=-DTAG_VALUES; archive=runtimeSystemGC.a ;;
    gengc) defines=-DTAG_VALUES; archive=runtimeSystemGenGC.a ;;
  esac
  "$CC" $defines -iquote"$ROOT/src/Runtime" "$OUT/flatten-$mode.s" "$OUT/flatten-$mode-link.s" \
    "$TESTS/record-check.c" "$runtime/$archive" -lm -o "$OUT/flatten-$mode" > "$OUT/flatten-$mode-link.log" 2>&1
  "$OUT/flatten-$mode" > "$OUT/flatten-$mode.log" 2>&1
  grep -q 'mixed record read/store passed' "$OUT/flatten-$mode.log"
done
done
if [ "$target" = arm64 ]; then
  "$CC" -DDARWIN_NATIVE=1 -DENABLE_GC -DENABLE_GEN_GC -DTAG_VALUES -iquote"$ROOT/src/Runtime" \
    "$ROOT/src/Runtime/tests/mutable-record-gc.c" "$runtime/runtimeSystemGenGC.a" \
    -lm -o "$OUT/scan" > "$OUT/scan-build.log" 2>&1
  "$OUT/scan" > "$OUT/scan.log" 2>&1
  grep -q 'mutable record old-to-young GC scan passed' "$OUT/scan.log"
fi
cp "$TESTS/ordinary.sml" "$OUT/ordinary.sml"
for flags in '--no_data_flattening --unbox_funargs' \
             '--data_flattening --data_flattening_refs --data_flattening_tuples --data_flattening_datatypes --data_flattening_floats' \
             '--data_flattening --no_unbox_funargs' \
             '--no_optimiser --data_flattening --no_data_flattening_refs --no_data_flattening_tuples --no_data_flattening_datatypes --no_data_flattening_floats'; do
  # Intentional splitting: these are fixed option lists, not shell input.
  "$MLKIT" $flags -o "$OUT/ordinary" "$OUT/ordinary.sml" > "$OUT/ordinary-build.log" 2>&1
  "$OUT/ordinary" > "$OUT/ordinary.log" 2>&1
  grep -q 'ordinary tuple compatibility passed' "$OUT/ordinary.log"
done
for feature in data_flattening_refs no_data_flattening_refs; do
  cp "$TESTS/analysis-source.sml" "$OUT/$feature.sml"
  "$MLKIT" --mlb-subdir RecordTestsCross --report_data_flattening \
    --no_unbox_funargs "--$feature" \
    -o "$OUT/$feature" "$OUT/$feature.sml" > "$OUT/$feature-build.log" 2>&1
  grep -q 'data flattening .* reference:' "$OUT/$feature-build.log"
  if [ "$feature" = no_data_flattening_refs ]; then
    grep -q 'reference: disabled' "$OUT/$feature-build.log"
  else
    grep -q 'data flattening: transformed' "$OUT/$feature-build.log"
  fi
  "$OUT/$feature" > "$OUT/$feature.log" 2>&1
  grep -q 'analysis source test passed' "$OUT/$feature.log"
done
# The same source runs with independent reference/tuple/argument controls.
# Recursive consumers prevent elimination of all record allocations.
for mode in no_gc gc gengc; do
  for features in default none refs tuples both; do
    case "$features" in
      default) switches= ;;
      none) switches='--no_data_flattening_refs --no_data_flattening_tuples' ;;
      refs) switches='--data_flattening_refs --no_data_flattening_tuples' ;;
      tuples) switches='--no_data_flattening_refs --data_flattening_tuples' ;;
      both) switches='--data_flattening_refs --data_flattening_tuples' ;;
    esac
    for args in unbox_funargs no_unbox_funargs; do
      name=flatten-$mode-$features-$args
      cp "$TESTS/flatten-source.sml" "$OUT/$name.sml"
      "$MLKIT" --mlb-subdir RecordTestsCross --report_data_flattening \
        "--$mode" "--$args" $switches \
        -o "$OUT/$name" "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
      if [ "$features" = none ]; then
        if grep -q 'data flattening: transformed' "$OUT/$name-build.log"; then
          echo 'Disabled features transformed a program' >&2; exit 1
        fi
      fi
      "$OUT/$name" > "$OUT/$name.log" 2>&1
      grep -q 'data flattening semantics passed' "$OUT/$name.log"
    done
  done
done
# Retain type schemes when the master switch is requested, even with optional
# checking and the global optimiser off; reuse them when optimisation resumes.
cat > "$OUT/cache-provider.sml" <<'SML'
fun tupleIdentity (p:int * int) = p
SML
cat > "$OUT/cache-client.sml" <<'SML'
val selected = #1 (tupleIdentity (1,2))
val () = prim("printStringML", "analysis cache test passed\n")
SML
printf 'cache-provider.sml\ncache-client.sml\n' > "$OUT/cache.mlb"
"$MLKIT" --no_data_flattening -o "$OUT/cache" "$OUT/cache.mlb" \
  > "$OUT/cache-standard.log" 2>&1
"$MLKIT" --data_flattening --no_type_check_lambda --no_optimiser -o "$OUT/cache" "$OUT/cache.mlb" \
  > "$OUT/cache-requested.log" 2>&1
cp "$OUT/cache-client.sml" "$OUT/cache-reload.sml"
printf 'cache-provider.sml\ncache-reload.sml\n' > "$OUT/cache-reload.mlb"
"$MLKIT" --no_type_check_lambda --data_flattening -o "$OUT/cache-reload" "$OUT/cache-reload.mlb" \
  > "$OUT/cache-reload-build.log" 2>&1
if grep -q 'reading source file:.*cache-provider.sml' "$OUT/cache-reload-build.log"; then
  echo 'Toggling the global optimiser unexpectedly invalidated the analysed dependency' >&2
  exit 1
fi
"$OUT/cache-reload" > "$OUT/cache-reload.log" 2>&1
grep -q 'analysis cache test passed' "$OUT/cache-reload.log"
echo 'Record representation, lowering, GC, and option checks passed'
