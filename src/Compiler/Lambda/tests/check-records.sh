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
echo 'Record representation, lowering, GC, and option checks passed'
