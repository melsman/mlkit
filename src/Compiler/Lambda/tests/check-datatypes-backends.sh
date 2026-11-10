#!/bin/sh
# Check private datatype groups and ordinary imported consumers together.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${FLATTEN_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-datatypes.XXXXXX")}
mkdir -p "$OUT"
echo "Datatype flattening outputs: $OUT"
prepare () {
  name=$1
  basis=$2
  cp "$TESTS/flatten-datatypes-lib.sml" "$OUT/$name-lib.sml"
  if [ "$basis" = none ]; then
    printf 'fun print (s:string) : unit = prim("printStringML", s)\n' > "$OUT/$name.sml"
  else
    : > "$OUT/$name.sml"
  fi
  cat "$TESTS/flatten-datatypes.sml" >> "$OUT/$name.sml"
  {
    if [ "$basis" != none ]; then printf '%s/basis/basis.mlb\n' "$basis"; fi
    printf '%s-lib.sml\n%s.sml\n' "$name" "$name"
  } > "$OUT/$name.mlb"
}
verify () {
  grep -q 'datatype data flattening passed' "$OUT/$1.log"
  if [ "$setting" = disabled ]; then
    if grep -q 'datatype families' "$OUT/$1-build.log"; then
      echo "Datatype specialisation ran despite the disable switch" >&2; exit 1
    fi
    return
  fi
  grep -q 'reference: eligible' "$OUT/$1-build.log"
  grep -q 'tuple: eligible' "$OUT/$1-build.log"
  grep -q 'data flattening: transformed' "$OUT/$1-build.log"
}
BASE_OUT=$OUT
for setting in default disabled; do
  OUT=$BASE_OUT/$setting
  mkdir -p "$OUT"
  switches=
  if [ "$setting" = disabled ]; then switches=--no_data_flattening_datatypes; fi
  for mode in no_gc gc gengc; do
    name=native-$mode
    prepare "$name" none
    SML_LIB=$ROOT "$MLKIT" $switches --report_data_flattening \
      "--$mode" -no_basislib -o "$OUT/$name" "$OUT/$name.mlb" > "$OUT/$name-build.log" 2>&1
    "$OUT/$name" > "$OUT/$name.log" 2>&1
    verify "$name"
    if [ -n "${MLKIT_X64:-}" ]; then
      name=x64-$mode
      prepare "$name" none
      SML_LIB=$ROOT "$MLKIT_X64" $switches --report_data_flattening \
        "--$mode" -no_basislib -c --assembler "${X64_ASSEMBLER:-cc -c}" \
        "$OUT/$name.mlb" > "$OUT/$name-build.log" 2>&1
      if [ "$setting" = default ]; then
        grep -q 'reference: eligible' "$OUT/$name-build.log"
        grep -q 'tuple: eligible' "$OUT/$name-build.log"
      else
        if grep -q 'datatype families' "$OUT/$name-build.log"; then
          echo "Datatype specialisation ran despite the disable switch" >&2; exit 1
        fi
      fi
    fi
  done
  if [ -n "${SMLTOJS:-}" ]; then
    prepare js "$ROOT/js"
    SML_LIB=$ROOT/js "$SMLTOJS" $switches --report_data_flattening \
      -o "$OUT/js" "$OUT/js.mlb" > "$OUT/js-build.log" 2>&1
    node "$ROOT/js/test/run-node.js" "$OUT/js.html" > "$OUT/js.log" 2>&1
    verify js
  fi
  if [ -n "${BARRY:-}" ]; then
    # Barry's SML output is exercised as one unit; native and JS tests above
    # additionally check the loaded environment boundary between the two units.
    cat "$TESTS/flatten-datatypes-lib.sml" "$TESTS/flatten-datatypes.sml" > "$OUT/barry.sml"
    (cd "$OUT" && SML_LIB=$ROOT "$BARRY" $switches --report_data_flattening \
      "$OUT/barry.sml") > "$OUT/barry-build.log" 2>&1
    generated=$(find "$OUT/MLB" -name barry.sml.sml -print)
    SML_LIB=$ROOT "$MLKIT" -o "$OUT/barry" "$generated" > "$OUT/barry-recompile.log" 2>&1
    "$OUT/barry" > "$OUT/barry.log" 2>&1
    verify barry
  fi
done
echo 'Datatype flattening backend checks passed'
