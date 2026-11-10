#!/bin/sh
# Source-level feature controls complement check-records.sh's direct IR tests.
# Supply SMLTOJS, BARRY and/or MLKIT_X64 to exercise those installed backends.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${FLATTEN_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-flatten-backends.XXXXXX")}
mkdir -p "$OUT"
echo "Flattening backend outputs: $OUT"
for feature in default none refs tuples both; do
  case "$feature" in
    default) switches= ;;
    none) switches='--no_data_flattening_refs --no_data_flattening_tuples' ;;
    refs) switches='--data_flattening_refs --no_data_flattening_tuples' ;;
    tuples) switches='--no_data_flattening_refs --data_flattening_tuples' ;;
    both) switches='--data_flattening_refs --data_flattening_tuples' ;;
  esac
  for args in unbox_funargs no_unbox_funargs; do
    name=$feature-$args
    if [ -n "${SMLTOJS:-}" ]; then
      cp "$TESTS/flatten-portable.sml" "$OUT/js-$name.sml"
      SML_LIB=$ROOT/js "$SMLTOJS" --report_data_flattening $switches "--$args" \
        -o "$OUT/js-$name" "$OUT/js-$name.sml" > "$OUT/js-$name-build.log" 2>&1
      node "$ROOT/js/test/run-node.js" "$OUT/js-$name.html" > "$OUT/js-$name.log" 2>&1
      grep -q 'portable data flattening passed' "$OUT/js-$name.log"
    fi
    if [ -n "${BARRY:-}" ]; then
      cp "$TESTS/flatten-portable.sml" "$OUT/barry-$name.sml"
      (cd "$OUT" && SML_LIB=$ROOT "$BARRY" --report_data_flattening $switches "--$args" \
        "$OUT/barry-$name.sml") > "$OUT/barry-$name-build.log" 2>&1
      generated=$(find "$OUT/MLB" -name "barry-$name.sml.sml" -print)
      SML_LIB=$ROOT "$MLKIT" -o "$OUT/barry-$name" "$generated" > "$OUT/barry-$name-recompile.log" 2>&1
      "$OUT/barry-$name" > "$OUT/barry-$name.log" 2>&1
      grep -q 'portable data flattening passed' "$OUT/barry-$name.log"
      if [ "$args" = no_unbox_funargs ] && [ "$feature" != none ]; then
        grep -q 'data flattening: transformed' "$OUT/barry-$name-build.log"
      fi
    fi
    if [ -n "${MLKIT_X64:-}" ]; then
      # No Basis dependency is needed by the explicit-primitive fixture.
      # Cross assembly is sufficient on a non-X64 development host.
      for mode in no_gc gc gengc; do
        file=$OUT/x64-$name-$mode.sml
        printf 'fun print (s:string) : unit = prim("printStringML", s)\n' > "$file"
        cat "$TESTS/flatten-portable.sml" >> "$file"
        (cd "$OUT" && SML_LIB=$ROOT "$MLKIT_X64" -no_basislib -c \
          --report_data_flattening $switches "--$args" "--$mode" \
          --assembler "${X64_ASSEMBLER:-cc -c}" "$file") > "$OUT/x64-$name-$mode-build.log" 2>&1
      done
    fi
  done
done
echo 'Flattening backend checks passed'
