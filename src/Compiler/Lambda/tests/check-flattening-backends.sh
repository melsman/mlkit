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
# The inliner must expose selections to reference primitives without relying
# on aggressive optimisation. Exercise RHS effects and non-forwarding wrappers.
for mode in no_gc gc gengc; do
  file=$OUT/inline-$mode.sml
  cp "$TESTS/inline-reference.sml" "$file"
  SML_LIB=$ROOT "$MLKIT" "--$mode" --no_aggressive_opt --no_unbox_funargs \
    --report_data_flattening -o "$OUT/inline-$mode" "$file" > "$OUT/inline-$mode-build.log" 2>&1
  "$OUT/inline-$mode" > "$OUT/inline-$mode.log" 2>&1
  grep -q 'selected reference inlining passed' "$OUT/inline-$mode.log"
  grep -q 'reference: eligible' "$OUT/inline-$mode-build.log"
  grep -q 'data flattening: transformed' "$OUT/inline-$mode-build.log"
  if [ -n "${MLKIT_X64:-}" ]; then
    file=$OUT/x64-inline-$mode.sml
    printf 'fun print (s:string) : unit = prim("printStringML", s)\n' > "$file"
    cat "$TESTS/inline-reference.sml" >> "$file"
    SML_LIB=$ROOT "$MLKIT_X64" -no_basislib -c "--$mode" --no_aggressive_opt \
      --no_unbox_funargs --report_data_flattening \
      --assembler "${X64_ASSEMBLER:-cc -c}" "$file" > "$OUT/x64-inline-$mode-build.log" 2>&1
    grep -q 'reference: eligible' "$OUT/x64-inline-$mode-build.log"
  fi
done
if [ -n "${SMLTOJS:-}" ]; then
  cp "$TESTS/inline-reference.sml" "$OUT/js-inline.sml"
  SML_LIB=$ROOT/js "$SMLTOJS" --no_aggressive_opt --no_unbox_funargs \
    --report_data_flattening -o "$OUT/js-inline" "$OUT/js-inline.sml" > "$OUT/js-inline-build.log" 2>&1
  node "$ROOT/js/test/run-node.js" "$OUT/js-inline.html" > "$OUT/js-inline.log" 2>&1
  grep -q 'selected reference inlining passed' "$OUT/js-inline.log"
  grep -q 'reference: eligible' "$OUT/js-inline-build.log"
fi
if [ -n "${BARRY:-}" ]; then
  cp "$TESTS/inline-reference.sml" "$OUT/barry-inline.sml"
  (cd "$OUT" && SML_LIB=$ROOT "$BARRY" --no_aggressive_opt --no_unbox_funargs \
    --report_data_flattening "$OUT/barry-inline.sml") > "$OUT/barry-inline-build.log" 2>&1
  generated=$(find "$OUT/MLB" -name barry-inline.sml.sml -print)
  SML_LIB=$ROOT "$MLKIT" -o "$OUT/barry-inline" "$generated" > "$OUT/barry-inline-recompile.log" 2>&1
  "$OUT/barry-inline" > "$OUT/barry-inline.log" 2>&1
  grep -q 'selected reference inlining passed' "$OUT/barry-inline.log"
  grep -q 'reference: eligible' "$OUT/barry-inline-build.log"
fi
echo 'Flattening backend checks passed'
