#!/bin/sh
# Check both selection and semantics; ordinary real arithmetic alone is not
# evidence that a record was flattened. Optional backends follow the other
# Lambda regression runners.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
TESTS=$ROOT/src/Compiler/Lambda/tests
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${FLATTEN_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-floats.XXXXXX")}
mkdir -p "$OUT"
echo "Float flattening outputs: $OUT"
verify () {
  grep -q 'float data flattening passed' "$OUT/$name.log"
  if [ "$setting" != off ] && [ "$args" = no_unbox_funargs ]; then
    grep -q 'float: eligible' "$OUT/$name-build.log"
    grep -q 'data flattening: transformed' "$OUT/$name-build.log"
  fi
  if [ "$setting" = off ] && grep -q 'float: eligible' "$OUT/$name-build.log"; then
    echo 'Disabled float flattening found an eligible candidate' >&2; exit 1
  fi
}
# The optional switch is deliberately unquoted: default supplies no argument.
for setting in default on off; do
  case "$setting" in default) option= ;; on) option=--data_flattening_floats ;; off) option=--no_data_flattening_floats ;; esac
  for args in unbox_funargs no_unbox_funargs; do
    for mode in no_gc gc gengc; do
      name=native-$setting-$args-$mode
      cp "$TESTS/flatten-floats.sml" "$OUT/$name.sml"
      SML_LIB=$ROOT "$MLKIT" $option --unbox_reals "--$args" "--$mode" --report_data_flattening \
        -o "$OUT/$name" "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
      "$OUT/$name" > "$OUT/$name.log" 2>&1
      verify
      if [ -n "${MLKIT_X64:-}" ]; then
        name=x64-$setting-$args-$mode
        printf 'fun print (s:string) : unit = prim("printStringML",s)\n' > "$OUT/$name.sml"
        cat "$TESTS/flatten-floats.sml" >> "$OUT/$name.sml"
        SML_LIB=$ROOT "$MLKIT_X64" -no_basislib -c $option --unbox_reals "--$args" "--$mode" \
          --report_data_flattening --assembler "${X64_ASSEMBLER:-cc -c}" \
          "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
        if [ "$setting" != off ] && [ "$args" = no_unbox_funargs ]; then
          grep -q 'float: eligible' "$OUT/$name-build.log"
        fi
      fi
    done
    if [ -n "${SMLTOJS:-}" ]; then
      name=js-$setting-$args
      cp "$TESTS/flatten-floats.sml" "$OUT/$name.sml"
      SML_LIB=$ROOT/js "$SMLTOJS" $option --unbox_reals "--$args" --report_data_flattening \
        -o "$OUT/$name" "$OUT/$name.sml" > "$OUT/$name-build.log" 2>&1
      node "$ROOT/js/test/run-node.js" "$OUT/$name.html" > "$OUT/$name.log" 2>&1
      verify
    fi

  done
  if [ -n "${BARRY:-}" ]; then
    name=barry-$setting
    cp "$TESTS/flatten-floats.sml" "$OUT/$name.sml"
    (cd "$OUT" && SML_LIB=$ROOT "$BARRY" $option --report_data_flattening \
      "$OUT/$name.sml") > "$OUT/$name-build.log" 2>&1
    generated=$(find "$OUT/MLB" -name "$name.sml.sml" -print)
    SML_LIB=$ROOT "$MLKIT" -o "$OUT/$name" "$generated" > "$OUT/$name-recompile.log" 2>&1
    "$OUT/$name" > "$OUT/$name.log" 2>&1
    grep -q 'float data flattening passed' "$OUT/$name.log"
  fi
done
echo 'Float flattening backend checks passed'
