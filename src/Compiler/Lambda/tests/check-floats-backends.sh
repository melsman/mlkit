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
    grep -q 'Quotients_flat of (raw f64 \* raw f64 \* raw f64)' "$OUT/$name-build.log"
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
      SML_LIB=$ROOT "$MLKIT" $option --unbox_reals "--$args" "--$mode" --report_data_flattening --report_datatype_representations \
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
  done
done
# Portable backends retain ordinary real arithmetic and reject unsupported flags.
for backend in js barry; do
  case "$backend" in
    js) compiler=${SMLTOJS:-}; basis=$ROOT/js; blocked='unbox_reals data_flattening_floats' ;;
    barry) compiler=${BARRY:-}; basis=$ROOT; blocked='unbox_reals data_flattening_floats data_flattening_refs' ;;
  esac
  [ -n "$compiler" ] || continue
  name=$backend-default
  cp "$TESTS/flatten-floats.sml" "$OUT/$name.sml"
  (cd "$OUT" && SML_LIB=$basis "$compiler" --report_data_flattening \
    -o "$OUT/$name" "$OUT/$name.sml") > "$OUT/$name-build.log" 2>&1
  if [ "$backend" = js ]; then
    node "$ROOT/js/test/run-node.js" "$OUT/$name.html" > "$OUT/$name.log" 2>&1
  else
    generated=$(find "$OUT/MLB" -name "$name.sml.sml" -print)
    SML_LIB=$ROOT "$MLKIT" -o "$OUT/$name" "$generated" > "$OUT/$name-recompile.log" 2>&1
    "$OUT/$name" > "$OUT/$name.log" 2>&1
  fi
  grep -q 'float data flattening passed' "$OUT/$name.log"
  if grep -q 'float: eligible' "$OUT/$name-build.log"; then
    echo "$backend unexpectedly enabled float flattening" >&2; exit 1
  fi
  for flag in $blocked; do
    for prefix in '' no_; do
      log=$OUT/$backend-blocked-$prefix$flag.log
      if SML_LIB=$basis "$compiler" "--$prefix$flag" "$OUT/$name.sml" > "$log" 2>&1; then
        echo "$backend accepted blocked option --$prefix$flag" >&2; exit 1
      fi
      grep -q 'option is blocked' "$log"
    done
  done
done
echo 'Float flattening backend checks passed'
