#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
compiler=${MLKIT:-$root/bin/mlkit}
out=${BOUNDS_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-bounds.XXXXXX")}
mkdir -p "$out"
echo "Bounds test outputs: $out"
stage=setup
trap 'status=$?; if [ "$status" -ne 0 ]; then
  echo "Bounds test failed during: $stage (exit $status)" >&2
  echo "Compiler: $compiler; logs: $out" >&2
  for log in "$out"/*.log; do
    [ -f "$log" ] || continue
    echo "--- $log (last 60 lines) ---" >&2
    tail -n 60 "$log" >&2
  done
fi' 0
expect () {
  stage="expect $1 in $2"
  if ! grep -q "$1" "$2"; then
    echo "Missing expected pattern '$1' in $2" >&2
    exit 1
  fi
}
for mode in no_gc gc gengc; do
  for opt in optimiser no_optimiser; do
    name=$mode-$opt
    cp "$root/src/Compiler/Lambda/tests/bounds.sml" "$out/$name.sml"
    stage="compile $name"
    SML_LIB=$root "$compiler" -"$mode" -no_par --"$opt" --statistics_after_optimisation --print_opt_lambda_expression \
      -o "$out/$name" "$out/$name.sml" > "$out/$name-build.log" 2>&1
    stage="execute $name"
    "$out/$name" > "$out/$name.log" 2>&1
    expect '^bounds checks passed$' "$out/$name.log"
  done
  cp "$root/src/Compiler/Lambda/tests/bounds-char.sml" "$out/char-$mode.sml"
  printf '%s\n' '$(SML_LIB)/basis/basis.mlb' "char-$mode.sml" > "$out/char-$mode.mlb"
  stage="compile char-$mode"
  SML_LIB=$root "$compiler" -"$mode" -no_par -c --log_to_file \
    --statistics_after_optimisation --print_opt_lambda_expression \
    "$out/char-$mode.mlb" > "$out/char-$mode-build.log" 2>&1
  expect 'unsigned range-folding' "$out/char-$mode.sml.log"
  stage="check absence of Subscript in char-$mode"
  if grep -q 'Subscript' "$out/char-$mode.sml.log"; then
    echo 'A statically safe character access still has a bounds check' >&2
    exit 1
  fi
done
# Check that the native rewrite actually fires, as well as checking its result.
expect 'unsigned bounds check' "$out/no_gc-optimiser-build.log"
expect 'unsigned bounds check' "$out/gc-optimiser-build.log"
expect 'range-folding' "$out/no_gc-optimiser-build.log"
if [ -n "${SMLTOJS:-}" ]; then
  cp "$root/src/Compiler/Lambda/tests/bounds.sml" "$out/js.sml"
  stage="compile JavaScript"
  (cd "$out" && SML_LIB=$root/js "$SMLTOJS" -o "$out/js" "$out/js.sml") \
    > "$out/js-build.log" 2>&1
  stage="execute JavaScript"
  node "$root/js/test/run-node.js" "$out/js.html" > "$out/js.log" 2>&1
  expect '^bounds checks passed$' "$out/js.log"
fi
echo 'Bounds tests passed'
