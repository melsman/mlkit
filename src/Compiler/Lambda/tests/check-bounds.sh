#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../.." && pwd)
compiler=${MLKIT:-$root/bin/mlkit}
out=${BOUNDS_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-bounds.XXXXXX")}
mkdir -p "$out"
echo "Bounds test outputs: $out"
for mode in no_gc gc gengc; do
  for opt in optimiser no_optimiser; do
    name=$mode-$opt
    cp "$root/src/Compiler/Lambda/tests/bounds.sml" "$out/$name.sml"
    SML_LIB=$root "$compiler" -"$mode" -no_par --"$opt" --statistics_after_optimisation --print_opt_lambda_expression \
      -o "$out/$name" "$out/$name.sml" > "$out/$name-build.log" 2>&1
    "$out/$name" > "$out/$name.log"
    grep -qx 'bounds checks passed' "$out/$name.log"
  done
  cp "$root/src/Compiler/Lambda/tests/bounds-char.sml" "$out/char-$mode.sml"
  printf '%s\n' '$(SML_LIB)/basis/basis.mlb' "char-$mode.sml" > "$out/char-$mode.mlb"
  SML_LIB=$root "$compiler" -"$mode" -no_par -c --log_to_file \
    --statistics_after_optimisation --print_opt_lambda_expression \
    "$out/char-$mode.mlb" > "$out/char-$mode-build.log" 2>&1
  grep -q 'unsigned range-folding' "$out/char-$mode.sml.log"
  if grep -q 'Subscript' "$out/char-$mode.sml.log"; then
    echo 'A statically safe character access still has a bounds check' >&2
    exit 1
  fi
done
# Check that the native rewrite actually fires, as well as checking its result.
grep -q 'unsigned bounds check' "$out/no_gc-optimiser-build.log"
grep -q 'unsigned bounds check' "$out/gc-optimiser-build.log"
grep -q 'range-folding' "$out/no_gc-optimiser-build.log"
if [ -n "${SMLTOJS:-}" ]; then
  cp "$root/src/Compiler/Lambda/tests/bounds.sml" "$out/js.sml"
  (cd "$out" && SML_LIB=$root/js "$SMLTOJS" -o "$out/js" "$out/js.sml") \
    > "$out/js-build.log" 2>&1
  node "$root/js/test/run-node.js" "$out/js.html" > "$out/js.log"
  grep -qx 'bounds checks passed' "$out/js.log"
fi
echo 'Bounds tests passed'
