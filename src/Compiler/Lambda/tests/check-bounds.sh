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
