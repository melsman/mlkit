#!/bin/sh
# Shared lowering check; ABI execution coverage belongs to the backend tests.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-real-auto-ir.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$OUT"/*.log; fi; rm -rf "$OUT"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
for mode in plain gc profiling; do
  case "$mode" in
    plain) flags=-no_gc ;;
    gc) flags=-gc ;;
    profiling) flags='-gc -rp' ;;
  esac
  cp "$ROOT/test/real_auto_conversion/ir.sml" "$OUT/$mode.sml"
  # Both native backends must compile without linking test symbols.
  "$MLKIT" $flags --no_basislib -c -no_opt -Pole -Prsp -Prip -Pdre -Pccp \
    --print_linearised_program -Ptypes "$OUT/$mode.sml" > "$OUT/$mode.log" 2>&1
  grep -q 'Optimised Lambda Program' "$OUT/$mode.log"
  grep -q 'Float64' "$OUT/$mode.log"
  grep -q 'f64_to_real\|__f64_to_real' "$OUT/$mode.log"
  grep -q 'real_to_f64\|__real_to_f64' "$OUT/$mode.log"
  grep -q 'AFTER LINEARISATION' "$OUT/$mode.log"
  # Region IR must give the inner call an unboxed result without result
  # storage. The outer conversion owns real allocation.
  grep -q 'ccall(@m2_mixed,.*:f64\[\]' "$OUT/$mode.log"
  grep -q 'ccall(__f64_to_real,.*m2_produce.*\[ at ' "$OUT/$mode.log"
  grep -q 'R64.fromF64 \[at .*m2_mixed' "$OUT/$mode.log"
  awk '/AFTER LINEARISATION/ { emit = 1 } emit' "$OUT/$mode.log" > "$OUT/$mode.linear"
  grep -q ':Float64 = ccall_auto("m2_mixed", <.*:Float64,.*:Int>)' "$OUT/$mode.linear"
  grep -q ':Float64 = ccall_auto("m2_produce", <>)' "$OUT/$mode.linear"
  grep -q ':Int = ccall_auto("m2_consume", <.*:Float64>)' "$OUT/$mode.linear"
  grep -q ':Unit = ccall_auto("m2_discard", <.*:Float64>)' "$OUT/$mode.linear"
  grep -q 'ccall("m2_raw", <' "$OUT/$mode.linear"
  if grep 'ccall("m2_raw", <' "$OUT/$mode.linear" | grep -q Float64; then
    echo 'Ordinary real call was converted' >&2
    exit 1
  fi
  for name in mixed consume produce discard raw; do
    grep -q "m2_$name" "$OUT/$mode.log"
  done
  echo "Real auto-conversion IR: $mode passed"
done
cp "$ROOT/test/real_auto_conversion/explicit.sml" "$OUT/explicit.sml"
"$MLKIT" --reml -no_gc --no_basislib -c -no_opt -Pole -Prsp -Pdre \
  --print_linearised_program -Ptypes "$OUT/explicit.sml" > "$OUT/explicit.log" 2>&1
grep -q '):(real,`r)\[ at `r\]' "$OUT/explicit.log"
if grep -q '\[ at `r, at `r\]' "$OUT/explicit.log"; then
  echo 'Boxing received both an explicit region pointer and result storage' >&2
  exit 1
fi
grep -q ':Float64 = ccall_auto("m2_explicit", <.*:Float64>)' "$OUT/explicit.log"
grep -q 'prim("F64_to_real", <[^,]*,[^,]*>)' "$OUT/explicit.log"
echo 'Real auto-conversion IR: explicit region passed'
