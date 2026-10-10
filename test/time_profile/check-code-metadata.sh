#!/bin/sh
# T2: native function tables, linked library retention, ASLR and offline lookup.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-code.XXXXXX")}
mkdir -p "$OUT"
echo "Code metadata artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
$CC -O2 -Wall -Wextra -Werror -DTP_LIBRARY -c "$ROOT/test/time_profile/feasibility.c" -o "$OUT/timer.o"
ar rcs "$OUT/libtptime.a" "$OUT/timer.o"
cp "$ROOT/test/time_profile/workload.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\nworkload.sml\n' > "$OUT/workload.mlb"
for mode in gc no_gc; do
  "$MLKIT" -no_par -"$mode" -rp -libdirs "$OUT" -libs tptime,m,c,dl \
    -o "$OUT/$mode" "$OUT/workload.mlb" > "$OUT/$mode-build.log" 2>&1
  nm -n "$OUT/$mode" > "$OUT/$mode-symbols.txt"
  for run in first second; do
    TP_MODE=wall TP_INTERVAL_US=1000 TP_SAMPLES="$OUT/$mode-$run.pcs" \
      "$OUT/$mode" ml +RTS -rp -rp_interval 0 -rp_file "$OUT/$mode-$run.rp" \
      > "$OUT/$mode-$run.log" 2>&1
    "$RPVIEW" "$OUT/$mode-$run.rp" --format json -o "$OUT/$mode-$run.json" > /dev/null
  done
  TP_MODE=wall TP_SAMPLES="$OUT/$mode-c.pcs" "$OUT/$mode" c +RTS -rp \
    -rp_interval 0 -rp_file "$OUT/$mode-c.rp" > "$OUT/$mode-c.log" 2>&1
  "$RPVIEW" "$OUT/$mode-c.rp" --format json -o "$OUT/$mode-c.json" > /dev/null
  if [ "$mode" = gc ]; then
    TP_MODE=wall TP_SAMPLES="$OUT/gc-allocation.pcs" "$OUT/gc" gc +RTS -rp \
      -rp_interval 0 -rp_file "$OUT/gc-allocation.rp" > "$OUT/gc-allocation.log" 2>&1
    "$RPVIEW" "$OUT/gc-allocation.rp" --format json -o "$OUT/gc-allocation.json" > /dev/null
  fi
  "$RPVIEW" "$OUT/$mode-first.rp" -o "$OUT/$mode.html" > /dev/null
  # One more link gives a different image identity and exercises mismatch checks.
  "$MLKIT" -no_par -"$mode" -rp -libdirs "$OUT" -libs tptime,m,c,dl \
    -o "$OUT/$mode-other" "$OUT/workload.mlb" > "$OUT/$mode-other-build.log" 2>&1
  TP_MODE=wall "$OUT/$mode-other" ml +RTS -rp -rp_interval 0 \
    -rp_file "$OUT/$mode-other.rp" > "$OUT/$mode-other.log" 2>&1
  "$RPVIEW" "$OUT/$mode-other.rp" --format json -o "$OUT/$mode-other.json" > /dev/null
done
printf ':quit;\n' | "$MLKIT" -no_gc -rp -rp_interval 0 -rp_file "$OUT/repl.rp" \
  > "$OUT/repl.log" 2>&1
"$RPVIEW" "$OUT/repl.rp" --format json -o "$OUT/repl.json" > /dev/null
node "$ROOT/test/time_profile/code-metadata-assertions.js" "$OUT" "$RPVIEW"
# Optional SML compiler runs exact-address and malformed-input unit checks.
if command -v mlton > /dev/null 2>&1; then
  mlton -output "$OUT/resolution" "$ROOT/test/time_profile/code-resolution.mlb"
  "$OUT/resolution"
fi
echo 'Function metadata and offline PC checks passed'
