#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time.XXXXXX")}
mkdir -p "$OUT"
echo "Artifacts: $OUT"
CC=${CC:-cc}
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
$CC -O2 -Wall -Wextra -Werror "$ROOT/test/time_profile/feasibility.c" -o "$OUT/timer"
$CC -O2 -Wall -Wextra -Werror -DTP_LIBRARY -c "$ROOT/test/time_profile/feasibility.c" -o "$OUT/timer.o"
ar rcs "$OUT/libtptime.a" "$OUT/timer.o"
cp "$ROOT/test/time_profile/workload.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\nworkload.sml\n' > "$OUT/workload.mlb"
"$MLKIT" -no_par -gc -libdirs "$OUT" -libs tptime,m,c,dl -o "$OUT/workload" "$OUT/workload.mlb" > "$OUT/build.log" 2>&1
: > "$OUT/results.txt"
for mode in wall user cpu; do
  for interval in 100 1000 10000; do
    for phase in busy sleep read blocked system; do
      printf 'standalone phase=%s ' "$phase" >> "$OUT/results.txt"
      TP_MODE=$mode TP_INTERVAL_US=$interval "$OUT/timer" "$phase" >> "$OUT/results.txt"
    done
  done
  for phase in ml gc c sleep; do
    printf 'mlkit phase=%s\n' "$phase" >> "$OUT/results.txt"
    TP_MODE=$mode TP_INTERVAL_US=1000 TP_SAMPLES="$OUT/$mode-$phase.pcs" \
      "$OUT/workload" "$phase" +RTS -report_gc >> "$OUT/results.txt" 2>> "$OUT/gc.log"
  done
done
cat "$OUT/results.txt"
nm -n "$OUT/workload" > "$OUT/symbols.txt"
# Exercise ownership against the actual region profiler, rather than replacing it.
"$MLKIT" -no_par -gc -region_profile -libdirs "$OUT" -libs tptime,m,c,dl \
  -o "$OUT/with-region" "$OUT/workload.mlb" > "$OUT/region-build.log" 2>&1
if TP_MODE=wall "$OUT/with-region" c +RTS -rp -rp_interval 1ms \
    -rp_file "$OUT/conflict.rp" > "$OUT/conflict.log" 2>&1; then
  echo 'Wall timer unexpectedly replaced region timer' >&2; exit 1
fi
rg -q 'timer/signal already owned' "$OUT/conflict.log"
TP_MODE=cpu "$OUT/with-region" c +RTS -rp -rp_interval 1ms \
  -rp_file "$OUT/coexist.rp" > "$OUT/coexist.log" 2>&1
# Check qualitative behavior, without imposing exact statistical sample counts.
awk '
/standalone/ {
  for (key in field) delete field[key]
  for (i=1; i<=NF; i++) { split($i,pair,"="); field[pair[1]]=pair[2] }
  if (field["phase"] == "busy" && field["samples"] == 0) bad=1
  if (field["mode"] == "wall" && field["phase"] == "read" && field["samples"] == 0) bad=1
  if (field["mode"] != "wall" && (field["phase"] == "sleep" || field["phase"] == "read") && field["samples"] > 5) bad=1
  if (field["phase"] == "blocked" && field["samples"] > 5) bad=1
}
END { exit bad }
' "$OUT/results.txt"
rg -q ' F\.' "$OUT/wall-ml.pcs"
rg -q ' (gc|evacuate|allocGen)$' "$OUT/wall-gc.pcs"
rg -q ' tp_busy$' "$OUT/wall-c.pcs"
echo 'PC, clock behavior, GC, and timer ownership checks passed'
