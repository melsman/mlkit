#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rp-invocations.XXXXXX")
echo "Invocation profiler artifacts: $OUT"
export SML_LIB=${SML_LIB:-$ROOT}
$CC -c "$ROOT/test/region_profile/periodic.c" -o "$OUT/periodic.o"
ar rcs "$OUT/libperiodic.a" "$OUT/periodic.o"
cp "$ROOT/test/region_profile/periodic.sml" "$OUT/"
printf '%s\n' "$OUT/periodic.sml" > "$OUT/periodic.mlb"
"$MLKIT" -no_gc -rp -libdirs "$OUT" -libs periodic,m,c,dl -o "$OUT/run" "$OUT/periodic.mlb" > "$OUT/build.log" 2>&1
samples() {
  RP_ITERATIONS=$1 "$OUT/run" +RTS -rp -rp_interval "$2" -rp_file "$OUT/count.rp" -RTS > "$OUT/run.out"
  grep -q 'periodic ok' "$OUT/run.out"
  "$RPVIEW" "$OUT/count.rp" --format json -o "$OUT/count.json" > /dev/null
  awk '/"type":"sample_begin"/ { n++ } END { print n+0 }' "$OUT/count.json"
}
# Extra loop iterations must produce exact additional entry samples, including
# the backend's optimized self-tail-call path. No timing assumptions are needed.
a=$(samples 79999 8000i)
b=$(samples 159999 8000i)
[ "$a" -gt 0 ] && [ "$((b-a))" -eq 10 ]
a=$(samples 100 1i)
b=$(samples 200 1i)
[ "$((b-a))" -eq 100 ]
[ "$(samples 200 0)" -eq 0 ]
RP_ITERATIONS=200 "$OUT/run" +RTS -rp -rp_paused -rp_interval 1i -rp_file "$OUT/paused.rp" -RTS > /dev/null
"$RPVIEW" "$OUT/paused.rp" --format json -o "$OUT/paused.json" > /dev/null
! grep -q '"type":"sample_begin"' "$OUT/paused.json"
for interval in 0i -1i 1.5i 8000ijunk 18446744073709551616i; do
  if "$OUT/run" +RTS -rp -rp_interval "$interval" -RTS > "$OUT/invalid.log" 2>&1; then
    echo "Accepted invalid interval: $interval" >&2; exit 1
  fi
done
"$OUT/run" +RTS -help > "$OUT/help.txt" 2>&1
grep -q '^Region profiling:' "$OUT/help.txt"
grep -q 'every N ML entries per thread' "$OUT/help.txt"
echo 'Invocation sampling checks passed'
