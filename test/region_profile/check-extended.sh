#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit-arm64}
CC=${CC:-cc}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rp-extended.XXXXXX")
echo "Extended profiler artifacts: $OUT"
export SML_LIB="$ROOT"
$CC -c "$ROOT/test/region_profile/periodic.c" -o "$OUT/periodic.o"
ar rcs "$OUT/libperiodic.a" "$OUT/periodic.o"
cp "$ROOT/test/region_profile/periodic.sml" "$OUT/"
printf '%s\n' "$OUT/periodic.sml" > "$OUT/periodic.mlb"
"$MLKIT" -no_gc -region_profile -libdirs "$OUT" -libs periodic -o "$OUT/periodic" "$OUT/periodic.mlb" > "$OUT/periodic.build" 2>&1
"$OUT/periodic" -rp -rp_interval 1ms -rp_report -rp_file "$OUT/periodic.rp" > "$OUT/periodic.out" 2> "$OUT/periodic.report"
grep -q 'periodic ok' "$OUT/periodic.out"
sh "$ROOT/test/region_profile/check-records.sh" periodic "$OUT/periodic.rp"
"$OUT/periodic" -rp -rp_paused -rp_file "$OUT/paused.rp" > /dev/null
! grep -q '"type":"sample_begin"' "$OUT/paused.rp"
for duration in -1 1 1.5ms 1us 999999999999999999999s; do
 if "$OUT/periodic" -rp -rp_interval "$duration" > "$OUT/invalid.out" 2>&1; then
  echo "Accepted invalid duration: $duration" >&2; exit 1
 fi
done
for mode in gc gengc parallel argobots; do
 case "$mode" in
  gc|gengc) source=gc; flags="-$mode"; runtime="-rp_gc_samples"; libs= ;;
  parallel) source=parallel; flags='-no_gc -par'; runtime=; libs= ;;
  argobots)
   if [ -z "${ARGOBOTS_ROOT:-}" ]; then echo 'Argobots skipped: set ARGOBOTS_ROOT'; continue; fi
   source=parallel; flags='-no_gc -par -argo'; runtime='-p 2'; libs="-libdirs $ARGOBOTS_ROOT/src/.libs -libs abt" ;;
 esac
 cp "$ROOT/test/region_profile/$source.sml" "$OUT/$mode.sml"
  printf '%s\n' "$ROOT/kitlib/region-profile.mlb" "$ROOT/basis/basis.mlb" > "$OUT/$mode.mlb"
 case "$mode" in parallel|argobots) printf '%s\n' "$ROOT/basis/par.mlb" >> "$OUT/$mode.mlb" ;; esac
 printf '%s\n' "$OUT/$mode.sml" >> "$OUT/$mode.mlb"
 "$MLKIT" $flags -region_profile $libs -o "$OUT/$mode" "$OUT/$mode.mlb" > "$OUT/$mode.build" 2>&1
 "$OUT/$mode" -rp -rp_interval 1ms -rp_report -rp_file "$OUT/$mode.rp" $runtime > "$OUT/$mode.out" 2> "$OUT/$mode.report"
 sh "$ROOT/test/region_profile/check-records.sh" "$mode" "$OUT/$mode.rp"
done
"$OUT/gengc" -rp -rp_interval 0 -rp_gc_samples -only_major_gc -rp_file "$OUT/gengc-major.rp" > "$OUT/gengc-major.out"
sh "$ROOT/test/region_profile/check-records.sh" gengc "$OUT/gengc-major.rp"
$CC -c "$ROOT/test/region_profile/foreign.c" -o "$OUT/foreign.o"
ar rcs "$OUT/libforeign.a" "$OUT/foreign.o" "$OUT/periodic.o"
cp "$ROOT/test/region_profile/foreign.sml" "$OUT/"
printf '%s\n' "$ROOT/basis/basis.mlb" "$ROOT/basis/par.mlb" "$OUT/foreign.sml" > "$OUT/foreign.mlb"
"$MLKIT" -no_gc -par -region_profile -libdirs "$OUT" -libs foreign -o "$OUT/foreign" "$OUT/foreign.mlb" > "$OUT/foreign.build" 2>&1
"$OUT/foreign" -rp -rp_interval 1ms -rp_report -rp_file "$OUT/foreign.rp" > "$OUT/foreign.out" 2> "$OUT/foreign.report"
grep -q 'foreign wait ok' "$OUT/foreign.out"
grep -q 'safe_point_timeout' "$OUT/foreign.rp"
"$RPVIEW" "$OUT/foreign.rp" --output "$OUT/foreign.html" > /dev/null
echo 'Blocked foreign call: cancellation, progress, and stream checks passed'
# Invalid REPL runtime options must fail promptly, rather than disappear or
# leave the compiler blocked opening a FIFO after its child exits.
status=0
printf ':quit\n' | sh "$ROOT/test/region_profile/with-timeout.sh" 30 "$MLKIT" -no_gc -region_profile -rp_paused > "$OUT/invalid-repl.log" 2>&1 || status=$?
[ "$status" -ne 124 ]
grep -q 'require -rp' "$OUT/invalid-repl.log"
for mode in no_gc gc; do
 mkdir "$OUT/repl-$mode"
 (cd "$OUT/repl-$mode" && "$MLKIT" -"$mode" -region_profile -rp -rp_interval 0 -rp_report -rp_file "$OUT/repl-$mode.rp" < "$ROOT/test/region_profile/repl.cmd" > "$OUT/repl-$mode.log" 2>&1)
 grep -q 'repl profile ok' "$OUT/repl-$mode.log"
 sh "$ROOT/test/region_profile/check-records.sh" repl "$OUT/repl-$mode.rp"
done
"$RPVIEW" "$OUT/parallel.rp" --output "$OUT/profile.html"
echo 'Extended profiler checks passed'
