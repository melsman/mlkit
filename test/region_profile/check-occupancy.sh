#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
REML=${REML:-$ROOT/bin/reml}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-occupancy.XXXXXX")
echo "Occupancy checks: $OUT"
$CC -O2 -std=gnu99 -Wall -Wextra -Werror -iquote "$ROOT/src/Runtime" \
  "$ROOT/src/Runtime/RegionProfile.c" "$ROOT/src/Runtime/tests/allocation-profile.c" -o "$OUT/bindings"
"$OUT/bindings" "$OUT/bindings.rp"
selector () { sed -n 's/.*"binding":\([0-9]*\),"unit":"\([^"]*\)","name":"`r".*/\2:\1/p' "$1" | head -1; }
$CC -c "$ROOT/test/region_profile/allocation.c" -o "$OUT/allocation.o"
ar rcs "$OUT/libfixture.a" "$OUT/allocation.o"
for kind in reset large; do
  case "$kind" in reset) file=allocation;; large) file=allocation-string;; esac
  cp "$ROOT/test/region_profile/$file.sml" "$OUT/$kind.sml"
  printf '%s\n' "$kind.sml" > "$OUT/$kind.mlb"
  "$REML" -no_par -rp -libdirs "$OUT" -libs fixture -o "$OUT/$kind" "$OUT/$kind.mlb" > "$OUT/$kind.build" 2>&1
  "$OUT/$kind" -rp -rp_interval 0 -rp_file "$OUT/discovery.rp"
  "$RPVIEW" "$OUT/discovery.rp" --format json > "$OUT/discovery.json"
  selected=$(selector "$OUT/discovery.json")
  test -n "$selected"
  "$OUT/$kind" -rp -rp_interval 0 -rp_region "$selected" -rp_file "$OUT/$kind.rp"
  "$RPVIEW" "$OUT/$kind.rp" --format json > "$OUT/$kind.json"
  node "$ROOT/test/region_profile/occupancy-assertions.js" "$OUT/$kind.json" "$kind"
  "$RPVIEW" "$OUT/$kind.rp" -o "$OUT/$kind.html"
done
{
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/reset.html" | sed '1d;$d'
  cat "$ROOT/test/region_profile/occupancy-viewer-assertions.js"
  cat "$ROOT/test/region_profile/site-graph-assertions.js"
} > "$OUT/viewer.js"
node "$OUT/viewer.js"
sh "$ROOT/test/region_profile/check-occupancy-reader.sh" "$OUT"
cp "$ROOT/test/region_profile/allocation-gc.sml" "$OUT/gc.sml"
printf '%s\n' gc.sml > "$OUT/gc.mlb"
for collector in gc gengc tagged; do
  case "$collector" in gc) set -- -gc;; gengc) set -- -gengc;; tagged) set -- -gc -tag_pairs;; esac
  "$MLKIT" "$@" -rp -o "$OUT/$collector" "$OUT/gc.mlb" > "$OUT/$collector.build" 2>&1
  "$OUT/$collector" -rp -rp_interval 0 -rp_region '<global>:4' -rp_file "$OUT/$collector.rp"
  "$RPVIEW" "$OUT/$collector.rp" --format json > "$OUT/$collector.json"
  node "$ROOT/test/region_profile/occupancy-assertions.js" "$OUT/$collector.json" "$collector"
done

$CC -DPROFILING -iquote "$ROOT/src/Runtime" -c "$ROOT/test/region_profile/allocation-callback.c" -o "$OUT/callback.o"
ar rcs "$OUT/libcallback.a" "$OUT/callback.o"
cp "$ROOT/test/region_profile/allocation-callback.sml" "$OUT/callback.sml"
printf '%s\n' callback.sml > "$OUT/callback.mlb"
"$REML" -no_par -rp -libdirs "$OUT" -libs callback -o "$OUT/callback" "$OUT/callback.mlb" > "$OUT/callback.build" 2>&1
"$OUT/callback" -rp -rp_interval 0 -rp_file "$OUT/discovery.rp"
"$RPVIEW" "$OUT/discovery.rp" --format json > "$OUT/discovery.json"
selected=$(selector "$OUT/discovery.json")
test -n "$selected"
"$OUT/callback" -rp -rp_interval 0 -rp_region "$selected" -rp_file "$OUT/callback.rp"
"$RPVIEW" "$OUT/callback.rp" --format json > "$OUT/callback.json"
node "$ROOT/test/region_profile/occupancy-assertions.js" "$OUT/callback.json" callback
sh "$ROOT/test/region_profile/check-site-svg.sh" "$OUT"

cp "$ROOT/test/region_profile/allocation-parallel.sml" "$OUT/parallel.sml"
printf '%s\n' "$SML_LIB/basis/basis.mlb" "$SML_LIB/basis/par.mlb" parallel.sml > "$OUT/parallel.mlb"
"$MLKIT" -no_gc -par -rp -o "$OUT/parallel" "$OUT/parallel.mlb" > "$OUT/parallel.build" 2>&1
"$OUT/parallel" -rp -rp_interval 0 -rp_region '<global>:5' -rp_file "$OUT/parallel.rp"
"$RPVIEW" "$OUT/parallel.rp" --format json > "$OUT/parallel.json"
node "$ROOT/test/region_profile/occupancy-assertions.js" "$OUT/parallel.json" parallel
