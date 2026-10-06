#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
REML=${REML:-$ROOT/bin/reml}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-all-regions.XXXXXX")
echo "All-region checks: $OUT"
$CC -c "$ROOT/test/region_profile/allocation.c" -o "$OUT/fixture.o"
ar rcs "$OUT/libfixture.a" "$OUT/fixture.o"
cp "$ROOT/test/region_profile/all-regions.sml" "$OUT/main.sml"
printf '%s\n' main.sml > "$OUT/main.mlb"
"$REML" -no_par -rp -libdirs "$OUT" -libs fixture -o "$OUT/program" "$OUT/main.mlb" > "$OUT/build.log" 2>&1
"$OUT/program" -rp -rp_interval 0 -rp_region all -rp_file "$OUT/all.rp"
"$RPVIEW" "$OUT/all.rp" --format json -o "$OUT/all.json"
"$RPVIEW" "$OUT/all.rp" -o "$OUT/all.html"
"$RPVIEW" "$OUT/all.rp" --sites -o "$OUT/all.svg"
grep -q 'Site contributions across all regions' "$OUT/all.svg"
node "$ROOT/test/region_profile/all-regions-assertions.js" "$OUT/all.json"
{
 cat "$ROOT/test/region_profile/graph-prelude.js"
 sed -n '/^<script>$/,/^<\/script>/p' "$OUT/all.html" | sed '1d;$d'
 cat "$ROOT/test/region_profile/all-regions-viewer-assertions.js"
} > "$OUT/viewer.js"
node "$OUT/viewer.js"
# Check every recorded infinite region, including empty globals and GC generations.
cp "$ROOT/test/region_profile/allocation-gc.sml" "$OUT/gc.sml"
printf '%s\n' gc.sml > "$OUT/gc.mlb"
for mode in gc gengc; do
 "$MLKIT" "-$mode" -rp -o "$OUT/$mode" "$OUT/gc.mlb" > "$OUT/$mode.build" 2>&1
 "$OUT/$mode" -rp -rp_interval 0 -rp_region all -rp_file "$OUT/$mode.rp"
 "$RPVIEW" "$OUT/$mode.rp" --format json -o "$OUT/$mode.json"
 node "$ROOT/test/region_profile/all-regions-assertions.js" "$OUT/$mode.json" general
 done
cp "$ROOT/test/region_profile/allocation-parallel.sml" "$OUT/parallel.sml"
printf '%s\n' "$SML_LIB/basis/basis.mlb" "$SML_LIB/basis/par.mlb" parallel.sml > "$OUT/parallel.mlb"
"$MLKIT" -no_gc -par -rp -o "$OUT/parallel" "$OUT/parallel.mlb" > "$OUT/parallel.build" 2>&1
"$OUT/parallel" -rp -rp_interval 0 -rp_region all -rp_file "$OUT/parallel.rp"
"$RPVIEW" "$OUT/parallel.rp" --format json -o "$OUT/parallel.json"
node "$ROOT/test/region_profile/all-regions-assertions.js" "$OUT/parallel.json" general
