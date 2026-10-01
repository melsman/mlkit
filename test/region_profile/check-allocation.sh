#!/bin/sh
# Native M1-M3 checks. CC/MLKIT/REML may be wrappers for a target architecture.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
REML=${REML:-$ROOT/bin/reml}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-allocation.XXXXXX")
echo "Allocation checks: $OUT"
export SML_LIB=${SML_LIB:-$ROOT}
json () { "$RPVIEW" "$1" --format json > "$2"; }
selector () { sed -n 's/.*"binding":\([0-9]*\),"unit":"\([^"]*\)","name":"`r".*/\2:\1/p' "$1" | head -1; }
$CC -O2 -std=gnu99 -Wall -Wextra -Werror -iquote "$ROOT/src/Runtime" \
  "$ROOT/src/Runtime/RegionProfile.c" "$ROOT/src/Runtime/tests/allocation-profile.c" -o "$OUT/runtime"
"$OUT/runtime" "$OUT/runtime.rp"
json "$OUT/runtime.rp" "$OUT/runtime.json"
for expected in '"count":1,"bytes":64' '"count":2,"bytes":56' '"count":3,"bytes":128' '"count":1,"bytes":32'; do
  grep -q "$expected" "$OUT/runtime.json"
done
for name in allocation allocation-callback; do
  cp "$ROOT/test/region_profile/$name.sml" "$OUT/"
  printf '%s\n' "$name.sml" > "$OUT/$name.mlb"
  $CC -iquote "$ROOT/src/Runtime" -c "$ROOT/test/region_profile/$name.c" -o "$OUT/$name.o"
  ar rcs "$OUT/libfixture.a" "$OUT/$name.o"
  "$REML" -no_par -rp -allocation_profile -libdirs "$OUT" -libs fixture -o "$OUT/$name" "$OUT/$name.mlb" > "$OUT/$name.build" 2>&1
  "$OUT/$name" -rp -rp_interval 0 -rp_file "$OUT/discovery.rp"
  json "$OUT/discovery.rp" "$OUT/discovery.json"
  selected=$(selector "$OUT/discovery.json")
  [ -n "$selected" ]
  "$OUT/$name" -rp -rp_interval 0 -rp_region "$selected" -rp_file "$OUT/$name.rp"
  json "$OUT/$name.rp" "$OUT/$name.json"
  if [ "$name" = allocation ]; then
    grep -q '"count":1010,"bytes":16160' "$OUT/$name.json"
    [ "$(grep -c '"type":"allocation"' "$OUT/$name.json")" -eq 1 ]
  else
    grep -q '"count":1,"bytes":40' "$OUT/$name.json"
    grep -q '"count":3,"bytes":40' "$OUT/$name.json"
    [ "$(grep -c '"type":"allocation"' "$OUT/$name.json")" -eq 2 ]
  fi
  "$OUT/$name" -rp -rp_interval 0 -rp_region '<global>:3' -rp_file "$OUT/untracked.rp"
  json "$OUT/untracked.rp" "$OUT/untracked.json"
  ! grep -q '"type":"allocation"' "$OUT/untracked.json"
  "$RPVIEW" "$OUT/$name.rp" -o "$OUT/$name.html"
  grep -q 'allocation-section' "$OUT/$name.html"
  for depth in 2 4 8; do
    if "$OUT/$name" -rp -rp_alloc_depth "$depth" > "$OUT/invalid.log" 2>&1; then exit 1; fi
    grep -q 'only allocation depth 1' "$OUT/invalid.log"
  done
  if "$OUT/$name" -rp -rp_build incorrect > "$OUT/invalid.log" 2>&1; then exit 1; fi
  grep -q 'build identifier does not match' "$OUT/invalid.log"
  rm "$OUT/libfixture.a"
done
cp "$ROOT/test/region_profile/allocation-string.sml" "$OUT/"
printf '%s\n' allocation-string.sml > "$OUT/string.mlb"
"$REML" -no_par -rp -allocation_profile -o "$OUT/string" "$OUT/string.mlb" > "$OUT/string.build" 2>&1
"$OUT/string" -rp -rp_interval 0 -rp_file "$OUT/string-discovery.rp"
json "$OUT/string-discovery.rp" "$OUT/string-discovery.json"
selected=$(selector "$OUT/string-discovery.json")
[ -n "$selected" ]
"$OUT/string" -rp -rp_interval 0 -rp_region "$selected" -rp_file "$OUT/string.rp"
json "$OUT/string.rp" "$OUT/string.json"
grep -q '"count":2,"bytes":9048' "$OUT/string.json"
cp "$ROOT/test/region_profile/allocation-gc.sml" "$OUT/"
printf '%s\n' allocation-gc.sml > "$OUT/gc.mlb"
for collector in -gc -gengc; do
  "$MLKIT" "$collector" -rp -allocation_profile -o "$OUT/gc" "$OUT/gc.mlb" > "$OUT/gc$collector.build" 2>&1
  for state in enabled disabled; do
    set --
    [ "$state" = enabled ] || set -- -disable_gc
    "$OUT/gc" "$@" -rp -rp_interval 0 -rp_region '<global>:4' -rp_file "$OUT/gc$collector-$state.rp"
    json "$OUT/gc$collector-$state.rp" "$OUT/gc$collector-$state.json"
    sh "$ROOT/test/region_profile/check-global-ids.sh" "$OUT/gc$collector-$state.json"
    grep -q '"count":100000,"bytes":1600000' "$OUT/gc$collector-$state.json"
  done
  grep '"type":"session_end"' "$OUT/gc$collector-enabled.json" | grep -Eq '"gc_collections":[1-9][0-9]*'
done
cp "$ROOT/test/region_profile/allocation-parallel.sml" "$OUT/"
printf '%s\n' '$(SML_LIB)/basis/basis.mlb' '$(SML_LIB)/basis/par.mlb' allocation-parallel.sml > "$OUT/parallel.mlb"
"$MLKIT" -no_gc -par -rp -allocation_profile -o "$OUT/parallel" "$OUT/parallel.mlb" > "$OUT/parallel.build" 2>&1
"$OUT/parallel" -rp -rp_interval 0 -rp_file "$OUT/parallel-discovery.rp"
json "$OUT/parallel-discovery.rp" "$OUT/parallel-discovery.json"
selected=$(sed -n 's/.*"binding":\([0-9]*\),"unit":"\([^"]*\)".*"source":".*allocation-parallel.sml","kind":"infinite","region_type":"array".*/\2:\1/p' "$OUT/parallel-discovery.json" | head -1)
[ -n "$selected" ]
"$OUT/parallel" -rp -rp_interval 0 -rp_region "$selected" -rp_file "$OUT/parallel.rp"
json "$OUT/parallel.rp" "$OUT/parallel.json"
[ "$(grep -c '"count":1000,"bytes":32000' "$OUT/parallel.json")" -eq 4 ]
[ "$(grep -c '"type":"allocation_site"' "$OUT/parallel.json")" -eq 1 ]
! grep -q '"type":"allocation_incomplete"' "$OUT/parallel.json"
echo 'Allocation counters, native inline/C paths, reset, callbacks, exceptions, GC exclusion, pthreads and viewer checks passed'
