#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit-arm64}
REML=${REML:-$ROOT/bin/reml-arm64}
CC=${CC:-cc}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rp.XXXXXX")
echo "Region profiler test artifacts: $OUT"
export SML_LIB="$ROOT"
# CC may include a target flag, e.g. 'gcc -arch x86_64'.
$CC -O2 -std=gnu99 -Wall -Wextra -Werror -iquote "$ROOT/src/Runtime" \
  "$ROOT/src/Runtime/RegionProfile.c" "$ROOT/src/Runtime/tests/region-profile.c" -o "$OUT/runtime"
"$OUT/runtime" "$OUT/runtime.rp"
sh "$ROOT/test/region_profile/check-records.sh" runtime "$OUT/runtime.rp"
$CC -c "$ROOT/test/region_profile/fixture.c" -o "$OUT/fixture.o"
ar rcs "$OUT/librpfixture.a" "$OUT/fixture.o"
# Copy sources so each run compiles fresh metadata without clearing user caches.
cp "$ROOT/test/region_profile/regions.sml" "$ROOT/test/region_profile/regions.mlb" "$OUT/"
"$REML" -no_par -region_profile -libdirs "$OUT" -libs rpfixture -o "$OUT/regions" "$OUT/regions.mlb" > "$OUT/regions.build" 2>&1
"$OUT/regions" -rp -rp_file "$OUT/regions.rp"
sh "$ROOT/test/region_profile/check-records.sh" regions "$OUT/regions.rp"
cp "$ROOT/test/region_profile/basic.sml" "$ROOT/test/region_profile/basic.mlb" "$OUT/"
"$MLKIT" -no_gc -o "$OUT/plain" "$OUT/basic.mlb" > "$OUT/plain.build" 2>&1
"$OUT/plain" > "$OUT/plain.out"
if "$OUT/plain" -rp > "$OUT/no-metadata.out" 2>&1; then
  echo 'Executable without metadata accepted -rp' >&2; exit 1
fi
grep -q 'recompile' "$OUT/no-metadata.out"
for args in '-rp_file' '-rp_file profile.rp' '-rp_paused' '-rp_interval 10ms'; do
  if "$OUT/plain" $args > "$OUT/invalid.out" 2>&1; then
    echo "Invalid profiler options accepted: $args" >&2; exit 1
  fi
done
# A callback boundary is diagnosed rather than interpreted as an ML caller.
$CC -c "$ROOT/test/region_profile/callback.c" -o "$OUT/callback.o"
ar rcs "$OUT/librpcallback.a" "$OUT/callback.o"
cp "$ROOT/test/region_profile/callback.sml" "$OUT/"
printf '%s\n' "$OUT/callback.sml" > "$OUT/callback.mlb"
"$MLKIT" -no_gc -region_profile -libdirs "$OUT" -libs rpcallback -o "$OUT/callback" "$OUT/callback.mlb" > "$OUT/callback.build" 2>&1
"$OUT/callback"
if "$OUT/callback" -rp -rp_file "$OUT/callback.rp" > "$OUT/callback.out" 2>&1; then
  echo 'Sampling across a C callback unexpectedly succeeded' >&2; exit 1
fi
grep -q 'cannot sample across a C-to-ML callback boundary' "$OUT/callback.out"
# Public API and a fully instrumented Basis use a separate cache variant.
cp "$ROOT/test/region_profile/api.sml" "$OUT/"
printf '%s\n' "$ROOT/kitlib/region-profile.mlb" "$ROOT/basis/basis.mlb" "$OUT/api.sml" > "$OUT/api.mlb"
"$MLKIT" -no_gc -region_profile -o "$OUT/api" "$OUT/api.mlb" > "$OUT/api.build" 2>&1
"$OUT/api" -rp -rp_paused -rp_file "$OUT/api.rp" -- first -rp application > "$OUT/api.out"
grep -qx 'first:-rp:application' "$OUT/api.out"
sh "$ROOT/test/region_profile/check-records.sh" api "$OUT/api.rp"
(cd "$OUT" && ./api -- disabled > disabled.out && test ! -e profile.rp)
cp "$ROOT/test/region_profile/graph.sml" "$OUT/"
printf '%s\n' "$OUT/graph.sml" > "$OUT/graph.mlb"
"$REML" -no_par -region_profile -o "$OUT/graph" "$OUT/graph.mlb" > "$OUT/graph.build" 2>&1
"$OUT/graph" -rp -rp_interval 0 -rp_file "$OUT/graph.rp" > "$OUT/graph.out"
sh "$ROOT/test/region_profile/check-records.sh" graph "$OUT/graph.rp"
"$RPVIEW" "$OUT/graph.rp" --output "$OUT/graph.html"
echo 'Region profiler accounting and graph example checks passed'
