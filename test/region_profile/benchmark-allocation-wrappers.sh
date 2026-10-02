#!/bin/sh
# Compare compilers before/after a wrapper change, with attribution disabled.
# Both compilers must use the same runtime ABI. No CI timing threshold.
set -eu
[ "$#" -eq 2 ] || { echo 'Usage: benchmark-allocation-wrappers.sh OLD_REML NEW_REML' >&2; exit 1; }
old=$1 new=$2
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
SML_LIB=$(CDPATH= cd -- "${SML_LIB:-$ROOT}" && pwd)
export SML_LIB
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-wrapper-bench.XXXXXX")
echo "Wrapper benchmark artifacts: $OUT"
export AP_ITERATIONS=${AP_ITERATIONS:-30000000}
echo "Loop iterations: $AP_ITERATIONS (plus 10 after reset)"
# Resolve caller-supplied relative executable paths before changing directory.
old=$(command -v "$old")
new=$(command -v "$new")
case $old in /*) ;; *) old="$PWD/$old" ;; esac
case $new in /*) ;; *) new="$PWD/$new" ;; esac
cd "$OUT"
CC=${CC:-cc}
cp "$ROOT/test/region_profile/allocation.sml" "$OUT/pair-ffi.sml"
sed 's/fun touch (p:int\*int) : int = prim ("ap_pair", p)/fun touch (p:int*int) : int = #1 p/' \
  "$OUT/pair-ffi.sml" > "$OUT/pair-ml.sml"
{
  printf 'fun scalar (n:int) : int = prim ("ap_scalar", n)\n'
  sed 's/touch (pair__noinline `r n)/scalar n/' "$OUT/pair-ffi.sml"
} > "$OUT/ffi.sml"
cp "$ROOT/test/region_profile/allocation.c" "$OUT/fixture.c"
printf '\nuintptr_t ap_scalar(uintptr_t n) { return n; }\n' >> "$OUT/fixture.c"
$CC -O2 -c "$OUT/fixture.c" -o "$OUT/fixture.o"
ar rcs "$OUT/libfixture.a" "$OUT/fixture.o"
# Separate caches also prevent the new compiler reusing old generated code.
cache=$(basename "$OUT" | tr -cd '[:alnum:]')
for fixture in pair-ffi pair-ml ffi; do
  printf '%s.sml\n' "$fixture" > "$OUT/$fixture.mlb"
  for mode in base before after; do
    compiler=$old
    set -- -rp
    case $mode in
      before) set -- "$@" -allocation_profile ;;
      after) compiler=$new; set -- "$@" -allocation_profile ;;
    esac
    "$compiler" -no_par "$@" -mlb-subdir "$cache$mode" \
      -libdirs "$OUT" -libs fixture --no_delete_target_files \
      -o "$OUT/$fixture-$mode" "$OUT/$fixture.mlb" > "$OUT/$fixture-$mode.build" 2>&1
  done
done
# Warm each executable, then rotate measurement order to reduce ordering bias.
for fixture in pair-ffi pair-ml ffi; do
  for mode in base before after; do
    "$OUT/$fixture-$mode" > /dev/null
    : > "$OUT/$fixture-$mode.times"
  done
  i=0
  while [ "$i" -lt 9 ]; do
    case $((i%3)) in
      0) modes='base before after' ;;
      1) modes='after base before' ;;
      2) modes='before after base' ;;
    esac
    for mode in $modes; do
      /usr/bin/time -p "$OUT/$fixture-$mode" > /dev/null 2> "$OUT/time"
      awk '$1=="real" {print $2}' "$OUT/time" >> "$OUT/$fixture-$mode.times"
    done
    i=$((i+1))
  done
  for mode in base before after; do
    printf '%s %s median_seconds=' "$fixture" "$mode"
    sort -n "$OUT/$fixture-$mode.times" | sed -n '5p'
  done
done
