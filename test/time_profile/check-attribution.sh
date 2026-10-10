#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-attribution.XXXXXX")}
mkdir -p "$OUT"
echo "Attribution artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
${CC:-cc} -O2 -Wall -Wextra -Werror -iquote "$ROOT/src/Runtime" -c "$ROOT/test/time_profile/attribution.c" -o "$OUT/attribution.o"
ar rcs "$OUT/libattribution.a" "$OUT/attribution.o"
cp "$ROOT/test/time_profile/attribution.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nattribution.sml\n' > "$OUT/attribution.mlb"
for mode in gc no_gc; do
  case $mode in gc) gcflag=-DTP_GC;; no_gc) gcflag=;; esac
  ${CC:-cc} -O2 -Wall -Wextra -Werror $gcflag -iquote "$ROOT/src/Runtime" -c "$ROOT/test/time_profile/attribution.c" -o "$OUT/attribution.o"
  ar rcs "$OUT/libattribution.a" "$OUT/attribution.o"
  "$MLKIT" -no_par -"$mode" -rp -libdirs "$OUT" -libs attribution,m,c,dl -o "$OUT/$mode" "$OUT/attribution.mlb" > "$OUT/$mode-build.log" 2>&1
  "$OUT/$mode" +RTS -tp -tp_file "$OUT/$mode.rp" > "$OUT/$mode.log" 2>&1
  "$RPVIEW" "$OUT/$mode.rp" --format json -o "$OUT/$mode.json" > /dev/null
done
node "$ROOT/test/time_profile/attribution-assertions.js" "$OUT"
echo 'Nested C origins, ML callbacks, caught exceptions, restoration and GC attribution passed'

sh "$ROOT/test/time_profile/check-viewer.sh" "$OUT/gc.rp" "$OUT/no_gc.rp"
