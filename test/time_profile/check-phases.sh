#!/bin/sh
set -eu
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-phases.XXXXXX")
trap 'rm -rf "$OUT"' EXIT
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
for config in gc no_gc; do
 "${MLKIT:-$ROOT/bin/mlkit}" -no_par -"$config" -rp -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/test" "$OUT/measure.mlb" > "$OUT/build.log" 2>&1
 for interval in 1ms 100us; do
  "$OUT/test" +RTS -tp -tp_paused -tp_interval "$interval" -p_file "$OUT/phase.rp" > "$OUT/run.log" 2>&1
  "${PVIEW:-$ROOT/bin/pview}" "$OUT/phase.rp" --format json -o "$OUT/phase.json" >/dev/null
  node "$ROOT/test/time_profile/phase-assertions.js" "$OUT/phase.json" "$config"
 done
done
