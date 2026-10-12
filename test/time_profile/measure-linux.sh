#!/bin/sh
# Opt-in T9 measurements, never timing assertions in CI.
set -eu
case $(uname -sm) in 'Linux x86_64') ;; *) echo 'Requires Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-t9-measure.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
sed '/val () = TimeProfile.start ()/d; /val () = TimeProfile.pause ()/d' "$ROOT/test/time_profile/measure.sml" > "$OUT/steady.sml"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nsteady.sml\n' > "$OUT/steady.mlb"
for config in gc gengc no_gc; do
 for build in ordinary instrumented; do
  flags=
  [ "$build" != instrumented ] || flags=-rp
  "$MLKIT" -no_par -"$config" $flags -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/$config-$build" "$OUT/steady.mlb" > "$OUT/$config-$build.log" 2>&1
 done
done
node "$ROOT/test/time_profile/measure-linux.js" "$OUT" "${PVIEW:-$ROOT/bin/pview}" "${REPETITIONS:-5}"
