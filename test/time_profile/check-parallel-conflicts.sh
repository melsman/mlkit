#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-par-conflicts.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/parallel-conflict.c" -o "$OUT/conflict.o"
ar rcs "$OUT/libconflict.a" "$OUT/conflict.o"
printf 'val x:int = prim("tp_parallel_probe",1)\n' > "$OUT/main.sml"
"${MLKIT:-$ROOT/bin/mlkit}" -no_gc -par -libdirs "$OUT" -libs conflict,m,c,dl -o "$OUT/test" "$OUT/main.sml" > "$OUT/build.log" 2>&1
for mode in handler mask; do
 if MLKIT_TP_PAR_CONFLICT=$mode "$OUT/test" +RTS -tp -tp_file "$OUT/test.rp" > "$OUT/$mode.log" 2>&1; then echo 'Accepted conflicting SIGUSR2 ownership' >&2; exit 1; fi
 case $mode in handler) expected='SIGUSR2 already in use';; mask) expected='unblocked SIGUSR2';; esac
 grep -q "$expected" "$OUT/$mode.log"
done
echo 'PASS: parallel SIGUSR2 ownership and blocked delivery rejected'
