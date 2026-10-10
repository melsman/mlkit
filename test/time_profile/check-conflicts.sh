#!/bin/sh
set -eu
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-timer-conflicts.XXXXXX")
trap 'rm -rf "$OUT"' EXIT
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/conflict.c" -o "$OUT/conflict.o"
ar rcs "$OUT/libconflict.a" "$OUT/conflict.o"
printf 'val probe:int = prim ("tp_conflict_probe", 1)\nval () = print (Int.toString probe ^ "\\n")\n' > "$OUT/conflict.sml"
for config in gc no_gc; do
 "${MLKIT:-$ROOT/bin/mlkit}" -no_par -"$config" -rp -libdirs "$OUT" -libs conflict,m,c,dl -o "$OUT/test" "$OUT/conflict.sml" > "$OUT/build.log" 2>&1
 for mode in mask handler timer; do
  if MLKIT_TP_TEST_CONFLICT=$mode "$OUT/test" +RTS -tp -tp_file "$OUT/conflict.rp" > "$OUT/run.log" 2>&1; then
   echo "Accepted conflicting $mode in $config" >&2;exit 1
  fi
  case $mode in mask) expected='unblocked SIGALRM';; *) expected='SIGALRM/ITIMER_REAL already in use';; esac
  grep -q "$expected" "$OUT/run.log"
 done
 "$OUT/test" +RTS -tp -tp_file "$OUT/valid.rp" > "$OUT/run.log" 2>&1
 "${RPVIEW:-$ROOT/bin/rpview}" "$OUT/valid.rp" --format json -o "$OUT/valid.json" >/dev/null
done
echo 'Blocked SIGALRM, existing handlers and existing timers rejected in GC/non-GC builds'
