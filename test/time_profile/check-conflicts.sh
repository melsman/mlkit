#!/bin/sh
set -eu
case $(uname -sm) in 'Darwin arm64'|'Linux x86_64') ;; *) echo 'Requires macOS ARM64 or Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-timer-conflicts.XXXXXX")
trap 'rm -rf "$OUT"' EXIT
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/conflict.c" -o "$OUT/conflict.o"
ar rcs "$OUT/libconflict.a" "$OUT/conflict.o"
printf 'val probe:int = prim ("tp_conflict_probe", 1)\nval () = print (Int.toString probe ^ "\\n")\n' > "$OUT/conflict.sml"
for config in gc no_gc parallel; do
 case $config in parallel) flags="-no_gc -par"; modes="mask handler"; expected_signal=SIGUSR2; export MLKIT_TP_TEST_PARALLEL=1;;
 *) flags="-$config -no_par -rp"; modes="mask handler timer"; expected_signal=SIGALRM; unset MLKIT_TP_TEST_PARALLEL;; esac
 "${MLKIT:-$ROOT/bin/mlkit}" $flags -libdirs "$OUT" -libs conflict,m,c,dl -o "$OUT/test" "$OUT/conflict.sml" > "$OUT/build.log" 2>&1
 for mode in $modes; do
  if MLKIT_TP_TEST_CONFLICT=$mode "$OUT/test" +RTS -tp -p_file "$OUT/conflict.rp" > "$OUT/run.log" 2>&1; then
   echo "Accepted conflicting $mode in $config" >&2;exit 1
  fi
  case $mode in mask) expected="unblocked $expected_signal";; *) if [ "$config" = parallel ]; then expected='SIGUSR2 already in use'; else expected='SIGALRM/ITIMER_REAL already in use'; fi;; esac
  grep -q "$expected" "$OUT/run.log"
 done
 "$OUT/test" +RTS -tp -p_file "$OUT/valid.rp" > "$OUT/run.log" 2>&1
 "${PVIEW:-$ROOT/bin/pview}" "$OUT/valid.rp" --format json -o "$OUT/valid.json" >/dev/null
done
echo 'PASS: blocked signals, existing handlers and timers rejected in single-thread and pthread builds'
