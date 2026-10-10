#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-recording.XXXXXX")}
mkdir -p "$OUT"
echo "Time recording artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
${CC:-cc} -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/recording.c" -o "$OUT/recording.o"
ar rcs "$OUT/librecording.a" "$OUT/recording.o"
cp "$ROOT/test/time_profile/recording.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nrecording.sml\n' > "$OUT/recording.mlb"
for mode in gc no_gc; do
  case $mode in gc) tagged=-DTP_TAGGED;; no_gc) tagged=;; esac
  ${CC:-cc} -O2 -Wall -Wextra -Werror $tagged -c "$ROOT/test/time_profile/recording.c" -o "$OUT/recording.o"
  ar rcs "$OUT/librecording.a" "$OUT/recording.o"
  "$MLKIT" -no_par -"$mode" -rp -libdirs "$OUT" -libs recording,m,c,dl -o "$OUT/$mode" "$OUT/recording.mlb" > "$OUT/$mode-build.log" 2>&1
  for scenario in normal overflow combined; do
    case $scenario in
      normal) set -- -tp_buffer 4096;;
      overflow) set -- -tp_buffer 4;;
      combined) set -- -rp -rp_interval 20ms;;
    esac
    "$OUT/$mode" +RTS -tp -tp_paused -tp_file "$OUT/$mode-$scenario.rp" "$@" > "$OUT/$mode-$scenario.log" 2>&1
    "$RPVIEW" "$OUT/$mode-$scenario.rp" --format json -o "$OUT/$mode-$scenario.json" > /dev/null
  done
  for args in '-tp_clock cpu -tp' '-tp_buffer 1 -tp' '-tp_interval 0ms -tp' '-tp_interval 2s -tp' '-tp_buffer 4'; do
    if "$OUT/$mode" +RTS $args > "$OUT/rejected.log" 2>&1; then
      echo "Unexpectedly accepted: $args" >&2; exit 1
    fi
  done
done
for config in plain parallel; do
  case $config in plain) set -- -no_par;; parallel) set -- -par -rp;; esac
  "$MLKIT" -no_gc "$@" -libdirs "$OUT" -libs recording,m,c,dl -o "$OUT/$config" "$OUT/recording.mlb" > "$OUT/$config-build.log" 2>&1
  if "$OUT/$config" +RTS -tp > "$OUT/$config-rejected.log" 2>&1; then
    echo "Time sampling unexpectedly accepted $config" >&2; exit 1
  fi
done
# A slow consumer forces signals during serialization; handler writes the other buffer.
${CC:-cc} -O2 -Wall -Wextra -Werror -DTP_SLOW_READER "$ROOT/test/time_profile/recording.c" -o "$OUT/slow-reader"
mkfifo "$OUT/pipe"
"$OUT/slow-reader" < "$OUT/pipe" > "$OUT/slow.rp" &
reader=$!
"$OUT/gc" slow +RTS -tp -tp_interval 1ms -tp_buffer 1024 -tp_file "$OUT/pipe" > "$OUT/slow.log" 2>&1
wait "$reader"
"$RPVIEW" "$OUT/slow.rp" --format json -o "$OUT/slow.json" > /dev/null
node "$ROOT/test/time_profile/recording-assertions.js" "$OUT" "$RPVIEW"
echo 'Time recorder lifecycle, overflow, shared timer and argument checks passed'

sh "$ROOT/test/time_profile/check-viewer.sh" "$OUT/gc-combined.rp" "$OUT/no_gc-combined.rp"
