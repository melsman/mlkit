#!/bin/sh
# T2: native function tables, linked library retention, ASLR and offline lookup.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-code.XXXXXX")}
mkdir -p "$OUT"
echo "Code metadata artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
PVIEW=${PVIEW:-$ROOT/bin/pview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
$CC -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
for mode in gc no_gc; do
  "$MLKIT" -"$mode" -no_par -rp -libdirs "$OUT" -libs measure,m,c,dl \
    -o "$OUT/$mode" "$OUT/measure.mlb" > "$OUT/$mode-build.log" 2>&1
  nm -n "$OUT/$mode" > "$OUT/$mode-symbols.txt"
  for run in first second; do
    "$OUT/$mode" +RTS -tp -tp_paused -rp -rp_interval 0 -p_file "$OUT/$mode-$run.rp" \
      > "$OUT/$mode-$run.log" 2>&1
    "$PVIEW" "$OUT/$mode-$run.rp" --format json -o "$OUT/$mode-$run.json" > /dev/null
  done
  "$PVIEW" "$OUT/$mode-first.rp" -o "$OUT/$mode.html" > /dev/null
  # One more link gives a different image identity and exercises mismatch checks.
  "$MLKIT" -"$mode" -no_par -rp -libdirs "$OUT" -libs measure,m,c,dl \
    -o "$OUT/$mode-other" "$OUT/measure.mlb" > "$OUT/$mode-other-build.log" 2>&1
  "$OUT/$mode-other" +RTS -tp -tp_paused -rp -rp_interval 0 \
    -p_file "$OUT/$mode-other.rp" > "$OUT/$mode-other.log" 2>&1
  "$PVIEW" "$OUT/$mode-other.rp" --format json -o "$OUT/$mode-other.json" > /dev/null
done
mkdir -p "$OUT/repl"
(cd "$OUT/repl" && printf ':quit;\n' | "$MLKIT" -no_gc -rp -rp_interval 0 -p_file "$OUT/repl.rp" \
  > "$OUT/repl.log" 2>&1)
"$PVIEW" "$OUT/repl.rp" --format json -o "$OUT/repl.json" > /dev/null
node "$ROOT/test/time_profile/code-metadata-assertions.js" "$OUT" "$PVIEW"
# Optional SML compiler runs exact-address and malformed-input unit checks.
if command -v mlton > /dev/null 2>&1; then
  mlton -output "$OUT/resolution" "$ROOT/test/time_profile/code-resolution.mlb"
  "$OUT/resolution"
fi
echo 'Function metadata and offline PC checks passed'
