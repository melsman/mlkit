#!/bin/sh
# Explicit recording layout, attribution availability and independent IR navigation.
set -eu
case $(uname -sm) in 'Darwin arm64'|'Linux x86_64') ;; *) echo 'Requires macOS ARM64 or Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-attribution-modes.XXXXXX")}
mkdir -p "$OUT"
echo "Attribution mode artifacts: $OUT"
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
for config in gc gengc no_gc; do
 for build in ordinary instrumented; do
  case $build in ordinary) flags=;; instrumented) flags=-rp;; esac
  exe="$OUT/$config-$build"
  "${MLKIT:-$ROOT/bin/mlkit}" -no_par -"$config" $flags -libdirs "$OUT" -libs measure,m,c,dl -o "$exe" "$OUT/measure.mlb" > "$exe-build.log" 2>&1
  if [ "$build" = ordinary ]; then
   for options in '-tp -tp_attribution detailed' '-tp -tp_attribution wrong' '-tp_attribution passive'; do
    if "$exe" +RTS $options > "$OUT/rejected.log" 2>&1; then
     echo "Unexpectedly accepted: $options" >&2; exit 1
    fi
   done
   "$exe" +RTS -tp -tp_attribution detailed > "$OUT/unavailable.log" 2>&1 && exit 1
   grep -q 'detailed C attribution requires' "$OUT/unavailable.log"
  fi
  for mode in passive detailed combined-passive; do
   if [ "$build" = ordinary ] && [ "$mode" != passive ]; then continue; fi
   case $mode in combined-passive) attribution=passive; set -- -rp -rp_interval 10ms;; *) attribution=$mode; set --;; esac
   stem="$exe-$mode"
   "$exe" +RTS -tp -tp_paused -tp_attribution "$attribution" -p_file "$stem.rp" "$@" > "$stem.log" 2>&1
   "${PVIEW:-$ROOT/bin/pview}" "$stem.rp" --format json -o "$stem.json" >/dev/null
   "${PVIEW:-$ROOT/bin/pview}" "$stem.rp" --ir-dir "$OUT/MLB" --ir-dir "$ROOT/basis/MLB" -o "$stem.html" >/dev/null
   node "$ROOT/test/time_profile/attribution-modes-assertions.js" "$stem.json" "$mode"
   {
    sh "$ROOT/test/region_profile/viewer-prelude.sh" "$stem.html"
    sed -n '/^<script>$/,/^<\/script>/p' "$stem.html" | sed '1d;$d'
    cat "$ROOT/test/time_profile/function-ir-assertions.js"
   } > "$stem-viewer.js"
   node "$stem-viewer.js"
  done
 done
done
