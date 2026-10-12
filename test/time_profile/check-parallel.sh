#!/bin/sh
set -eu
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-parallel.XXXXXX")}
mkdir -p "$OUT"
echo "Parallel time profiling artifacts: $OUT"
export SML_LIB=${SML_LIB:-$ROOT}
cp "$ROOT/test/time_profile/parallel.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/basis/par.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nparallel.sml\n' > "$OUT/parallel.mlb"
for mode in ordinary profiled; do
 case $mode in ordinary) flags=;; profiled) flags=-rp;; esac
 "${MLKIT:-$ROOT/bin/mlkit}" -no_gc -par $flags -o "$OUT/$mode" "$OUT/parallel.mlb" > "$OUT/$mode-build.log" 2>&1
 "$OUT/$mode" > "$OUT/$mode-off.log"
 if [ "$mode" = profiled ]; then
  "$OUT/$mode" +RTS -rp -rp_interval 0 -p_file "$OUT/region-only.rp" > "$OUT/region-only.log"
  cmp "$OUT/$mode-off.log" "$OUT/region-only.log"
  "${PVIEW:-$ROOT/bin/pview}" "$OUT/region-only.rp" --format json -o "$OUT/region-only.json" >/dev/null
  if grep -q '"type":"time_session"' "$OUT/region-only.json"; then echo 'Region-only execution started time sampling' >&2; exit 1; fi
 fi
 for scenario in normal overflow combined; do
  if [ "$mode" = ordinary ] && [ "$scenario" = combined ]; then continue; fi
  case $scenario in normal) set --;; overflow) set -- -tp_buffer 4;; combined) set -- -rp -rp_interval 20ms;; esac
  stem="$OUT/$mode-$scenario"
  "$OUT/$mode" +RTS -tp -p_file "$stem.rp" "$@" > "$stem.log" 2>&1
  cmp "$OUT/$mode-off.log" "$stem.log"
  "${PVIEW:-$ROOT/bin/pview}" "$stem.rp" --format json -o "$stem.json" >/dev/null
  "${PVIEW:-$ROOT/bin/pview}" "$stem.rp" --ir-dir "$OUT/MLB" --ir-dir "$ROOT/basis/MLB" -o "$stem.html" >/dev/null
  node "$ROOT/test/time_profile/parallel-assertions.js" "$stem.json" "$scenario"
  {
   sh "$ROOT/test/region_profile/viewer-prelude.sh" "$stem.html"
   sed -n '/^<script>$/,/^<\/script>/p' "$stem.html" | sed '1d;$d'
   cat "$ROOT/test/time_profile/parallel-viewer-assertions.js"
  } > "$stem-viewer.js"
  node "$stem-viewer.js"
 done
 if "$OUT/$mode" +RTS -tp -tp_attribution detailed > "$OUT/rejected.log" 2>&1; then
  echo 'Accepted parallel detailed attribution' >&2; exit 1
 fi
 grep -q 'passive attribution only' "$OUT/rejected.log"
done
