#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-basis-linking.XXXXXX")
echo "Basis linking artifacts: $OUT"
cp "$ROOT/kitdemo/life.sml" "$OUT/life.sml"
# Compare names, not just a count: replacing one dependency with another must
# also be reviewed. Compiler cache paths and link order are irrelevant.
printf '%s\n' General.sml.o Initial.sml.o Initial2.sml.o List.sml.o life.sml.o > "$OUT/expected-units"
for mode in gc nogc gcprof nogcprof; do
  case "$mode" in gc) flags=-gc ;; nogc) flags=-no_gc ;; gcprof) flags='-gc -rp' ;; nogcprof) flags='-no_gc -rp' ;; esac
  "$MLKIT" $flags -debug_linking -o "$OUT/$mode" "$OUT/life.sml" > "$OUT/$mode.log" 2>&1
  sed -n 's@^Using .*/\([^/]*\) (.*)$@\1@p' "$OUT/$mode.log" | LC_ALL=C sort > "$OUT/$mode.units"
  if ! diff -u "$OUT/expected-units" "$OUT/$mode.units"; then
    echo "Life link dependencies changed in $mode; review added or removed units" >&2
    exit 1
  fi
  "$OUT/$mode" > "$OUT/$mode.out"
  cmp "$OUT/gc.out" "$OUT/$mode.out"
done
echo 'Basis linking: unused system libraries eliminated; Life output unchanged across GC/profiling modes'
