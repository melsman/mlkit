#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
: "${SML_LIB:=$ROOT}"
: "${MLKIT:=$ROOT/bin/mlkit}"
export SML_LIB
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-intword32.XXXXXX")
echo "Int32/Word32 test outputs: $OUT"
case "$(uname -s)/${INTWORD32_ARCH:-$(uname -m)}" in
  Darwin/arm64) arch='-arch arm64';;
  Darwin/x86_64) arch='-arch x86_64';;
  Linux/x86_64) arch=;;
  *) exit 1;;
esac
${CC:-cc} $arch -O2 -Wall -Wextra -Werror -c "$ROOT/test/intword32/native.c" -o "$OUT/plain.o"
${CC:-cc} $arch -O2 -Wall -Wextra -Werror -DTAG_VALUES -c "$ROOT/test/intword32/native.c" -o "$OUT/tag.o"
printf 'intword32 native: OK\n' > "$OUT/expected"
for mode in ${INTWORD32_MODES:-plain tagged gc gengc prof gcprof spill noopt par}; do
  case "$mode" in
    plain) flags='-no_gc -no_par'; obj=plain;;
    tagged) flags='-no_gc -no_par -tag'; obj=tag;;
    gc) flags=-gc; obj=tag;;
    gengc) flags=-gengc; obj=tag;;
    prof) flags='-no_gc -rp'; obj=plain;;
    gcprof) flags='-gc -rp'; obj=tag;;
    spill) flags='-gc --no_register_allocation'; obj=tag;;
    noopt) flags='-gc -no_opt'; obj=tag;;
    par) flags='-no_gc -par'; obj=plain;;
    *) exit 1;;
  esac
  # X64's plain and tag-without-GC modes share the RI cache name. Their
  # Basis ABIs differ, so give the explicit tagging case a separate cache.
  cache=${INTWORD32_CACHE:-}
  if [ "$mode" = tagged ]; then cache="${cache}IntWord32Tagged"; fi
  mkdir "$OUT/$mode"
  cp "$ROOT/test/intword32/native.sml" "$OUT/$mode/main.sml"
  printf '$(SML_LIB)/basis/basis.mlb
main.sml
' > "$OUT/$mode/main.mlb"
  (cd "$OUT/$mode" && "$MLKIT" $flags --mlb-subdir "$cache" --no_basislib --no_delete_target_files \
    -ldexe "${CC:-cc} $arch $OUT/$obj.o" -o program main.mlb > build.log 2>&1) || { tail -60 "$OUT/$mode/build.log"; exit 1; }
  "$OUT/$mode/program" > "$OUT/$mode/actual"
  cmp "$OUT/expected" "$OUT/$mode/actual"
  echo "$mode passed"
done
