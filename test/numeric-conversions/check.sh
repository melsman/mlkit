#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
compiler=${MLKIT:-$root/bin/mlkit}
archive=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-conversions.XXXXXX")
printf 'Conversion validation artifacts: %s\n' "$archive"
cp "$root/test/numeric-conversions.sml" "$archive/matrix.sml"
cp "$root/test/numeric-conversions/"*.sml "$archive/"
cat > "$archive/matrix.mlb" <<'MLB'
$(SML_LIB)/basis/basis.mlb
matrix.sml
MLB
cat > "$archive/client.mlb" <<'MLB'
$(SML_LIB)/basis/basis.mlb
library.sml
client.sml
MLB
cat > "$archive/selection.mlb" <<'MLB'
$(SML_LIB)/basis/basis.mlb
library.sml
selection.sml
MLB
export SML_LIB="$root"
for mode in gc plain noopt; do
    case "$mode" in
        gc) set -- ;;
        plain) set -- -no_gc -no_par ;;
        noopt) set -- -no_optimiser --mlb-subdir NUMERIC_NOOPT ;;
    esac
    "$compiler" "$@" -output "$archive/matrix-$mode" "$archive/matrix.mlb" > "$archive/matrix-$mode.log" 2>&1
    "$archive/matrix-$mode" > "$archive/matrix-$mode.out"
    cmp "$archive/matrix-$mode.out" "$root/test/numeric-conversions.sml.out.ok"
    "$compiler" "$@" -output "$archive/client-$mode" "$archive/client.mlb" > "$archive/client-$mode.log" 2>&1
    "$archive/client-$mode" 255 > "$archive/client-$mode.out"
    test "$(cat "$archive/client-$mode.out")" = 'conversion client: OK'
done
# The library and client are distinct compilation units. Inspect the optimized
# selection unit only; Basis/library output may legitimately contain fallbacks.
"$compiler" -output "$archive/selection" "$archive/selection.mlb" > "$archive/selection.log" 2>&1
find "$archive/MLB" -type f -name 'selection.sml*.o.ir' -exec cat {} + > "$archive/selection.ir"
test -s "$archive/selection.ir"
if grep -E 'intinf|IntInf|__convert_|fromByte|fromSignedByte|fromInt64|fromLarge|:: ' "$archive/selection.ir"; then
    echo 'A composed conversion retained an IntInf intermediate or conversion call' >&2
    exit 1
fi
printf 'Numeric conversion checks passed.\n'
