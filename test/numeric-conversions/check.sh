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
cat > "$archive/shadow.mlb" <<'MLB'
$(SML_LIB)/basis/basis.mlb
shadow.sml
shadow-client.sml
MLB
cat > "$archive/foreign.mlb" <<'MLB'
$(SML_LIB)/basis/basis.mlb
foreign.sml
MLB
${CC:-cc} -c "$root/test/numeric-conversions/foreign.c" -o "$archive/foreign.o"
export SML_LIB="$root"
for mode in gc plain noopt; do
    case "$mode" in
        gc) set -- ;;
        plain) set -- -no_gc -no_par ;;
        noopt)
            set -- -no_optimiser
            # Recompile test units without rebuilding the shared Basis.
            rm -rf "$archive/MLB" ;;
    esac
    "$compiler" "$@" -output "$archive/matrix-$mode" "$archive/matrix.mlb" > "$archive/matrix-$mode.log" 2>&1
    if ! "$archive/matrix-$mode" > "$archive/matrix-$mode.out" 2>&1; then
        cat "$archive/matrix-$mode.out" >&2
        exit 1
    fi
    cmp "$archive/matrix-$mode.out" "$root/test/numeric-conversions.sml.out.ok"
    "$compiler" "$@" -output "$archive/client-$mode" "$archive/client.mlb" > "$archive/client-$mode.log" 2>&1
    if ! "$archive/client-$mode" 255 > "$archive/client-$mode.out" 2>&1; then
        cat "$archive/client-$mode.out" >&2
        exit 1
    fi
    test "$(cat "$archive/client-$mode.out")" = 'conversion client: OK'
    "$compiler" "$@" -output "$archive/shadow-$mode" "$archive/shadow.mlb" > "$archive/shadow-$mode.log" 2>&1
    "$archive/shadow-$mode" > "$archive/shadow-$mode.out"
    test "$(cat "$archive/shadow-$mode.out")" = 'conversion shadowing: OK'
    "$compiler" "$@" -ldexe "${CC:-cc} $archive/foreign.o" \
        -output "$archive/foreign-$mode" "$archive/foreign.mlb" > "$archive/foreign-$mode.log" 2>&1
    "$archive/foreign-$mode" > "$archive/foreign-$mode.out"
    printf 'foreign conversion called\nforeign conversion: OK\n' > "$archive/foreign.expected"
    cmp "$archive/foreign-$mode.out" "$archive/foreign.expected"
done
# The library and client are distinct compilation units. Inspect the optimized
# selection unit only; Basis/library output may legitimately contain fallbacks.
rm -rf "$archive/MLB"
"$compiler" -output "$archive/selection" "$archive/selection.mlb" > "$archive/selection.log" 2>&1
find "$archive/MLB" -type f -name 'selection.sml*.o.ir' -exec cat {} + > "$archive/selection.ir"
test -s "$archive/selection.ir"
if grep -E 'intinf|IntInf|__convert_|fromByte|fromSignedByte|fromInt64|fromLarge|:: ' "$archive/selection.ir"; then
    echo 'A composed conversion retained an IntInf intermediate or conversion call' >&2
    exit 1
fi
printf 'Numeric conversion checks passed.\n'
