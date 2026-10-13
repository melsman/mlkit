#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-flush-disabled.XXXXXX")}
export SML_LIB=${SML_LIB:-$ROOT}
mkdir -p "$OUT"
${CC:-cc} -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/flush-disabled.c" -o "$OUT/flush-disabled.o"
ar rcs "$OUT/libflush_disabled.a" "$OUT/flush-disabled.o"
printf 'val () = prim("tp_flush_disabled_check", ())\n' > "$OUT/main.sml"
for config in gc no_gc; do
 "${MLKIT:-$ROOT/bin/mlkit}" -no_par -"$config" -rp -libdirs "$OUT" -libs flush_disabled,m,c,dl -o "$OUT/$config" "$OUT/main.sml" > "$OUT/$config-build.log" 2>&1
 "$OUT/$config"
 "$OUT/$config" +RTS -rp -rp_interval 0 -p_file "$OUT/$config.prof"
done
echo 'PASS: disabled TimeProfile.flush leaves shared region output untouched'
