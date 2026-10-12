#!/bin/sh
# Linux X64 executable identity, PIE relocation, exact native/ML boundaries and stripping.
set -eu
case $(uname -sm) in 'Linux x86_64') ;; *) echo 'Requires Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-elf.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
PVIEW=${PVIEW:-$ROOT/bin/pview}
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/main.mlb"
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
"$MLKIT" -no_par -gc -ldexe 'gcc -pie -Wl,--build-id=sha1' -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/run" "$OUT/main.mlb" > "$OUT/build.log" 2>&1
readelf -n "$OUT/run" > "$OUT/notes.txt"
for name in first second; do
 "$OUT/run" +RTS -tp -p_file "$OUT/$name.prof" > "$OUT/$name.out"
 "$PVIEW" "$OUT/$name.prof" --format json -o "$OUT/$name.json"
done
cp "$OUT/run" "$OUT/stripped"
strip --strip-all "$OUT/stripped"
"$OUT/stripped" +RTS -tp -p_file "$OUT/stripped.prof" > "$OUT/stripped.out"
"$PVIEW" "$OUT/stripped.prof" --format json -o "$OUT/stripped.json"
"$PVIEW" "$OUT/first.prof" --ir-dir "$OUT/MLB" --ir-dir "$ROOT/basis/MLB" -o "$OUT/first.html"
node "$ROOT/test/time_profile/elf-assertions.js" "$OUT" "$PVIEW"
echo "PASS: ELF build ID, PIE/ASLR, native and ML resolution, stripped core ranges and offline IR ($OUT)"
