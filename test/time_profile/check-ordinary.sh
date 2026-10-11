#!/bin/sh
set -eu
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-ordinary.XXXXXX")}
echo "Ordinary time profiling artifacts: $OUT"
export SML_LIB=${SML_LIB:-$ROOT}
mkdir -p "$OUT"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
for config in gc no_gc gengc; do
 "${MLKIT:-$ROOT/bin/mlkit}" -no_par -"$config" -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/$config" "$OUT/measure.mlb" > "$OUT/$config-build.log" 2>&1
 "$OUT/$config" > "$OUT/$config-off.log"
 "$OUT/$config" +RTS -tp -tp_paused -tp_file "$OUT/$config.rp" > "$OUT/$config-on.log"
 sed '/^c /d' "$OUT/$config-off.log" > "$OUT/off-stable.log"
 sed '/^c /d' "$OUT/$config-on.log" > "$OUT/on-stable.log"
 cmp "$OUT/off-stable.log" "$OUT/on-stable.log"
 "${RPVIEW:-$ROOT/bin/rpview}" "$OUT/$config.rp" --format json -o "$OUT/$config.json" >/dev/null
 "${RPVIEW:-$ROOT/bin/rpview}" "$OUT/$config.rp" --ir-dir "$OUT/MLB" --ir-dir "$ROOT/basis/MLB" -o "$OUT/$config.html" >/dev/null
 node "$ROOT/test/time_profile/ordinary-assertions.js" "$OUT/$config.json" "$OUT/$config.html" "${RPVIEW:-$ROOT/bin/rpview}" "${RPENCODER:-$ROOT/bin/rpfixture}"
done
# No explicit API calls: overflow persists until normal-shutdown draining.
printf 'val x = List.tabulate (200000, fn i => i)\nfun spin__noinline 0 = () | spin__noinline n = (List.foldl op+ 0 x; spin__noinline(n-1))\nval () = spin__noinline 2000\n' > "$OUT/exit.sml"
printf '$(SML_LIB)/basis/basis.mlb\nexit.sml\n' > "$OUT/exit.mlb"
"${MLKIT:-$ROOT/bin/mlkit}" -no_par -gc -o "$OUT/exit" "$OUT/exit.mlb" > "$OUT/exit-build.log" 2>&1
"$OUT/exit" +RTS -tp -tp_buffer 2 -tp_interval 100us -tp_file "$OUT/exit.rp"
"${RPVIEW:-$ROOT/bin/rpview}" "$OUT/exit.rp" --format json -o "$OUT/exit.json" >/dev/null
node -e 'const r=require("fs").readFileSync(process.argv[1],"utf8").trim().split("\n").map(JSON.parse),a=require("assert");a.equal(r.filter(x=>x.type==="time_sample").length,2);const s=r.filter(x=>x.type==="time_status").at(-1);a(s.dropped>0);a.equal(s.final,1);a(r.some(x=>x.type==="session_end"));' "$OUT/exit.json"
echo 'PASS: ordinary GC/GenGC/noGC sampling, output parity, passive ranges and shutdown overflow'
# Stripping removes optional C names but must retain compiler/shared ranges.
"${MLKIT:-$ROOT/bin/mlkit}" -no_par -gc -strip -o "$OUT/stripped" "$OUT/exit.mlb" > "$OUT/stripped-build.log" 2>&1
"$OUT/stripped" +RTS -tp -tp_buffer 2 -tp_file "$OUT/stripped.rp"
"${RPVIEW:-$ROOT/bin/rpview}" "$OUT/stripped.rp" --format json -o "$OUT/stripped.json" >/dev/null
node -e 'const r=require("fs").readFileSync(process.argv[1],"utf8").trim().split("\n").map(JSON.parse),a=require("assert"),f=r.filter(x=>x.type==="code_function");a(f.some(x=>x.function.includes("spin__noinline")));for(const n of ["mlkit_arm64_allocate_preserving","mlkit_arm64_allocate_untagged_preserving","mlkit_arm64_reset_preserving","resetRegion","gc"])a(f.some(x=>x.function===n),n);' "$OUT/stripped.json"
echo 'PASS: stripped executable retains passive ML/shared runtime metadata'
# Ordinary parallel runtimes recognize the option and reject activation clearly.
printf 'val x = 1\n' > "$OUT/parallel.sml"
"${MLKIT:-$ROOT/bin/mlkit}" -no_gc -par -o "$OUT/parallel" "$OUT/parallel.sml" > "$OUT/parallel-build.log" 2>&1
"$OUT/parallel"
if "$OUT/parallel" +RTS -tp > "$OUT/parallel-rejected.log" 2>&1; then
 echo 'Unexpectedly accepted ordinary parallel time sampling' >&2; exit 1
fi
if [ -z "$(sed -n '/single-thread macOS ARM64/p' "$OUT/parallel-rejected.log")" ]; then
 cat "$OUT/parallel-rejected.log" >&2; exit 1
fi
echo 'PASS: ordinary parallel option recognized and unsupported activation rejected'
