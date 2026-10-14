#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
PVIEW=${PVIEW:-$ROOT/bin/pview}
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-unified-options.XXXXXX")}
export SML_LIB=${SML_LIB:-$ROOT}
mkdir -p "$OUT"
cat > "$OUT/main.sml" <<'SML'
val stop = Time.+ (Time.now (), Time.fromMilliseconds 100)
fun spin () = if Time.< (Time.now (), stop) then spin () else ()
val () = spin ()
SML
printf '$(SML_LIB)/basis/basis.mlb\nmain.sml\n' > "$OUT/main.mlb"
"$MLKIT" -no_par -tp -o "$OUT/ordinary" "$OUT/main.mlb" > "$OUT/ordinary-build.log" 2>&1
"$MLKIT" -no_par -p -o "$OUT/combined" "$OUT/main.mlb" > "$OUT/combined-build.log" 2>&1
for mode in tp rp p; do
 exe=combined
 [ "$mode" != tp ] || exe=ordinary
 "$OUT/$exe" +RTS "-$mode" -p_file "$OUT/$mode.prof" -RTS
 "$PVIEW" "$OUT/$mode.prof" --format json -o "$OUT/$mode.json"
done
node - "$OUT" <<'JS'
const fs=require('fs'),a=require('assert'),p=process.argv[2];
for(const mode of ['tp','rp','p']) {
 const r=fs.readFileSync(`${p}/${mode}.json`,'utf8').trim().split('\n').map(JSON.parse);
 a.equal(r.some(x=>x.type==='time_sample'), mode!=='rp');
 a.equal(r.some(x=>x.type==='sample_begin'), mode!=='tp');
}
JS
for mode in tp rp p; do
 "$OUT/combined" +RTS "-$mode" -p_paused -p_file "$OUT/paused-$mode.prof" -RTS
 "$PVIEW" "$OUT/paused-$mode.prof" --format json -o "$OUT/paused-$mode.json"
 node -e 'const a=require("assert"),r=require("fs").readFileSync(process.argv[1],"utf8").trim().split("\n").map(JSON.parse);a(!r.some(x=>x.type==="time_sample"||x.type==="sample_begin"));' "$OUT/paused-$mode.json"
done
(cd "$OUT" && ./ordinary +RTS -tp -RTS && "$PVIEW" --format json -o default.json)
[ -s "$OUT/profile.prof" ]
cp "$OUT/tp.prof" "$OUT/legacy.rp"
"$PVIEW" "$OUT/legacy.rp" --format json -o "$OUT/legacy.json"
cmp "$OUT/tp.json" "$OUT/legacy.json"
for option in -rp_file -tp_file; do
 for exe in ordinary combined; do
  if "$OUT/$exe" +RTS -tp "$option" "$OUT/bad.prof" > "$OUT/error" 2>&1; then exit 1; fi
  grep -q 'unknown profiler option' "$OUT/error"
 done
 if "$MLKIT" "$option" "$OUT/bad.prof" > "$OUT/error" 2>&1; then exit 1; fi
 grep -q 'unknown option' "$OUT/error"
done
for option in -p_paused; do
 if "$OUT/ordinary" +RTS "$option" > "$OUT/error" 2>&1; then exit 1; fi
 grep -q 'require -rp, -tp or -p' "$OUT/error"
done
if "$OUT/ordinary" +RTS -p_file "$OUT/bad.prof" > "$OUT/error" 2>&1; then exit 1; fi
grep -q "require -rp, -tp or -p" "$OUT/error"
if "$OUT/ordinary" +RTS -p > "$OUT/error" 2>&1; then exit 1; fi
grep -q 'requires region profiling support' "$OUT/error"
"$MLKIT" -help > "$OUT/compiler-help" 2>&1
"$OUT/combined" +RTS -help > "$OUT/runtime-help" 2>&1
"$OUT/ordinary" +RTS -help > "$OUT/ordinary-runtime-help" 2>&1
grep -q '^  -p ' "$OUT/runtime-help"
if grep -q '^  -p ' "$OUT/ordinary-runtime-help"; then
 echo 'Ordinary runtime help advertises unavailable combined profiling' >&2; exit 1
fi
"$PVIEW" --help > "$OUT/viewer-help" 2>&1
grep -q -- '-p_file' "$OUT/runtime-help"
grep -q 'Usage: pview' "$OUT/viewer-help"
mkdir -p "$OUT/repl"
(cd "$OUT/repl" && "$MLKIT" -no_gc -rp -p_paused -p_file "$OUT/repl.prof" < "$ROOT/test/region_profile/repl.cmd" > "$OUT/repl.log" 2>&1)
grep -q 'repl profile ok' "$OUT/repl.log"
"$PVIEW" "$OUT/repl.prof" --format json -o "$OUT/repl.json"
for option in -tp -p; do
 status=0
 (cd "$OUT/repl" && sh "$ROOT/test/region_profile/with-timeout.sh" 30 "$MLKIT" "$option" < /dev/null > "$OUT/repl-error" 2>&1) || status=$?
 [ "$status" -ne 0 ] && [ "$status" -ne 124 ]
 grep -q 'time profiling is unsupported in the dynamic REPL' "$OUT/repl-error"
done
# Disabled time flush must not touch an open region-only stream.
${CC:-cc} -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/flush-disabled.c" -o "$OUT/flush.o"
ar rcs "$OUT/libflush.a" "$OUT/flush.o"
printf 'val () = prim("tp_flush_disabled_check", ())\n' > "$OUT/flush.sml"
for config in gc no_gc; do
 "$MLKIT" -no_par -"$config" -rp -libdirs "$OUT" -libs flush,m,c,dl -o "$OUT/flush-$config" "$OUT/flush.sml" > "$OUT/flush-$config-build.log" 2>&1
 "$OUT/flush-$config"
 "$OUT/flush-$config" +RTS -rp -rp_interval 0 -p_file "$OUT/flush-$config.prof"
done
echo "PASS: unified profiler activation, shared output/pause/defaults, legacy files and removed options ($OUT)"
