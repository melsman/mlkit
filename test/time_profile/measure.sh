#!/bin/sh
# Opt-in macOS ARM64 measurement; keeps raw profiles, timings and summary.
set -eu
case $(uname -sm) in 'Darwin arm64') ;; *) echo 'Requires macOS ARM64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-measure.XXXXXX")}
mkdir -p "$OUT"
echo "Measurement artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
cp "$ROOT/src/Parsing/Topdec.grm" "$OUT/Topdec.grm"
# Repeated parser generation avoids drawing overhead conclusions from a 40ms run.
cat > "$OUT/yacc-driver.sml" <<'SML'
val file = hd (CommandLine.arguments ())
fun repeat 0 = ()
  | repeat n = (ParseGen.parseGen file; repeat (n-1))
val () = repeat 20
SML
node - "$ROOT" "$OUT" <<'JS'
const fs=require('fs'),path=require('path'),[root,out]=process.argv.slice(2),base=path.join(root,'src/Tools/ml-yacc/src');
const isolated=path.join(out,'yacc-source');fs.mkdirSync(isolated,{recursive:true});
for(const folder of ['src','lib'])fs.cpSync(path.join(root,'src/Tools/ml-yacc',folder),path.join(isolated,folder),{recursive:true,filter:p=>!p.split(path.sep).includes('MLB')});
const source=path.join(isolated,'src');
const mlb=fs.readFileSync(path.join(source,'ml-yacc.mlb'),'utf8').split('\n').map(line=>{
 const name=line.trim();if(name==='export-yacc.sml')return path.join(out,'yacc-driver.sml');
 if(/\.(sml|sig|mlb)$/.test(name)&&!name.startsWith('$'))return path.resolve(source,name);
 return line;
}).join('\n');fs.writeFileSync(path.join(out,'mlyacc.mlb'),mlb);
JS
for config in gc no_gc; do
 for mode in plain instrumented; do
  case $mode in plain) flags=;; instrumented) flags=-rp;; esac
  "$MLKIT" -no_par -"$config" $flags -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/phases-$config-$mode" "$OUT/measure.mlb" > "$OUT/phases-$config-$mode-build.log" 2>&1
  "$MLKIT" -no_par -"$config" $flags -o "$OUT/kkb-$config-$mode" "$ROOT/test/kkb_eq.sml" > "$OUT/kkb-$config-$mode-build.log" 2>&1
  "$MLKIT" -no_par -"$config" $flags -o "$OUT/mlyacc-$config-$mode" "$OUT/mlyacc.mlb" > "$OUT/mlyacc-$config-$mode-build.log" 2>&1
 done
done
cp "$ROOT/test/time_profile/attribution.sml" "$OUT/callbacks.sml"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\ncallbacks.sml\n' > "$OUT/callbacks.mlb"
for config in gc no_gc; do
 case $config in gc) tagged=-DTP_GC;; no_gc) tagged=;; esac
 cc -O2 -Wall -Wextra -Werror $tagged -iquote "$ROOT/src/Runtime" -c "$ROOT/test/time_profile/attribution.c" -o "$OUT/callbacks.o"
 ar rcs "$OUT/libcallbacks.a" "$OUT/callbacks.o"
 "$MLKIT" -no_par -"$config" -rp -libdirs "$OUT" -libs callbacks,m,c,dl -o "$OUT/callbacks-$config-instrumented" "$OUT/callbacks.mlb" > "$OUT/callbacks-$config-build.log" 2>&1
done
node "$ROOT/test/time_profile/measure.js" "$OUT" "${PVIEW:-$ROOT/bin/pview}" "${REPEATS:-5}"
