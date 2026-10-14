#!/bin/sh
# Opt-in native measurement; keeps raw profiles, timings and summary.
set -eu
case $(uname -sm) in 'Darwin arm64'|'Linux x86_64') ;; *) echo 'Requires macOS ARM64 or Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-measure-ordinary.XXXXXX")}
mkdir -p "$OUT"
echo "Measurement artifacts: $OUT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
CONFIGS=${CONFIGS:-"gc gengc no_gc"}
WORKLOADS=${WORKLOADS:-"phases kkb mlyacc"}
export CONFIGS WORKLOADS
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/measure.c" -o "$OUT/measure.o"
ar rcs "$OUT/libmeasure.a" "$OUT/measure.o"
cp "$ROOT/test/time_profile/measure.sml" "$OUT/"
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/kitlib/time-profile.mlb\nmeasure.sml\n' > "$OUT/measure.mlb"
case " $WORKLOADS " in *" mlyacc "*)
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
;; esac
for config in $CONFIGS; do
 for mode in plain instrumented; do
  case $mode in plain) flags=;; instrumented) flags=-rp;; esac
  for workload in $WORKLOADS; do
   case $workload in phases) source="$OUT/measure.mlb";;
    kkb) source="$ROOT/test/kkb_eq.sml";; mlyacc) source="$OUT/mlyacc.mlb";;
    *) echo "Unknown workload $workload" >&2; exit 1;; esac
   "$MLKIT" -no_par -"$config" $flags -libdirs "$OUT" -libs measure,m,c,dl -o "$OUT/$workload-$config-$mode" "$source" > "$OUT/$workload-$config-$mode-build.log" 2>&1
  done
 done
done
if [ "${BUILD_ONLY:-0}" = 1 ]; then exit 0; fi
if [ "${ATTRIBUTION:-0}" = 1 ]; then set -- --attribution; else set --; fi
node "$ROOT/test/time_profile/measure-ordinary.js" "$OUT" "${PVIEW:-$ROOT/bin/pview}" "${REPEATS:-5}" "$@"
