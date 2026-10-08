#!/bin/sh
# Allocating primitive sugar must retain both call and region locators.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-ir-primitives.XXXXXX")
echo "Primitive IR artifacts: $OUT"
cat > "$OUT/main.sml" <<'SML'
fun box__noinline (x:real) = x + 1.0
fun join__noinline s = s ^ s
val result = (box__noinline 1.0, join__noinline "x")
val _ = print (Real.toString (#1 result) ^ #2 result ^ "\n")
SML
cd "$OUT"
"$MLKIT" -no_gc -rp -Pcee -log_to_file -o program main.sml > build.log 2>&1
set -- MLB/*/main.sml.o.ir
ir=$1
log=${ir%.o.ir}.log
for file in "$ir" "$log"; do
  grep -q 'R64.fromF64' "$file"
  ! grep -q '\$__f64_to_real\|\$concatStringML' "$file"
done
LC_ALL=C awk '
  { lines[NR]=$0 }
  /^MLKIT-IR-LOCATIONS 1$/ { table=1; next }
  table && /^[0-9]+\t/ {
    token=substr(lines[$4],$5,$3);
    if(token=="R64.fromF64") { conversion++; calls[$1]=1 }
    if(token=="^") { concat++; calls[$1]=1 }
    if(token ~ /^(attop|atbot|sat) /) regions[$1]=1;
  }
  END {
    if(!conversion || !concat) exit 1;
    for(mark in calls) if(!regions[mark]) exit 2;
  }' "$ir"
./program +RTS -rp -rp_interval 1i -rp_region all -rp_file profile.rp -RTS > result
[ "$(cat result)" = '2.0xx' ]
"$RPVIEW" profile.rp -o profile.html > /dev/null
{
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' profile.html | sed '1d;$d'
  cat <<'JS'
const tokens = new Set();
for (const record of profile.allocations) {
  const location = irSites.get(String(record.definition));
  assert.equal(location.status,'available');
  showIR(record,null);
  const marks = el('ir-code').querySelectorAll('mark');
  assert(marks.length > 0,'Missing allocation highlight');
  if (Number(record.location_kind) === 1)
    for (const mark of marks) tokens.add(mark.textContent);
}
assert(tokens.has('R64.fromF64'),'Missing friendly conversion call highlight');
assert(tokens.has('^'),'Missing infix concatenation call highlight');
JS
} > check.js
node check.js
echo 'Shared primitive formatting and native call/allocation highlights passed'
