#!/bin/sh
# Dependency-free DOM behavior checks; uses the report's actual inline script.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-ir-viewer.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
sh "$ROOT/test/region_profile/encode-fixture.sh" "$ROOT/test/region_profile/graph-fixture.json" "$OUT/profile.rp"
"${RPVIEW:-$ROOT/bin/rpview}" "$OUT/profile.rp" -o "$OUT/profile.html" > /dev/null
{
 cat "$ROOT/test/region_profile/graph-prelude.js"
 sed -n '/^<script>$/,/^<\/script>/p' "$OUT/profile.html" | sed '1d;$d;s/^const samples=/let samples=/'
 cat "$ROOT/test/region_profile/graph-assertions.js"
 cat "$ROOT/test/region_profile/call-graph-assertions.js"
 cat "$ROOT/test/region_profile/ir-assertions.js"
 printf '%s\n' 'console.log("Graph and IR navigation: PASS");'
} > "$OUT/check.js"
node "$OUT/check.js"
# Optional native profiles verify the actual compiler/runtime/IR packaging chain.
for profile in "$@"; do
 "${RPVIEW:-$ROOT/bin/rpview}" "$profile" -o "$OUT/native.html" > /dev/null
 {
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/native.html" | sed '1d;$d'
  cat <<'JS'
assert((profile.allocations||[]).length>0,'Native fixture must contain allocations');
for(const record of profile.allocations){
 assert.equal(irSites.get(String(record.definition)).status,'available');
 showIR(record,null);
 const marks=el('ir-code').querySelectorAll('mark');assert(marks.length>0,'Missing native highlight');
 assert(Number(record.location_kind)===1?marks[0].textContent.startsWith('$'):/^(attop|atbot|sat) /.test(marks[0].textContent),'Wrong native allocation location');
}
if((profile.ir_documents||[]).some(d=>(d.calls||[]).length)){
 el('allocation-group').value='calls';allocationTable();
 const buttons=el('allocation-call-graph').querySelectorAll('button').filter(b=>b.textContent.startsWith('Site '));
 assert.equal(buttons.length,new Set(profile.allocations.map(r=>String(r.definition))).size);
 assert(el('allocation-call-graph').textContent.includes('caller context'));
 if(profile.ir_documents.some(d=>(d.calls||[]).some(e=>e.kind==='closure'))){
  el('allocation-group').value='creators';allocationTable();
  const graph=el('allocation-call-graph');
  assert.equal(graph.querySelectorAll('button').filter(b=>b.textContent.startsWith('Site ')).length,buttons.length);
  assert(/creates closure|Creates closure within/.test(graph.textContent),'Missing closure creator relationship');
 }
}
console.log('Native allocation IR highlights and call graph: PASS');
JS
 } > "$OUT/native.js"
 node "$OUT/native.js"
done
