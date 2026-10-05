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
console.log('Native allocation IR highlights: PASS');
JS
 } > "$OUT/native.js"
 node "$OUT/native.js"
done
