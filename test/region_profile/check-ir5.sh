#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit-arm64}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-ir5.XXXXXX")
echo "IR5 checks: $OUT"
cp "$ROOT/test/region_profile/ir5-duplicates.sml" "$OUT/main.sml"
printf '%s\n' "$SML_LIB/basis/basis.mlb" main.sml > "$OUT/main.mlb"
"$MLKIT" -no_gc -rp -o "$OUT/program" "$OUT/main.mlb" > "$OUT/build.log" 2>&1
"$OUT/program" +RTS -rp -rp_interval 0 -rp_region '<global>:1' -rp_file "$OUT/profile.rp"
"$RPVIEW" "$OUT/profile.rp" --format json -o "$OUT/profile.json"
"$RPVIEW" "$OUT/profile.rp" -o "$OUT/profile.html"
{
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/profile.html" | sed '1d;$d'
  cat <<'JS'
assert.equal(profile.version,'10');
assert(profile.allocations.every(r=>!('point' in r)),'No transitional point field');
const sites=profile.allocations.filter(r=>r.function.includes('make__noinline'));
assert.equal(sites.length,1,'Both lowered allocations share one site definition');
assert.equal(sites[0].count,'200');
assert.equal(sites[0].bytes,'2400');
const location=irSites.get(String(sites[0].definition));
assert.equal(location.status,'available');
assert(location.spans.length>=1,'The originating IR allocation remains navigable');
assert(location.spans.every(s=>String(s.mark)===String(sites[0].site)),'Site IDs directly identify IR spans');
showIR(sites[0],null);
assert(el('ir-code').querySelectorAll('mark').length>0);
console.log('IR5: duplicated allocations aggregate under the original IR site, with direct spans');
JS
} > "$OUT/check.js"
node "$OUT/check.js"
# Generated-site metadata must never accidentally match an IR marker.
node - "$OUT" <<'JS'
const fs=require('fs'),dir=process.argv[2];
const rows=fs.readFileSync(dir+'/profile.json','utf8').trim().split('\n').map(JSON.parse);
fs.writeFileSync(dir+'/generated.json',rows.map(r=>JSON.stringify(r.type==='allocation_site'?{...r,location_kind:2}:r)).join('\n')+'\n');
JS
sh "$ROOT/test/region_profile/encode-fixture.sh" "$OUT/generated.json" "$OUT/generated.rp"
"$RPVIEW" "$OUT/generated.rp" -o "$OUT/generated.html" > /dev/null
{
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/generated.html" | sed '1d;$d'
  printf '%s\n' "assert(profile.ir_sites.length>0);assert(profile.ir_sites.every(s=>s.status==='generated'&&s.spans.length===0));console.log('IR5: generated sites have no misleading IR location');"
} > "$OUT/generated.js"
node "$OUT/generated.js"
