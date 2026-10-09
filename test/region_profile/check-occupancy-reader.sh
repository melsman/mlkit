#!/bin/sh
# Use the reset fixture produced by check-occupancy.sh.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$1
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
node - "$OUT" <<'JS'
const fs=require('fs'),out=process.argv[2];
const records=fs.readFileSync(out+'/reset.json','utf8').trim().split('\n').map(JSON.parse);
function save(name,rows){fs.writeFileSync(out+'/'+name+'.json',rows.map(JSON.stringify).join('\n')+'\n');}
const end=records.findLastIndex(r=>r.type==='sample_end');
save('truncated',records.slice(0,end));
const summary=records.findIndex(r=>r.type==='occupancy_summary');
save('duplicate',[...records.slice(0,summary),records[summary],...records.slice(summary)]);
save('wrong-instance',records.map(r=>r.type==='occupancy_summary'?{...r,instance:999999}:r));
save('missing-summary',records.filter(r=>r.type!=='occupancy_summary'));
JS
for kind in duplicate wrong-instance missing-summary; do
  if ! sh "$ROOT/test/region_profile/encode-fixture.sh" "$OUT/$kind.json" "$OUT/$kind.rp" > "$OUT/rejected.out" 2>&1; then
    grep -Eq "duplicate occupancy summary|unknown occupancy instance|missing occupancy summary" "$OUT/rejected.out"
    continue
  fi
  if "$RPVIEW" "$OUT/$kind.rp" -o "$OUT/rejected.html" > "$OUT/rejected.out" 2>&1; then
    echo "Invalid occupancy accepted: $kind" >&2; exit 1
  fi
done
sh "$ROOT/test/region_profile/encode-fixture.sh" "$OUT/truncated.json" "$OUT/truncated.rp"
"$RPVIEW" "$OUT/truncated.rp" -o "$OUT/truncated.html" > /dev/null
{
  sh "$ROOT/test/region_profile/viewer-prelude.sh" "$OUT/truncated.html"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/truncated.html" | sed '1d;$d'
  printf '%s\n' "assert.equal(samples.length,1);assert(profile.allocations.every(r=>r.sample==='1'));assert(profile.occupancy_summaries.every(r=>r.sample==='1'));console.log('Occupancy validation and incomplete snapshots: PASS');"
} > "$OUT/truncated.js"
node "$OUT/truncated.js"
