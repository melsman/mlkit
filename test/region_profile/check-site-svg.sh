#!/bin/sh
set -eu
OUT=$1
RPVIEW=${RPVIEW:-rpview}
"$RPVIEW" "$OUT/reset.rp" --sites --regions 0 -o "$OUT/reset.svg" > /dev/null
"$RPVIEW" "$OUT/callback.rp" --sites --regions 0 -o "$OUT/callback.svg" > /dev/null
"$RPVIEW" "$OUT/callback.rp" --sites --regions 1 -o "$OUT/grouped.svg" > /dev/null
"$RPVIEW" "$OUT/callback.rp" --sites --scope thread:0 -o "$OUT/thread.svg" > /dev/null
for args in '--sites --format html' '--sites --format svg --metric stack'; do
  if "$RPVIEW" "$OUT/reset.rp" $args -o "$OUT/rejected" > "$OUT/rejected.log" 2>&1; then
    echo "Invalid site graph options accepted: $args" >&2; exit 1
  fi
done
node - "$OUT" <<'JS'
const fs=require('fs'),assert=require('assert'),dir=process.argv[2];
const read=name=>fs.readFileSync(dir+'/'+name+'.svg','utf8');
const bands=svg=>[...svg.matchAll(/<polygon [^>]*points="([^"]+)"/g)].map(m=>m[1].split(' ').map(p=>p.split(',').map(Number)));
const reset=read('reset'),points=bands(reset);
assert.equal(points.length,1);
// Two simultaneous samples: 16,000 payload bytes, then 160 after reset.
const first=points[0][3][1]-points[0][0][1],second=points[0][2][1]-points[0][1][1];
assert(Math.abs(first/second-100)<0.01,'Snapshot occupancy must not be accumulated');
assert(reset.includes('Site contributions for r'));
assert(reset.includes('15.62 KiB'));
assert(reset.includes('Allocation sites'));
assert(reset.includes('Profiling descriptors, page headers,'));
const callback=read('callback');
assert.equal(bands(callback).length,2);
assert(callback.includes('80.00 bytes'));
for(const p of bands(callback))assert(Math.abs((p[2][1]-p[1][1])-264)<0.01,'Each site contributes 40 of 80 bytes');
assert(read('grouped').includes('Other (1 sites)'));
assert.equal(bands(read('thread')).length,2);
for(const name of ['reset','callback','grouped','thread'])assert(!/NaN|Infinity|<script/.test(read(name)));
console.log('Site SVG: snapshot values, site bands, grouping, units and scope passed');
JS
if command -v xmllint >/dev/null 2>&1; then
  for name in reset callback grouped thread; do xmllint --noout "$OUT/$name.svg"; done
fi
