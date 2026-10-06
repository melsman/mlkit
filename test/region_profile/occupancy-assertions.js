// Validate sampled occupancy independently of the renderer.
const fs = require('fs');
const assert = require('assert');
const records = fs.readFileSync(process.argv[2], 'utf8').trim().split('\n').map(JSON.parse);
assert.equal(records[0].version, 10);
assert(records.filter(r => r.type === "allocation_site").every(r => !("point" in r)), "IR5 has no site-to-point mapping");
const sites = new Map(records.filter(r => r.type === 'allocation_site').map(r => [r.definition, r]));
const rows = records.filter(r => r.type === 'allocation');
const summaries = records.filter(r => r.type === 'occupancy_summary');
assert(summaries.length, 'Missing selected-region snapshots');
for (const s of summaries) {
  const local = rows.filter(r => r.sample === s.sample && r.instance === s.instance);
  assert.equal(local.reduce((n,r) => n+r.bytes,0),s.payload);
  assert.equal(local.reduce((n,r) => n+r.count,0),s.objects);
  assert.equal(s.object_overhead,s.objects*8);
  for (const r of local) {
    assert(sites.has(r.definition));
    assert.equal(r.region_definition,s.region_definition);
    assert(sites.get(r.definition).site > 0, 'Expected a compiler allocation site');
  }
}
if (process.argv[3] === 'reset') {
  assert.deepStrictEqual(summaries.map(s => [s.objects,s.payload]),[[1000,16000],[10,160]]);
  assert.equal(new Set(rows.map(r => r.definition)).size,1,'Fast/slow allocations must share their site');
} else if (process.argv[3] === 'large') {
  assert.equal(summaries[0].objects,2);
  assert.equal(summaries[0].payload,9048);
  assert(rows.every(r => sites.get(r.definition).location_kind === 1),'Foreign allocation origin lost');
 } else if (process.argv[3] === 'callback') {
  assert.equal(summaries[0].objects,4);
  assert.equal(summaries[0].payload,80);
  assert.deepStrictEqual(rows.map(r => [r.count,r.bytes]).sort((a,b) => a[0]-b[0]),[[1,40],[3,40]]);
  assert(rows.every(r => sites.get(r.definition).location_kind === 1));
} else if (process.argv[3] === 'parallel') {
  assert(records.filter(r => r.type === 'thread_start').length >= 5);
  assert(summaries.length >= 4);
  assert(rows.some(r => r.count > 0));
} else if (['gc','gengc','tagged'].includes(process.argv[3])) {
  assert.equal(summaries[0].objects,100000);
  assert.equal(summaries[0].payload,process.argv[3] === 'tagged' ? 2400000 : 1600000);
  assert(records.some(r => r.type === 'session_end' && r.gc_collections > 0));
}
console.log('Occupancy accounting:',process.argv[3],'passed');
