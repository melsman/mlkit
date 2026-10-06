const fs=require('fs'),assert=require('assert');
const records=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse);
assert.equal(records.find(r=>r.type==='allocation_session').selector,'all');
assert(!records.some(r=>r.type==='allocation_region'),'All mode must not identify one region as selected');
const sites=new Map(records.filter(r=>r.type==='allocation_site').map(r=>[r.definition,r]));
const regions=records.filter(r=>r.type==='region'),summaries=records.filter(r=>r.type==='occupancy_summary'),rows=records.filter(r=>r.type==='allocation');
const header=records.find(r=>r.type==='header'), byInstance=new Map(), bySample=new Map();
for(const r of rows){const k=r.sample+':'+r.instance;if(!byInstance.has(k))byInstance.set(k,[]);byInstance.get(k).push(r);}
for(const r of regions){if(!bySample.has(r.sample))bySample.set(r.sample,[]);bySample.get(r.sample).push(r);}
assert(regions.length>0);assert.equal(summaries.length,regions.length,'Every region has a summary');
for(const s of summaries){
 const local=byInstance.get(s.sample+':'+s.instance)||[];
 assert.equal(local.reduce((v,r)=>v+BigInt(r.bytes),0n),BigInt(s.payload));
 assert.equal(local.reduce((v,r)=>v+BigInt(r.count),0n),BigInt(s.objects));
 assert.equal(BigInt(s.object_overhead),BigInt(s.objects)*BigInt(header.word_bytes));
 const region=bySample.get(s.sample)[s.instance];assert(region);assert.equal(region.definition,s.region_definition);
 // Page headers use two words, or three with generational GC. Large
 // storage already includes its object descriptor, but no page header.
 const pageHeaders=BigInt(region.pages)*BigInt(header.page_bytes)+BigInt(region.large_bytes)-BigInt(s.payload)-BigInt(s.object_overhead)-BigInt(s.slack);
 assert([2n,3n].some(words=>pageHeaders===BigInt(region.pages)*BigInt(header.word_bytes)*words),'Payload, descriptors and slack must reconcile with region capacity');
 assert(BigInt(s.slack)>=BigInt(region.unused_tail),'Slack includes unused tails');
 for(const r of local){assert.equal(r.region_definition,s.region_definition);assert.equal(r.thread,region.thread);assert(sites.has(r.definition));}
}
if(process.argv[3]!=='general'){
 const pair=rows.filter(r=>sites.get(r.definition).function.includes('pair__noinline'));
 assert.equal(new Set(pair.map(r=>r.definition)).size,1,'One site is shared by both regions');
 assert.equal(new Set(pair.map(r=>r.region_definition)).size,2);
 assert.deepStrictEqual([...new Set(pair.map(r=>r.sample))].map(sample=>pair.filter(r=>r.sample===sample).map(r=>r.count).sort()),[[2,3],[1,2]]);
 assert(pair.every(r=>r.bytes===r.count*16));
}
console.log('All regions: per-instance reconciliation and attribution passed');
