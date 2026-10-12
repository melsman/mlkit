const fs=require('fs'),assert=require('assert');
const rows=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse);
const samples=rows.filter(r=>r.type==='time_sample'),functions=rows.filter(r=>r.type==='code_function');
assert(samples.length>0);
const session=rows.find(r=>r.type==='time_session');
assert.equal(session.buffer_capacity,131072);
assert(session.semantics.includes('passive 16-byte'));
assert(!rows.some(r=>r.type==='sample_begin'||r.type==='allocation_session'));
assert(functions.some(f=>f.function.includes('compute__noinline')));
assert(functions.some(f=>f.source==='<runtime>'&&f.function==='tp_measure_busy'));
const arm=functions.some(f=>f.function==='mlkit_arm64_allocate_preserving');
for(const name of (arm?['mlkit_arm64_allocate_preserving','mlkit_arm64_allocate_untagged_preserving','mlkit_arm64_reset_preserving']:['__allocate','__reset_region']))assert(functions.some(f=>f.function===name));
const status=rows.filter(r=>r.type==='time_status').at(-1);
assert.equal(status.final,1);assert.equal(status.dropped,0);assert.equal(status.routing_dropped,0);
assert(samples.every(s=>s.origin_pc===0&&s.state===0),'ordinary code has no per-call markers');
const html=fs.readFileSync(process.argv[3],'utf8');
const profile=JSON.parse(html.match(/const profile=(.*?);\n/s)[1]);
assert(profile.time_samples.some(s=>s.attribution.source==='<runtime>'),'named C samples');
assert(profile.ir_documents.some(d=>d.source.endsWith('measure.sml')),'ordinary matching IR missing');
assert(profile.ir_documents.some(d=>d.source.endsWith('measure.sml')&&d.region_data.some(r=>r.kind==='function'&&r.flavor==='named')),'ordinary function labels missing');
assert(profile.time_samples.some(s=>s.attribution.status==='function'&&s.attribution.source!=='<runtime>'),'ML samples');
console.log('PASS: ordinary passive ML and C resolution without boundary instrumentation');

// Resolve actual shared ranges through the wire reader and HTML attribution.
const cp=require('child_process'),image=rows.find(r=>r.type==='code_image');
const shared=arm?[['allocation','mlkit_arm64_allocate_preserving'],['reset','mlkit_arm64_reset_preserving']]:[['allocation','__allocate'],['reset','__reset_region']];
const gc=functions.find(f=>f.function.startsWith('<gc-stub>'));
if(gc)shared.push(['gc',gc.function]);
const modified=rows.map(r=>({...r}));
const selected=modified.filter(r=>r.type==='time_sample');
for(const [i,[category,name]] of shared.entries()){
 const f=functions.find(f=>f.function===name);
 selected[i].pc=Number(BigInt(image.load_address)+BigInt(f.start));selected[i].state=0;selected[i].origin_pc=0;
}
const prefix=process.argv[2].replace(/\.json$/,'')+'-categories';
fs.writeFileSync(prefix+'.json',modified.map(JSON.stringify).join('\n')+'\n');
for(const [tool,args] of [[process.argv[5],[prefix+'.json',prefix+'.rp']],[process.argv[4],[prefix+'.rp','-o',prefix+'.html']]]){
 const result=cp.spawnSync(tool,args,{encoding:'utf8'});assert.equal(result.status,0,result.stderr);
}
const categorized=JSON.parse(fs.readFileSync(prefix+'.html','utf8').match(/const profile=(.*?);\n/s)[1]);
for(const [i,[category]] of shared.entries())assert.equal(categorized.time_samples[i].attribution.status,category);
console.log('PASS: exact shared Allocation/Reset/GC ranges resolve to category rows');
