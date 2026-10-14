const fs=require('fs'),assert=require('assert');
const rows=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse),scenario=process.argv[3];
const session=rows.find(r=>r.type==='time_session'),samples=rows.filter(r=>r.type==='time_sample'),events=rows.filter(r=>r.type==='time_thread'),final=rows.filter(r=>r.type==='time_status').at(-1);
assert.equal(session.time_version,3);
assert(events.filter(e=>e.event==='end'&&e.thread!==0).every(e=>e.cpu_ns>0&&BigInt(e.cpu_ns)<18446744073709551615n),'worker CPU accounting unavailable');
assert(events.every(e=>e.stream===e.thread),'pthread stream identity');assert.equal(session.buffer_capacity,scenario==='overflow'?4:131072);
assert.equal(final.final,1);assert.equal(final.recorded,samples.length);assert.equal(final.routing_dropped,0);
const starts=events.filter(e=>e.event==='start'),ends=events.filter(e=>e.event==='end');assert.equal(starts.length,9);assert.equal(ends.length,9);
let totalDrops=0;
for(const start of starts){const end=ends.find(e=>e.thread===start.thread),selected=samples.filter(s=>s.thread===start.thread);assert(end);assert(selected.length>0,'thread never sampled');assert.equal(end.recorded,selected.length);totalDrops+=end.dropped;let last=start.time;for(const s of selected){assert(s.time>=last&&s.time<=end.time);assert(s.pc>0);assert.equal(s.state,0);assert.equal(s.origin_pc,0);last=s.time;}}
assert.equal(totalDrops,final.dropped);
if(scenario==='overflow')assert(final.dropped>0);else assert.equal(final.dropped,0);
assert.equal(rows.some(r=>r.type==='sample_begin'),scenario==='combined');
// Balanced overlapping workers must all receive targeted delivery, independently of main.
if(scenario!=='overflow'){for(const group of [starts.slice(1,5),starts.slice(5)]){const counts=group.map(t=>samples.filter(s=>s.thread===t.thread).length);assert(Math.min(...counts)>Math.max(...counts)*0.2,'strong delivery imbalance');}}
console.log('PASS: registered lifetimes, targeted worker delivery, per-stream chronology, counts and loss');
// Version-3 streams cannot silently accept unknown IDs or ended/duplicate lifetimes.
if(scenario==='normal') {
 const cp=require('child_process'),pview=process.env.PVIEW||require('path').resolve(__dirname,'../../bin/pview');
 const file=process.argv[2].replace(/\.json$/,'.rp'),original=fs.readFileSync(file),frames=[];
 for(let offset=8;offset<original.length;){const size=original.readUInt32LE(offset);frames.push({offset,size,tag:original[offset+4]});offset+=size+4;}
 const sample=frames.find(f=>f.tag===24),start=frames.find(f=>f.tag===26),end=frames.filter(f=>f.tag===26).at(-1);
 for(const [name,frame,field,value] of [['thread',sample,3,999n],['stream',sample,4,99n],['timestamp',sample,1,0n],['state',sample,5,3n],['end-count',end,3,0n],['start-count',start,3,1n]]) {
  const bytes=Buffer.from(original);bytes.writeBigUInt64LE(value,frame.offset+5+field*8);const mutated=file+'-'+name;fs.writeFileSync(mutated,bytes);assert.notEqual(cp.spawnSync(pview,[mutated,'--format','json'],{stdio:'ignore'}).status,0,name);
 }
 console.log('PASS: malformed parallel identities, chronology, attribution and lifecycle rejected');
}
