const fs=require('fs'),assert=require('assert');
const rows=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse),samples=rows.filter(r=>r.type==='time_sample');
const phases=[];let start=null;
for(const r of rows.filter(r=>r.type==='time_status')){
 if(r.active===1&&!start)start=r;
 else if(r.active===0&&start){phases.push(samples.filter(s=>s.sequence>start.recorded&&s.sequence<=r.recorded));start=null;}
}
assert.equal(phases.length,4,'Four explicitly delimited phases');
for(const phase of phases)assert(phase.length>0,'Each long phase must receive samples');
const images=new Map(rows.filter(r=>r.type==='code_image').map(r=>[r.image,r.load_address]));
const compute=rows.filter(r=>r.type==='code_function'&&r.function.includes('compute__noinline'));
const inCompute=pc=>compute.some(f=>pc>=images.get(f.image)+f.start&&pc<images.get(f.image)+f.end);
// Integer modulo may enter a C helper; count that helper's initiating ML PC.
assert(phases[0].filter(s=>inCompute(s.pc)||(s.state===2&&inCompute(s.origin_pc))).length>phases[0].length*.75,'Compute phase attributed to its ML function');
for(const index of [2,3])assert(phases[index].filter(s=>s.state===2).length>phases[index].length*.75,'C and sleep predominantly foreign extent');
if(process.argv[3]==='gc')assert(phases[1].some(s=>s.state===3),'Allocation phase observes GC');
const status=rows.filter(r=>r.type==='time_status').at(-1);
assert.equal(status.final,1);assert.equal(status.dropped,0);assert.equal(status.routing_dropped,0);
assert(samples.every(s=>s.thread===0&&s.stream===0));
console.log('PASS: compute/allocation/C/sleep phase boundaries and attribution');
