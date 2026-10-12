// T6b/T6c/T6d: opt-in serial measurements; timing observations are not CI thresholds.
const fs = require('fs'), path = require('path'), cp = require('child_process');
const os = require('os'), crypto = require('crypto'), assert = require('assert');
const [out, rpview, repetitions = '5'] = process.argv.slice(2);
const repeats = Number(repetitions);
assert(Number.isInteger(repeats) && repeats >= 3);
const results = [], checksums = new Map();
const attributionStudy = process.argv.includes("--attribution");
const settings = attributionStudy ? [
 {name:'off'}, {name:'profiled-off', instrumented:true},
 {name:'1ms', interval:'1ms', phasesOnly:true},
 {name:'100us', interval:'100us'},
 {name:'10us', interval:'10us', phasesOnly:true},
 {name:'100us-65536', interval:'100us', capacity:65536, phasesOnly:true},
 {name:'profiled-passive', instrumented:true, interval:'100us', attribution:'passive'},
 {name:'profiled-detailed', instrumented:true, interval:'100us', attribution:'detailed'},
 {name:'combined-passive', instrumented:true, region:true, interval:'100us', attribution:'passive'},
 {name:'combined-detailed', instrumented:true, region:true, interval:'100us', attribution:'detailed'}
] : [
 {name:'off'}, {name:'paused', interval:'1ms', paused:true},
 ...['1ms','100us','10us'].map(interval => ({name:interval, interval})),
 {name:'100us-small', interval:'100us', capacity:64, phasesOnly:true},
 {name:'10us-4096', interval:'10us', capacity:4096, phasesOnly:true},
 {name:'10us-max', interval:'10us', capacity:1048576, phasesOnly:true},
 {name:'profiled-off', instrumented:true},
 {name:'region-only', instrumented:true, region:true},
 {name:'combined-100us', instrumented:true, region:true, interval:'100us'}
];
let seed = 241;
function shuffle(xs) {
 const a = [...xs];
 for (let i = a.length-1; i > 0; --i) {
  seed = (Math.imul(seed,1664525)+1013904223) >>> 0;
  const j = seed % (i+1); [a[i],a[j]] = [a[j],a[i]];
 }
 return a;
}
for (const config of ['gc','gengc','no_gc']) for (const workload of ['phases','kkb','mlyacc']) {
 const args = workload === 'mlyacc' ? [path.join(out,'Topdec.grm')] : [];
 const executable = instrumented => path.join(out,`${workload}-${config}-${instrumented?'instrumented':'plain'}`);
 assert.equal(cp.spawnSync(executable(false),args,{stdio:'ignore'}).status,0);
 for (let repeat = 0; repeat < repeats; ++repeat) for (const setting of shuffle(settings.filter(s => (!s.phasesOnly || workload==='phases') && !(s.paused && workload==='phases')))) {
  const stem = path.join(out,`${workload}-${config}-${setting.name}-${repeat}`), profile = stem+'.rp';
  const options = [];
  if (setting.interval) options.push('-tp','-tp_interval',setting.interval,...(!attributionStudy || setting.capacity ? ['-tp_buffer',String(setting.capacity || 65536)] : []),'-tp_file',profile,...(setting.attribution ? ['-tp_attribution',setting.attribution] : []),...(setting.paused || workload==='phases' ? ['-tp_paused'] : []));
  if (setting.region) options.push('-rp','-rp_interval','10ms','-rp_file',profile);
  const begin = process.hrtime.bigint();
  const run = cp.spawnSync('/usr/bin/time',['-l','-p',executable(setting.instrumented),...args,...(options.length?['+RTS',...options]:[])],{encoding:'utf8',maxBuffer:32*1024*1024});
  const elapsed = Number(process.hrtime.bigint()-begin)/1e9;
  fs.writeFileSync(stem+'.stdout',run.stdout || ''); fs.writeFileSync(stem+'.stderr',run.stderr || '');
  assert.equal(run.status,0,stem+'\n'+run.stderr);
  const stable = workload==='phases' ? run.stdout.replace(/^c .*\n/gm,'') : run.stdout;
  const checksum = crypto.createHash('sha256').update(stable).digest('hex'), key=workload+config;
  if(checksums.has(key)) assert.equal(checksum,checksums.get(key),'Changed output: '+stem);
  checksums.set(key,checksum);
  const time = Object.fromEntries([...run.stderr.matchAll(/^(real|user|sys)\s+(\d+(?:\.\d+)?)/gm)].map(m => [m[1],Number(m[2])]));
  assert(['real','user','sys'].every(k => Number.isFinite(time[k])), 'Missing resource timing: '+stem);
  assert(Number(run.stderr.match(/(\d+)\s+maximum resident set size/)?.[1]) > 0, 'Missing peak RSS: '+stem);
  const row = {workload,config,setting:setting.name,repeat,elapsed_s:elapsed,...time,rss_bytes:Number(run.stderr.match(/(\d+)\s+maximum resident set size/)?.[1]),executable_bytes:fs.statSync(executable(setting.instrumented)).size};
  if (options.length) {
   const decoded = cp.spawnSync(rpview,[profile,'--format','json','-o',stem+'.json'],{encoding:'utf8',maxBuffer:1024*1024});
   assert.equal(decoded.status,0,decoded.stderr);
   const records = fs.readFileSync(stem+'.json','utf8').trim().split('\n').map(JSON.parse);
   const samples = records.filter(r=>r.type==='time_sample'), statuses=records.filter(r=>r.type==='time_status'), status=statuses.at(-1);
   row.region_snapshots = records.filter(r=>r.type==='sample_begin').length;
   row.output_bytes = fs.statSync(profile).size;
   if (setting.region) assert(row.region_snapshots>0,'No region snapshots: '+stem);
   else assert.equal(row.region_snapshots,0);
   if (setting.interval) {
    assert.equal(status.final,1); assert.equal(status.recorded,samples.length);
    const session = records.find(r=>r.type==='time_session');
    const detailed = session.semantics.includes('detailed 32-byte');
    Object.assign(row,{capacity:session.buffer_capacity,record_bytes:detailed?32:16,samples:samples.length,buffer_drops:status.dropped,routing_drops:status.routing_dropped});
    if(attributionStudy) {
     assert.equal(session.buffer_capacity,setting.capacity || (detailed?65536:131072));
     assert.equal(detailed,!!setting.instrumented && setting.attribution!=='passive');
     if(!detailed) assert(samples.every(s=>s.state===0 && s.origin_pc===0));
     else assert(samples.some(s=>s.state===2 && s.origin_pc>0));
    }
    if (setting.paused) assert.equal(samples.length,0);
    else assert(samples.length>0);
    const image=records.find(r=>r.type==='code_image'), base=BigInt(image.load_address);
    const functions=records.filter(r=>r.type==='code_function').map(f=>({...f,start:base+BigInt(f.start),end:base+BigInt(f.end)})).sort((a,b)=>a.start<b.start?-1:1);
    function resolve(pc) { const n=BigInt(pc);let lo=0,hi=functions.length;while(lo<hi){const mid=(lo+hi)>>>1;if(functions[mid].start<=n)lo=mid+1;else hi=mid;}const f=functions[lo-1];return f&&n<f.end?f:null; }
    const counts={ml:0,c:0,unknown:0};
    for(const sample of samples){const f=resolve(sample.pc);counts[f?(f.source==='<runtime>'?'c':'ml'):'unknown']++;}
    row.raw_pc_counts=counts;
    row.state_counts=[0,1,2,3].map(state=>samples.filter(s=>s.state===state).length);
    row.c_origin_fallback_samples=samples.filter(s=>s.state===2 && !resolve(s.pc) && resolve(s.origin_pc)).length;
    if(workload==='phases') {
     const intervalNs={'1ms':1000000,'100us':100000,'10us':10000}[setting.interval];
     const phases=[];let start=null;
     for(const end of statuses) {
      if(end.active===1 && !start) start=end;
      else if(end.active===0 && start) {
       const selected=samples.filter(s=>s.sequence>start.recorded && s.sequence<=end.recorded);
       const duration=end.time-start.time, name=['compute','allocate','c','sleep'][phases.length];
       phases.push({name,duration_ns:duration,samples:selected.length,origin_fallback_samples:selected.filter(s=>s.state===2 && !resolve(s.pc) && resolve(s.origin_pc)).length,dropped:end.dropped-start.dropped,capture_ratio:selected.length*intervalNs/duration,busy_c_samples:selected.filter(s=>resolve(s.pc)?.function==='tp_measure_busy').length});start=null;
      }
     }
     assert.equal(phases.length,4);row.phases=phases;
    }
   }
  }
  results.push(row);
  fs.writeFileSync(path.join(out,'measurements.json'),JSON.stringify({host:{platform:process.platform,controller_arch:process.arch,benchmark_arch:'arm64',release:os.release(),cpu:os.cpus()[0]?.model},repeats,attributionStudy,settings,results},null,2)+'\n');
  console.log(`${workload} ${config} ${setting.name} #${repeat}: ${time.real}s, ${row.samples ?? '-'} samples, ${row.buffer_drops ?? 0} drops`);
 }
}
const median = xs => [...xs].sort((a,b)=>a-b)[Math.floor(xs.length/2)];
const summary=[];
for(const config of ['gc','gengc','no_gc']) for(const workload of ['phases','kkb','mlyacc']) for(const setting of settings) {
 const rows=results.filter(r=>r.config===config && r.workload===workload && r.setting===setting.name);if(!rows.length)continue;
 const baseline=median(results.filter(r=>r.config===config&&r.workload===workload&&r.setting==='off').map(r=>r.real));
 summary.push({config,workload,setting:setting.name,median_s:median(rows.map(r=>r.real)),min_s:Math.min(...rows.map(r=>r.real)),max_s:Math.max(...rows.map(r=>r.real)),overhead_percent:100*(median(rows.map(r=>r.real))/baseline-1),median_samples:median(rows.map(r=>r.samples || 0)),median_rss_bytes:median(rows.map(r=>r.rss_bytes)),median_output_bytes:median(rows.map(r=>r.output_bytes || 0)),drops:rows.reduce((n,r)=>n+(r.buffer_drops || 0)+(r.routing_drops || 0),0)});
}
fs.writeFileSync(path.join(out,'summary.json'),JSON.stringify(summary,null,2)+'\n');
console.table(summary);

// Logical file sizes from fresh isolated ML-Yacc caches; exclude shared Basis caches.
if (attributionStudy) {
 function cacheSize(directory) {
  const result={files:0,bytes:0,object_bytes:0,ir_bytes:0};
  for(const entry of fs.readdirSync(directory,{withFileTypes:true})) {
   const file=path.join(directory,entry.name);
   if(entry.isDirectory()) {const child=cacheSize(file);for(const key of Object.keys(result))result[key]+=child[key];}
   else if(entry.isFile()) {const bytes=fs.statSync(file).size;result.files++;result.bytes+=bytes;if(file.endsWith('.o'))result.object_bytes+=bytes;if(file.endsWith('.ir'))result.ir_bytes+=bytes;}
  }
  return result;
 }
 const caches=[];
 for(const [config,ordinary,instrumented] of [['gc','ARM64_RI_GC','ARM64_RI_GC_PROF_CODE1_TP1'],['gengc','ARM64_RI_GENGC','ARM64_RI_GENGC_PROF_CODE1_TP1'],['no_gc','ARM64_RI','ARM64_RI_PROF_CODE1_TP1']])for(const [mode,folder] of [['ordinary',ordinary],['instrumented',instrumented]]) {
  const size={files:0,bytes:0,object_bytes:0,ir_bytes:0};
  for(const root of ['src','lib']) {const child=cacheSize(path.join(out,'yacc-source',root,'MLB',folder));for(const key of Object.keys(size))size[key]+=child[key];}
  caches.push({config,mode,folder,...size});
 }
 fs.writeFileSync(path.join(out,'cache-sizes.json'),JSON.stringify({scope:'fresh isolated ML-Yacc src/lib caches; logical bytes; excludes shared Basis and main-driver caches',caches},null,2)+'\n');
}
