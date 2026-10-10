// Repeated opt-in measurements. No timing thresholds in ordinary CI.
const fs=require('fs'),path=require('path'),cp=require('child_process'),os=require('os'),crypto=require('crypto');
const [out,rpview,repeatsArg]=process.argv.slice(2),repeats=Number(repeatsArg||5);
if(!Number.isInteger(repeats)||repeats<3)throw Error('At least three repetitions are required');
const settings=process.argv.includes('--large-buffer')?['10us-large']:['plain','off','paused','1ms','100us','10us','10us-large'],results=[],checksums=new Map();
const workloads=['phases','kkb','mlyacc','callbacks'].filter(w=>fs.existsSync(path.join(out,w+'-gc-instrumented')));
const median=xs=>[...xs].sort((a,b)=>a-b)[Math.floor(xs.length/2)];
let seed=241;function shuffle(xs){const result=[...xs];for(let i=result.length-1;i>0;i--){seed=(Math.imul(seed,1664525)+1013904223)>>>0;const j=seed%(i+1);[result[i],result[j]]=[result[j],result[i]];}return result;}
function run(workload,config,setting,repeat){
 const stem=path.join(out,`${workload}-${config}-${setting}-${repeat}`),exe=path.join(out,`${workload}-${config}-${setting==='plain'?'plain':'instrumented'}`),profile=stem+'.rp';
 const args=workload==='mlyacc'?[path.join(out,'Topdec.grm')]:[];
 if(!['plain','off'].includes(setting))args.push('+RTS','-tp','-tp_file',profile,'-tp_interval',setting==='paused'?'1ms':setting==='10us-large'?'10us':setting,...(setting==='paused'||workload==='phases'?['-tp_paused']:[]),...(setting==='10us-large'?['-tp_buffer','65536']:[]));
 const begin=process.hrtime.bigint(),run=cp.spawnSync('/usr/bin/time',['-p',exe,...args],{encoding:'utf8',maxBuffer:32*1024*1024});
 const elapsed=Number(process.hrtime.bigint()-begin)/1e9;
 fs.writeFileSync(stem+'.stdout',run.stdout||'');fs.writeFileSync(stem+'.stderr',run.stderr||'');
 if(run.status!==0)throw Error(`Failed ${stem}: ${run.stderr}`);
 if(workload!=='phases'){
  const checksum=crypto.createHash('sha256').update(run.stdout).digest('hex'),key=workload+config;
  if(checksums.has(key)&&checksums.get(key)!==checksum)throw Error('Output changed: '+stem);checksums.set(key,checksum);
 }
 const time=Object.fromEntries([...run.stderr.matchAll(/^(real|user|sys)\s+(\d+(?:\.\d+)?)/gm)].map(m=>[m[1],Number(m[2])]));
 const row={workload,config,setting,repeat,elapsed_s:elapsed,...time};
 if(fs.existsSync(profile)){
  const json=stem+'.json',read=cp.spawnSync(rpview,[profile,'--format','json','-o',json],{encoding:'utf8'});if(read.status!==0)throw Error(read.stderr);
  const records=fs.readFileSync(json,'utf8').trim().split('\n').map(JSON.parse),samples=records.filter(r=>r.type==='time_sample'),statuses=records.filter(r=>r.type==='time_status'),status=statuses.at(-1);
  Object.assign(row,{samples:samples.length,buffer_drops:status.dropped,routing_drops:status.routing_dropped,output_bytes:fs.statSync(profile).size,recorder_samples:samples.filter(s=>s.state===1).length,gc_samples:samples.filter(s=>s.state===3).length});
  if(workload==='phases'&&setting!=='paused'){
   const phases=[];let start=null;
   for(const status of statuses){if(status.active===1&&!start)start=status;else if(status.active===0&&start){const selected=samples.filter(s=>s.sequence>start.recorded&&s.sequence<=status.recorded);phases.push({name:['compute','allocate','c','sleep'][phases.length],duration_ns:status.time-start.time,samples:selected.length,states:[0,1,2,3].map(state=>selected.filter(s=>s.state===state).length)});start=null;}}
   if(phases.length!==4)throw Error('Missing phase boundaries: '+stem);row.phases=phases;
  }
 }
 results.push(row);fs.writeFileSync(path.join(out,'measurements.json'),JSON.stringify({host:{platform:process.platform,controller_arch:process.arch,benchmark_arch:'arm64',release:os.release(),cpu:os.cpus()[0]?.model},repeats,results},null,2));
 console.log(`${workload} ${config} ${setting} #${repeat}: ${elapsed.toFixed(3)}s, ${row.samples??'-'} samples`);
}
for(const config of ['gc','no_gc'])for(const workload of workloads){
 const options=settings.filter(s=>!(s==='10us-large'&&workload!=='phases')&&!(workload==='phases'&&s==='paused')&&!(workload==='callbacks'&&['plain','paused'].includes(s)));
 // One unsampled warm-up for code/file caches; every reported run is retained.
 const exe=path.join(out,`${workload}-${config}-${workload==='callbacks'?'instrumented':'plain'}`),args=workload==='mlyacc'?[path.join(out,'Topdec.grm')]:[];
 if(cp.spawnSync(exe,args,{stdio:'ignore'}).status!==0)throw Error('Warm-up failed');
 for(let i=0;i<repeats;i++)for(const setting of shuffle(options))run(workload,config,setting,i);
}
const summary=[];
for(const config of ['gc','no_gc'])for(const workload of workloads)for(const setting of settings){
 const rows=results.filter(r=>r.config===config&&r.workload===workload&&r.setting===setting);if(!rows.length)continue;
 const baselineRows=results.filter(r=>r.config===config&&r.workload===workload&&r.setting===(workload==='callbacks'?'off':'plain')).map(r=>(r.real??r.elapsed_s));const baseline=baselineRows.length?median(baselineRows):null;
 summary.push({workload,config,setting,median_s:median(rows.map(r=>(r.real??r.elapsed_s))),min_s:Math.min(...rows.map(r=>(r.real??r.elapsed_s))),max_s:Math.max(...rows.map(r=>(r.real??r.elapsed_s))),overhead_percent:baseline===null?null:100*(median(rows.map(r=>(r.real??r.elapsed_s)))/baseline-1),median_samples:median(rows.map(r=>r.samples||0)),median_bytes:median(rows.map(r=>r.output_bytes||0)),drops:rows.reduce((n,r)=>n+(r.buffer_drops||0)+(r.routing_drops||0),0)});
}
fs.writeFileSync(path.join(out,'summary.json'),JSON.stringify(summary,null,2));console.table(summary);
