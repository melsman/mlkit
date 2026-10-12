// Linux T9: interleaved repeat measurements on one allowed CPU. No timing gates.
const fs=require('fs'),cp=require('child_process'),os=require('os'),path=require('path'),a=require('assert');
const [out,viewer,repetitions='5']=process.argv.slice(2),repeats=Number(repetitions);
a(Number.isInteger(repeats)&&repeats>=3);
const cpu=process.env.T9_CPU||fs.readFileSync('/proc/self/status','utf8').match(/^Cpus_allowed_list:\s*(\d+)/m)[1];
const settings=[];
for(const build of ['ordinary','instrumented'])for(const mode of ['off','paused','1ms','100us','10us'])
 settings.push({build,mode,attribution:build==='ordinary'?'passive':'detailed'});
for(const mode of ['1ms','100us'])settings.push({build:'instrumented',mode,attribution:'passive'});
settings.push({build:'ordinary',mode:'100us',attribution:'passive',capacity:64});
settings.push({build:'ordinary',mode:'10us',attribution:'passive',capacity:1048576});
const results=[],checksums=new Map();
let random=241;
function shuffle(xs){xs=xs.slice();for(let i=xs.length-1;i>0;i--){random=(Math.imul(random,1664525)+1013904223)>>>0;const j=random%(i+1);[xs[i],xs[j]]=[xs[j],xs[i]];}return xs;}
for(const config of ['gc','gengc','no_gc']) {
 for(const build of ['ordinary','instrumented']) {
  const run=cp.spawnSync('taskset',['-c',cpu,path.join(out,`${config}-${build}`)],{encoding:'utf8',timeout:30000});
  a.equal(run.status,0,run.stderr);
 }
 for(let repeat=0;repeat<repeats;repeat++)for(const setting of shuffle(settings)) {
  const id=`${config}-${setting.build}-${setting.attribution}-${setting.mode}-${setting.capacity||'default'}-${repeat}`;
  const stem=path.join(out,id),profile=stem+'.prof';
  const options=setting.mode==='off'?[]:['+RTS','-tp','-tp_attribution',setting.attribution,'-p_file',profile,
   ...(setting.mode==='paused'?['-tp_paused']:['-tp_interval',setting.mode]),...(setting.capacity?['-tp_buffer',String(setting.capacity)]:[])];
  const begin=process.hrtime.bigint();
  const run=cp.spawnSync('taskset',['-c',cpu,'/usr/bin/time','-o',stem+'.usage','-f','%e %U %S %M',path.join(out,`${config}-${setting.build}`),...options],{encoding:'utf8',timeout:30000});
  const wall=Number(process.hrtime.bigint()-begin)/1e9;
  a.equal(run.status,0,`${id}: ${run.stderr}`);
  const checksum=run.stdout.split('\n').filter(x=>!x.startsWith('c ')).join('\n');
  if(checksums.has(config))a.equal(checksum,checksums.get(config));else checksums.set(config,checksum);
  const [childWall,user,system,rss]=fs.readFileSync(stem+'.usage','utf8').trim().split(/\s+/).map(Number);
  const result={config,...setting,repeat,cpu:Number(cpu),parent_wall_s:wall,child_wall_s:childWall,user_s:user,system_s:system,peak_rss_kib:rss,executable_bytes:fs.statSync(path.join(out,`${config}-${setting.build}`)).size};
  if(options.length) {
   const view=cp.spawnSync(viewer,[profile,'--format','json','-o',stem+'.json'],{encoding:'utf8',timeout:30000});a.equal(view.status,0,view.stderr);
   const rows=fs.readFileSync(stem+'.json','utf8').trim().split('\n').map(JSON.parse);
   const samples=rows.filter(x=>x.type==='time_sample'),status=rows.filter(x=>x.type==='time_status').at(-1),session=rows.find(x=>x.type==='time_session');
   a.equal(status.final,1);a.equal(status.recorded,samples.length);
   if(setting.mode==='paused')a.equal(samples.length,0);else a(samples.length>0);
   const first=rows.find(x=>x.type==='time_status');
   Object.assign(result,{samples:samples.length,buffer_capacity:session.buffer_capacity,interval_ns:session.interval_ns,dropped:status.dropped,routing_dropped:status.routing_dropped,profile_bytes:fs.statSync(profile).size,
    recorded_wall_fraction:setting.mode==='paused'?null:samples.length*session.interval_ns/(status.time-first.time),
    recorder_semantics:session.semantics});
  }
  results.push(result);
  fs.writeFileSync(path.join(out,'measurements.json'),JSON.stringify({protocol:'T9 Linux X64 native, shuffled per repeat, taskset one allowed CPU; /usr/bin/time CPU/wall have centisecond precision; peak RSS KiB; coalescing unobservable',host:{platform:os.platform(),release:os.release(),cpu:os.cpus()[Number(cpu)].model},repeats,results},null,2)+'\n');
 }
}
const median=xs=>xs.slice().sort((a,b)=>a-b)[Math.floor(xs.length/2)];
const summary=[];
for(const config of ['gc','gengc','no_gc'])for(const setting of settings) {
 const rs=results.filter(r=>r.config===config&&r.build===setting.build&&r.mode===setting.mode&&r.attribution===setting.attribution&&r.capacity===setting.capacity);
 summary.push({config,...setting,runs:rs.length,median_parent_wall_s:median(rs.map(r=>r.parent_wall_s)),median_cpu_s:median(rs.map(r=>r.user_s+r.system_s)),median_peak_rss_kib:median(rs.map(r=>r.peak_rss_kib)),samples:rs.map(r=>r.samples??0),drops:rs.map(r=>r.dropped??0),routing_drops:rs.map(r=>r.routing_dropped??0)});
}
fs.writeFileSync(path.join(out,'summary.json'),JSON.stringify(summary,null,2)+'\n');
console.log(`PASS: ${results.length} Linux measurement runs, output parity and profile consistency (${out})`);
