const fs=require('fs'),cp=require('child_process'),a=require('assert');
const [out,viewer]=process.argv.slice(2);
const rows=name=>fs.readFileSync(`${out}/${name}.json`,'utf8').trim().split('\n').map(JSON.parse);
const first=rows('first'),second=rows('second'),stripped=rows('stripped');
const image=r=>r.find(x=>x.type==='code_image');
a.equal(image(first).build_id,fs.readFileSync(`${out}/notes.txt`,'utf8').match(/Build ID: ([0-9a-f]+)/)[1]);
a.equal(image(first).build_id.length,40);
a.equal(image(first).build_id,image(second).build_id);
a.notEqual(image(first).load_address,image(second).load_address,'PIE executable did not relocate');
for(const records of [first,second,stripped]) {
 const functions=records.filter(r=>r.type==='code_function');
 a(functions.some(f=>f.function.includes('compute__noinline')));
 for(const name of ['gc','resetRegion','__allocate','__reset_region'])a(functions.some(f=>f.function===name),`missing retained ${name}`);
 const metadata=records.find(r=>r.type==='code_metadata');
 a.equal(metadata.function_count,functions.length);
 const samples=records.filter(r=>r.type==='time_sample');a(samples.length>20);
 a(samples.every(s=>s.state===0&&s.origin_pc===0));
 const resolve=(pc,build)=>{
  const result=cp.spawnSync(viewer,[`${out}/first.prof`,'--resolve-pc',String(pc),'--image-build',build||image(first).build_id],{encoding:'utf8'});
  a.equal(result.status,0,result.stderr);return JSON.parse(result.stdout);
 };
 if(records===first) {
  const fn=functions.find(f=>f.function==='tp_measure_busy');a(fn);
  const start=BigInt(image(records).load_address)+BigInt(fn.start);
  a.equal(resolve(start).function,fn.function);
  a.equal(resolve(start,'0'.repeat(40)).status,'build-mismatch');
  a.equal(resolve(1).status,'unknown-pc');
 }
}
const profile=JSON.parse(fs.readFileSync(`${out}/first.html`,'utf8').match(/const profile=(.*?);\n/s)[1]);
a(profile.ir_documents.some(d=>d.source.endsWith('measure.sml')));
a(profile.time_samples.some(s=>s.attribution.source==='<runtime>'));
a(profile.time_samples.some(s=>s.attribution.status==='function'&&s.attribution.source!=='<runtime>'));
console.log('PASS: ELF identity, relocated sampling, retained stripped bounds and matching IR');
