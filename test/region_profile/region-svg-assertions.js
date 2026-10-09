const fs=require('fs'),assert=require('assert'),cp=require('child_process');
const [viewer,dir]=process.argv.slice(2);
const records=fs.readFileSync(dir+'/all.json','utf8').trim().split('\n').map(JSON.parse);
const sites=new Map(records.filter(r=>r.type==='allocation_site').map(r=>[r.definition,r]));
const pair=records.filter(r=>r.type==='allocation'&&sites.get(r.definition).function.includes('pair__noinline'));
const defs=records.filter(r=>r.type==='binding');
function render(args){return cp.spawnSync(viewer,[dir+'/all.rp',...args,'-o',dir+'/selected.svg'],{encoding:'utf8'});}
for(const id of new Set(pair.map(r=>r.region_definition))){
  const binding=defs.find(r=>r.definition===id).binding;
  const result=render(['--region','r'+binding]);
  assert.equal(result.status,0,result.stderr);
  const svg=fs.readFileSync(dir+'/selected.svg','utf8');
  assert(svg.includes('Site contributions for r'+binding+' in'));
  const rows=records.filter(r=>r.type==='occupancy_summary'&&r.region_definition===id);
  const totals=new Map();for(const r of rows)totals.set(r.sample,(totals.get(r.sample)||0)+r.payload);
  const peak=Math.max(...totals.values());
  assert(svg.includes('Sampled maximum: '+peak.toFixed(2)+' bytes'),'Only selected region payload must be included');
  assert.equal((svg.match(/<polygon /g)||[]).length,1,'Shared site must have one filtered band');
}
// Empty, but recorded, global regions produce valid empty graphs.
const empty=defs.find(d=>d.unit==='<global>'&&records.some(r=>r.type==='occupancy_summary'&&r.region_definition===d.definition)&&!records.some(r=>r.type==='allocation'&&r.region_definition===d.definition));
assert(empty);
assert.equal(render(['--region','r'+empty.binding]).status,0);
assert(fs.readFileSync(dir+'/selected.svg','utf8').includes('Sampled maximum: 0.00 bytes'));
for(const args of [['--sites','--region','r999999999999999999'],['--sites','--region',''],['--sites','--region','163'],['--sites','--region','r-1'],['--sites','--region'],['--sites','--region','r1','--format','html']]){
  assert.notEqual(render(args).status,0,'Must reject '+JSON.stringify(args));
}
const binding=defs.find(r=>r.definition===pair[0].region_definition).binding;
const implicit=cp.spawnSync(viewer,[dir+'/all.rp','--region','r'+binding],{cwd:dir,encoding:'utf8'});
assert.equal(implicit.status,0,implicit.stderr);
assert(fs.readFileSync(dir+'/profile.svg','utf8').includes('Site contributions for r'+binding+' in'));
const scoped=render(['--region','r'+binding,'--scope','thread:0']);
assert.equal(scoped.status,0,scoped.stderr);
assert(fs.readFileSync(dir+'/selected.svg','utf8').includes('View: Thread 0'));
console.log('Region SVG: region-filtered shared sites, empty regions and option validation passed');
