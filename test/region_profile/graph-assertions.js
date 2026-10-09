el('metric').value='stack';draw();
assert.deepStrictEqual(model().totals,samples.map(s=>s.stacks.reduce((n,r)=>n+BigInt(r.stack_bytes),0n)+s.regions.filter(r=>r.kind==='finite').reduce((n,r)=>n+BigInt(r.finite_bytes),0n)));
assert(model().bands.every(b=>b.key==='stack'||samples.some(s=>s.regions.some(r=>r.kind==='finite'&&regionKey(r)===b.key))));
assert(!el('chart').children.some(n=>n.attrs['data-peak']));
el('metric').value='total';draw();
const originalReasons=samples.map(s=>s.reason);
samples[0].reason='before_gc';samples[1].reason='after_gc';draw();
const gcBars=el('chart').children.filter(n=>n.attrs['data-gc']==='duration');
assert.equal(gcBars.length,1);assert.equal(gcBars[0].attrs.fill,'#dc2626');
assert(Number(gcBars[0].attrs.width)>0);
samples[0].reason='explicit';draw();
assert.equal(el('chart').children.filter(n=>n.attrs['data-tick']==='gc').length,1);
samples.forEach((s,i)=>s.reason=originalReasons[i]);draw();
const snapshotTicks=el('chart').children.filter(n=>n.attrs['data-tick']==='snapshot');
assert.equal(snapshotTicks.length,samples.length);
assert.deepStrictEqual(snapshotTicks.map(n=>Number(n.attrs.x1)),[80,960]);
assert(snapshotTicks.every(n=>n.attrs.stroke==='#2563eb'&&Number(n.attrs.y2)-Number(n.attrs.y1)===4));
// Execution-stream controls appear only when the profile records worker IDs.
const streamSamples=samples;
const streamGroup=el('group').value,streamScope=el('scope').value;
samples=streamSamples.map(s=>({...s,regions:s.regions.map(r=>({...r,worker:-1})),stacks:s.stacks.map(r=>({...r,worker:-1}))}));
el('group').value='worker';el('scope').value='worker:-1';scopes();
assert(!el('scope').children.some(o=>o.value.startsWith('worker:')));
assert(!el('group').children.some(o=>o.value==='worker'));
assert.equal(el('scope').value,'all');assert.equal(el('group').value,'aggregate');
assert.equal(el('execution-stream-note').textContent,'');
samples=streamSamples;scopes();
assert(el('scope').children.some(o=>o.value==='worker:0'));
assert(el('group').children.some(o=>o.value==='worker'));
assert(el('execution-stream-note').textContent.includes('experimental'));
el('group').value=streamGroup;el('scope').value=streamScope;


assert.equal(samples[0].max_pages,'123');
assert.equal(el('profile-title').textContent,'Region profile for program.sml (GC enabled)');
assert.equal(el('gc-note').textContent,'Garbage collections: 17');
samples[0].regions[0].pages='2';samples[0].regions[0].unused_tail='16384';
el('metric').value='pages';draw();
assert.deepStrictEqual(model().totals,[16384n,0n]);
samples[0].regions[0].pages='0';samples[0].regions[0].unused_tail='0';
assert(el('chart').children.some(n=>n.attrs['data-peak']==='pages'&&n.attrs.y1===n.attrs.y2));
assert(el('chart').children.some(n=>n.textContent==='Memory (KiB)'));
for(const metric of ['pages','page_footprint','total']){
 el('metric').value=metric;draw();
 const line=el('chart').children.find(n=>n.attrs['data-peak']==='pages');assert(line);
 assert.equal(line.attrs.y1,line.attrs.y2);
 if(metric!=='total'){assert.equal(line.attrs.y1,'65');assert(el('chart').children.some(n=>n.textContent.includes('984 KiB (123 pages)')));}
 assert(el('chart').children.some(n=>n.textContent.includes('123 pages')));
 el('scope').value='thread:1';draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));el('scope').value='all';
}
el('metric').value='large_bytes';draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));
el('scope').value='all';el('metric').value='total';el('limit').value='1';draw();
assert.equal(model().bands.length,3); // largest region, stack, Other
assert.equal(model().bands[0].key,'other');
assert.equal(model().bands[0].values[0],56n);
assert.deepStrictEqual(el('legend').children.map(n=>n.children[1].textContent),model().bands.map(b=>b.label).reverse());
el('limit').value='0';draw();
el('show-type').checked=true;
assert(label(samples[0].regions[0]).includes('type unavailable'));
for(const type of ['top','bot','pair','triple','string','array','ref'])assert(label({...samples[0].regions[0],region_type:type}).endsWith('('+type+')'));
assert(label({...samples[0].regions[0],region_type:'pair'}).endsWith('(pair)'));
el('show-type').checked=false;
el('legend-right').checked=true;draw();assert.equal(el('graph-layout').className,'graph-layout legend-right');
assert(label({...samples[0].regions[0],name:'',binding:'5'}).startsWith('r5 · '));
el('legend-right').checked=false;draw();assert.equal(el('graph-layout').className,'graph-layout');
assert(label({...samples[0].regions[0],name:'',binding:'5'}).startsWith('Region #5 · '));
const sourceRegion={...samples[0].regions[0],source:'/project/test/life.sml'};
assert(label(sourceRegion).includes('life.sml'));assert(!label(sourceRegion).includes('/project/'));
assert(detail(sourceRegion).includes('Source: /project/test/life.sml'));
assert(baseName({...sourceRegion,source:'REPL #3'})==='REPL #3');
assert(baseName({...sourceRegion,unit:'<global>'})==='global');
assert(baseName(samples[0].regions[0])===samples[0].regions[0].unit);
assert(regionKey(sourceRegion)===regionKey({...sourceRegion,source:'/elsewhere/life.sml'}));
const exported=exportSvgDocument();
assert.equal(exported.tag,'svg');assert(Number(exported.attrs.width)<1440);
assert(!exported.querySelectorAll('text').some(n=>n.textContent.includes('ML stack band')));
assert(exported.querySelectorAll('text').some(n=>n.textContent==='Region profile for program.sml (GC enabled)'));
assert(exported.querySelectorAll('text').some(n=>n.textContent.includes('Metric:') && n.textContent.includes('Garbage collections: 17')));
assert(exported.querySelectorAll('text').some(n=>n.textContent.includes('Samples: '+samples.length)));
assert.equal(exported.querySelectorAll('text').filter(n=>n.textContent.includes('Sampled maximum:')).length,1);
const exportChart=exported.querySelectorAll('svg')[0];assert.equal(exportChart.attrs.width/exportChart.attrs.height,1.5);
assert.equal(exportChart.querySelectorAll('polygon').length,model().bands.length);
assert(exportChart.querySelectorAll('polygon').every(n=>!n.attrs.fill.startsWith('hsl')));
assert(exported.querySelectorAll('text').some(n=>Number(n.attrs.x)>1026));
assert(exported.querySelectorAll('text').every(n=>Number(n.attrs.y)<Number(exported.attrs.height)));
const originalName=samples[0].regions[0].name;samples[0].regions[0].name='Very long region '.repeat(100);
const tall=exportSvgDocument();assert(Number(tall.attrs.height)>Number(exported.attrs.height));samples[0].regions[0].name=originalName;draw();
const beforeLabels=model().totals.slice(),beforeKeys=model().bands.map(b=>b.key);
assert(!label(samples[0].regions[2]).includes('(finite)'));
assert(!label({...samples[0].regions[2],finite_bytes:'0'}).includes('(finite)'));
assert(!label(samples[0].regions[0]).includes('(infinite)'));
assert(label(samples[0].regions[3]).includes(' · global'));
el('show-base').checked=false;el('group').value='region';draw();
assert(!label(samples[0].regions[3]).includes('global'));
assert(!label(samples[0].regions[0]).includes('(infinite)'));
assert(el('legend').children.some(n=>n.title.includes('Base name: global')&&n.title.includes('Region kind: infinite')));
assert(detail({...samples[0].regions[0],region_type:'pair'}).includes('Region type: pair'));
assert(el('rows').children.some(n=>(n.children[0].title||'').includes('Region kind: finite')));
// A and B have identical display names with base names and kinds hidden.
assert.equal(label(samples[0].regions[0]),label(samples[0].regions[2]));
assert.equal(el('rows').children.length,4); // distinct bindings still have distinct rows
assert.deepStrictEqual(model().totals,beforeLabels);assert.deepStrictEqual(model().bands.map(b=>b.key),beforeKeys);
el('show-peak').checked=false;draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));assert.equal(el('peak-note').textContent,'');
el('show-peak').checked=true;draw();assert(el('chart').children.some(n=>n.attrs['data-peak']));
el('show-base').checked=true;el('group').value='aggregate';draw();
const huge=2n**60n+1n;
assert.deepStrictEqual(model().totals,[huge+155n,huge+155n]);
assert.equal(model().bands.length,4); // A, B, global, stack; duplicate names not merged
const ordered=model().bands;assert(ordered.every((b,i)=>!i||ordered[i-1].sum<=b.sum));
const colors=new Map(ordered.map(b=>[b.key,bandColor(b.key)]));
function check(scope,expected){el('scope').value=scope;draw();assert.deepStrictEqual(model().totals,expected);assert(model().bands.every(b=>colors.get(b.key)===bandColor(b.key)));}
check('thread:1',[huge+99n,huge+99n]);check('thread:2',[56n,56n]);
check('worker:0',[huge+99n,huge+99n]);check('worker:1',[56n,56n]);
check('cpu:4',[huge+99n,0n]);check('cpu:5',[56n,huge+155n]);
check('cpu:-1',[0n,0n]);check('all',[huge+155n,huge+155n]);
assert(el('chart').children.some(n=>n.textContent==='Elapsed time (ms)'));
assert(el('chart').children.some(n=>n.textContent==='Memory (EiB)'));
assert(el('chart').children.filter(n=>n.tag==='polygon').length===4);
el('metric').value='large_bytes';draw();assert.deepStrictEqual(model().totals,[huge+51n,huge+51n]);
// Narrowing changes graph sums, axes, snapshot selection and SVG export.
el('metric').value='total';
const fullRangeSamples=samples;
samples=[0,1,2].map(i=>({...fullRangeSamples[i%2],sample:String(10+i),time:String(1000000*(i+1))}));
resetSnapshotRange();const fullRangeTotals=model().totals.slice();
el('sample').value=0;el('range-start').value=1;narrowSnapshots('start');
assert.deepStrictEqual(model().totals,fullRangeTotals.slice(1));
assert.equal(el('sample').min,1);assert.equal(el('sample').value,1);assert.equal(el('sample').max,2);
assert.equal(el('chart').children.filter(n=>n.attrs['data-tick']==='snapshot').length,2);
assert(el('range-caption').textContent.includes('11–12'));
assert(el('caption').textContent.startsWith('Snapshot 11'));
el('range-end').value=1;narrowSnapshots('end');
assert.equal(model().totals.length,1);assert.equal(el('sample').min,el('sample').max);
assert(exportSvgDocument().querySelectorAll('text').some(n=>n.textContent.includes('Samples: 1')));
assert(!el('chart').children.some(n=>Object.values(n.attrs).some(v=>/NaN|Infinity/.test(v))));
el('range-start').value=2;narrowSnapshots('start');assert.equal(snapshotBounds()[0],1);
el('range-end').value=0;narrowSnapshots('end');assert.equal(snapshotBounds()[1],1);
resetSnapshotRange();assert.deepStrictEqual(model().totals,fullRangeTotals);assert(el('range-reset').disabled);
samples=fullRangeSamples;resetSnapshotRange();

// Function labels honor Show base names without merging distinct units.
const allocationOriginal={session:profile.allocation_session,rows:profile.allocations};
profile.allocation_session={enabled:'1',selector:'<global>:4',build_id:'test'};
profile.allocations=['unit-a','unit-b'].map(unit=>({unit,function:'union19',source:'/source/sets.sml',site:'1',count:'1',bytes:'16'}));
el('show-base').checked=false;allocationTable();
assert.equal(el('allocation-rows').children.length,2);
assert(el('allocation-rows').children.every(r=>r.children[0].querySelector('button').textContent==='union19 · site 1'));
assert(el('allocation-rows').children[0].children[0].title.includes('unit-a'));
el('show-base').checked=true;allocationTable();
assert(el('allocation-rows').children.every(r=>r.children[0].querySelector('button').textContent==='union19 · site 1 · sets.sml'));
const hashA='a'.repeat(22),hashB='b'.repeat(22);
profile.allocations=[['map1_'+hashA,'/source/lib/a.sml','1'],['map1_'+hashA,'/source/lib/a.sml','2'],['copy2_'+hashB,'/source/app/a.sml','3']].map(([name,source,site])=>({unit:'unit-a',function:name,source,site,definition:site,count:'1',bytes:'16'}));
el('show-base').checked=false;allocationTable();
assert.equal(el('allocation-rows').children[0].children[0].querySelector('button').textContent,'map1 · site 1');
assert.equal(el('allocation-rows').children[0].children[1].textContent,'lib/a.sml');
assert.equal(el('allocation-rows').children[0].children[1].title,'/source/lib/a.sml');
assert(el('allocation-rows').children[0].children[0].title.includes(hashA));
profile.allocations[2].function='map1_'+hashB;allocationTable();
assert(el('allocation-rows').children.every(r=>r.children[0].textContent.startsWith('map1_')));
assert.equal(shortFunction('user_function'),'user_function');
assert.equal(sourceLabels(['global','/source/only.sml'])('/source/only.sml'),'only.sml');
profile.allocation_session=allocationOriginal.session;profile.allocations=allocationOriginal.rows;

samples=[samples[0]];draw();assert(el('range-start').disabled&&el('range-end').disabled);assert(!el('chart').children.some(n=>Object.values(n.attrs).some(v=>/NaN|Infinity/.test(v))));
const unitFixture=JSON.parse(JSON.stringify(samples[0]));
unitFixture.stacks=[];unitFixture.regions=[unitFixture.regions[0]];
unitFixture.regions[0].page_footprint='0';unitFixture.regions[0].finite_bytes='0';
samples=[unitFixture];el('show-peak').checked=false;
for(const [size,unit] of [['512','bytes'],['1024','KiB'],[String(2**20),'MiB'],[String(2**30),'GiB']]){
 unitFixture.regions[0].large_bytes=size;draw();assert(el('chart').children.some(n=>n.textContent==='Memory ('+unit+')'));
 assert(el('caption').textContent.includes(unit));
}
for(const [time,unit] of [['500','ns'],['500000','µs'],['500000000','ms'],['5000000000','s']]){
 unitFixture.time=time;draw();assert(el('chart').children.some(n=>n.textContent==='Elapsed time ('+unit+') · single snapshot'));
 assert(el('caption').textContent.includes(' '+unit+' · '));
}
samples=[];draw();assert(el('caption').textContent.includes('No completed'));assert(el('range-start').disabled&&el('range-end').disabled);assert(el('export-svg').disabled);assert.throws(exportSvgDocument,/No completed/);
console.log('Stacked graph: exact sums, ordering, colors, filters/migration, units, truncation, empty/single samples passed');
