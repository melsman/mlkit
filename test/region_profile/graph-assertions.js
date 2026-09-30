
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
for(const type of ['top','bot','pair','triple','string','array','ref'])assert(label({...samples[0].regions[0],region_type:type}).endsWith('(infinite, '+type+')'));
el('show-kind').checked=false;assert(label({...samples[0].regions[0],region_type:'pair'}).endsWith('(pair)'));
el('show-kind').checked=true;el('show-type').checked=false;
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
assert(exported.querySelectorAll('text').some(n=>n.textContent==='Garbage collections: 17'));
const exportChart=exported.querySelectorAll('svg')[0];assert.equal(exportChart.attrs.width/exportChart.attrs.height,1.5);
assert.equal(exportChart.querySelectorAll('polygon').length,model().bands.length);
assert(exportChart.querySelectorAll('polygon').every(n=>!n.attrs.fill.startsWith('hsl')));
assert(exported.querySelectorAll('text').some(n=>Number(n.attrs.x)>1026));
assert(exported.querySelectorAll('text').every(n=>Number(n.attrs.y)<Number(exported.attrs.height)));
const originalName=samples[0].regions[0].name;samples[0].regions[0].name='Very long region '.repeat(100);
const tall=exportSvgDocument();assert(Number(tall.attrs.height)>Number(exported.attrs.height));samples[0].regions[0].name=originalName;draw();
const beforeLabels=model().totals.slice(),beforeKeys=model().bands.map(b=>b.key);
assert(label(samples[0].regions[2]).endsWith('(finite)'));
assert(label({...samples[0].regions[2],finite_bytes:'0'}).endsWith('(finite)'));
assert(label(samples[0].regions[0]).endsWith('(infinite)'));
assert(label(samples[0].regions[3]).includes(' · global'));
el('show-base').checked=false;el('show-kind').checked=false;el('group').value='region';draw();
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
el('show-base').checked=true;el('show-kind').checked=true;el('group').value='aggregate';draw();
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
el('metric').value='total';samples=[samples[0]];draw();assert(!el('chart').children.some(n=>Object.values(n.attrs).some(v=>/NaN|Infinity/.test(v))));
samples[0].stacks=null;draw();assert(el('stack-note').textContent.includes('unavailable'));assert(!model().bands.some(b=>b.key==='stack'));
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
samples=[];draw();assert(el('caption').textContent.includes('No completed'));assert(el('export-svg').disabled);assert.throws(exportSvgDocument,/No completed/);
console.log('Stacked graph: exact sums, ordering, colors, filters/migration, units, truncation, old/empty/single samples passed');
