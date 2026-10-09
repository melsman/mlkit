// Multiple live instances contribute to one site; scopes and windows stay exact.
{
 const saved=profile.allocations,first=saved[0],ids=samples.slice(0,2).map(s=>String(s.sample));
 assert(first&&ids.length===2);
 const row=(sample,site,thread,bytes,count)=>({...first,sample,site,thread,worker:thread,cpu:thread,bytes,count});
 profile.allocations=[row(ids[0],first.site,'0','16','1'),row(ids[0],first.site,'1','32','2'),
  row(ids[0],'999999','0','8','1'),row(ids[1],first.site,'0','64','4')];
 // These synthetic allocations introduce owners absent from the snapshot fixture.
 const extraScopes=[];for(const value of ['thread:0','worker:1','cpu:1','thread:999'])if(!el('scope').options.some(o=>o.value===value)){const option=document.createElement('option');option.value=value;option.textContent=value;el('scope').append(option);extraScopes.push(option);}
 rangeStart=0;rangeEnd=1;el('metric').value='sites';el('scope').value='all';el('limit').value='0';draw();
 assert.deepStrictEqual(model().totals,[56n,64n]);
 assert.equal(model().bands.length,2);
 assert.deepStrictEqual(model().bands.find(b=>b.key===siteKey(first)).counts,[3n,4n]);
 assert(el('group').disabled);assert.equal(el('quantity-label').textContent,'Objects');assert(el('tail-label').hidden);
 const originalColor=bandColor(siteKey(first));
 el('limit').value='1';draw();assert.equal(model().bands[0].label,'Other (1 sites)');
 assert.deepStrictEqual(model().totals,[56n,64n]);assert.deepStrictEqual(model().bands[0].counts,[1n,0n]);
 el('scope').value='thread:0';draw();assert.deepStrictEqual(model().totals,[24n,64n]);
 el('scope').value='worker:1';draw();assert.deepStrictEqual(model().totals,[32n,0n]);
 el('scope').value='cpu:1';draw();assert.deepStrictEqual(model().totals,[32n,0n]);
 rangeStart=1;el('scope').value='all';draw();assert.deepStrictEqual(model().totals,[64n]);
 assert.equal(bandColor(siteKey(first)),originalColor);
 const area=el('chart').querySelectorAll('polygon').find(p=>p.getAttribute('data-band')===siteKey(first));
 assert.equal(area.getAttribute('role'),'button');area.listeners.click();assert.equal(irSelectedRecord.definition,first.definition);
 assert(el('legend').querySelectorAll('button').length>0);
 const exported=exportSvgDocument();assert(exported.querySelectorAll('polygon').length>0);
 el('scope').value='thread:999';draw();assert.deepStrictEqual(model().totals,[0n]);assert.equal(model().bands.length,0);
 for(const option of extraScopes)option.remove();
 profile.allocations=saved;rangeStart=0;rangeEnd=samples.length-1;el('metric').value='total';el('scope').value='all';draw();
 assert(!el('group').disabled);assert(!el('tail-label').hidden);
 console.log('Interactive site graph: exact occupancy, Other, scopes, windows, IR links and SVG passed');
}
