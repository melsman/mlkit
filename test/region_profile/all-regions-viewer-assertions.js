assert.equal(profile.allocation_session.selector,'all');
assert(!el('attribution-region-control').hidden);
el('attribution-region').value='all';el('metric').value='sites';el('limit').value='0';draw();
const all=model().totals;
assert(el('allocation-title').textContent.includes('all regions'));assert(el('allocation-flow').hidden);
const recorded=[...attributionBindings].filter(([k,r])=>(profile.allocations||[]).some(a=>a.region_definition===r.definition&&a.function.includes('pair__noinline')));
assert.equal(recorded.length,2);
const sums=samples.map(()=>0n);
for(const [selector,region] of attributionBindings){
 el('attribution-region').value=selector;draw();const totals=model().totals;
 const expected=samples.map(s=>(profile.allocations||[]).filter(r=>r.sample===s.sample&&attributionDefinitions.get(String(r.region_definition))?.unit+':'+attributionDefinitions.get(String(r.region_definition))?.binding===selector).reduce((n,r)=>n+BigInt(r.bytes),0n));
 assert.deepStrictEqual(totals,expected);totals.forEach((n,i)=>sums[i]+=n);
}
assert.deepStrictEqual(sums,all,'All equals the sum of each binding');
el('attribution-region').value=recorded[0][0];el('allocation-view').value='flow';draw();
assert(!el('allocation-flow').hidden);assert(el('allocation-flow').textContent.includes('r'+recorded[0][1].binding));
rangeStart=1;rangeEnd=1;draw();assert.equal(model().totals.length,1);
el('attribution-region').value='all';draw();assert.equal(model().totals[0],all[1]);
assert(el('allocation-flow').hidden);assert(!el('allocation-table').hidden);
console.log('All-region viewer: binding selection, aggregate totals, slice and range passed');
