{
// Run against the embedded report only: no filesystem, fetch or browser server.
assert(profile.complete, 'Example recording must be complete');
assert(samples.length >= 2, 'Need multiple snapshots to exercise range selection');
assert(profile.allocations.length > 0, 'Need measured occupancy');
assert(profile.region_flow.available, 'Fresh examples need region metadata');
assert.equal(profile.region_flow.issues.length, 0);
// Exercise selectable filters; unavailable workers and finite regions are not UI choices.
const scopes = el('scope').options.map(o=>o.value).filter(v=>v==='all'||/^(thread|worker):/.test(v));
const bindings = el('attribution-region').options.map(o=>o.value).filter(v=>v!=='all');
let checks = 0;
for (const selector of ['all', ...bindings]) {
 el('attribution-region').value = selector;
 for (const scope of selector === 'all' ? scopes : ['all']) {
  el('scope').value = scope;
  const [field, owner] = scope.split(':');
  const matches = r => (field === 'all' || String(r[field]) === owner) &&
   (selector === 'all' || (() => {const b = attributionDefinitions.get(String(r.region_definition));return b.unit+':'+b.binding === selector;})());
  for (const [lo, hi] of [[0,samples.length-1], [0,0], [samples.length-1,samples.length-1]]) {
   rangeStart = lo; rangeEnd = hi;
   const ids = new Set(samples.slice(lo,hi+1).map(s => String(s.sample)));
   const rows = profile.allocations.filter(r => ids.has(String(r.sample)) && matches(r));
   const summaries = profile.occupancy_summaries.filter(r => ids.has(String(r.sample)) && matches(r));
   const totals = new Map(summaries.map(r => [String(r.sample),0n]));
   for (const r of rows) totals.set(String(r.sample),(totals.get(String(r.sample))||0n)+BigInt(r.bytes));
   let peak = null, bytes = -1n;
   for (const [id, value] of totals) if (value > bytes) {peak=id;bytes=value;}
   const expected = rows.filter(r => String(r.sample) === peak);
   const sites = new Set(expected.map(r => JSON.stringify([r.unit,r.site])));
   el('allocation-view').value = 'site'; allocationTable();
   const displayed = el('allocation-rows').children;
   assert.equal(displayed.length, sites.size);
   el('allocation-value').value='count';allocationTable();
   assert.equal(el('allocation-rows').children.reduce((n,r) => n+BigInt(r.children[1].title.split(' ')[0]),0n),expected.reduce((n,r) => n+BigInt(r.count),0n));
   el('allocation-value').value='bytes';allocationTable();
   assert.equal(displayed.reduce((n,r) => n+BigInt(r.children[1].title.split(' ')[0]),0n),bytes < 0n ? 0n : bytes);
   if (peak !== null) {
    assert(el('allocation-note').textContent.includes('Showing snapshot '+peak+' '));
    for (const [key,label] of [['object_overhead','Descriptor overhead: '],['slack','unused page space: ']]) {
     const total=summaries.filter(r => String(r.sample)===peak).reduce((n,r) => n+BigInt(r[key]),0n);
     assert(el('allocation-note').textContent.includes(label+allocationSize(total)));
    }
   }
   el('allocation-view').value = 'flow'; allocationTable();
   const binding=attributionBindings.get(selector);
   const root=binding && JSON.stringify([Number(binding.binding)<=7?'<global>':binding.unit,String(binding.binding)]);
   const hasRoot=profile.region_flow.nodes.some(n=>n.id===root);
   if (selector === 'all' || !hasRoot) {
    assert(el('allocation-flow').hidden && !el('allocation-table').hidden);
    assert(el('allocation-flow-note').textContent.includes(selector==='all'?'Choose a region':'Showing allocation sites'));
   } else {
    assert(!el('allocation-flow').hidden);
    const buttons=el('allocation-flow').querySelectorAll('button').filter(b => b.className?.split(' ').includes('allocation-site'));
    assert.equal(buttons.length,sites.size,'Slice must show each measured site once');
    assert.equal(buttons.reduce((n,b) => n+BigInt(b.title.match(/ · (\d+) bytes$/)[1]),0n),bytes < 0n ? 0n : bytes);
   }
   checks++;
  }
 }
}
// Every observed site can be navigated without reading the original sidecar.
for (const r of new Map(profile.allocations.map(r => [r.definition,r])).values()) {
 const location=irSites.get(String(r.definition));
 if (Number(r.location_kind) === 2) {assert.equal(location.status,'generated');continue;}
 assert.equal(location.status,'available');
 showIR(r,null);
 const marks=el('ir-code').querySelectorAll('mark');assert(marks.length > 0);
 assert(Number(r.location_kind)===1 ? marks[0].textContent.startsWith('$') : /^(attop|atbot|sat) /.test(marks[0].textContent));
 const doc=irDocuments.get(location.identity);
 assert.equal(el('ir-path').value,doc.path);
 assert.equal(el('ir-filename').textContent,doc.path.split(/[\\/]/).pop());
}
// Derive the classic msort path from metadata, without assuming numeric IDs.
const flow=profile.region_flow, nodes=new Map(flow.nodes.map(n => [n.id,n]));
const locals=flow.nodes.filter(n => n.role==='local' && /msort/.test(n.owner));
if ([...irDocuments.values()].some(d => /(?:^|[/\\])msort\.sml$/.test(d.source))) assert(locals.length>0,'Missing msort local bindings');
let msortPaths=0;
for (const local of locals) {
 const reached=new Set(), pending=[local.id];
 while(pending.length){const id=pending.pop();if(reached.has(id))continue;reached.add(id);for(const e of flow.edges)if(e.actual===id)pending.push(e.formal);}
 if (![...reached].some(id => nodes.get(id)?.role==='formal' && /msort/.test(nodes.get(id).owner))) continue;
 msortPaths++;
 for(const name of ['msort','merge','cp'])assert([...reached].some(id => nodes.get(id)?.role==='formal' && new RegExp(name).test(nodes.get(id).owner)), 'Missing msort path to '+name);
 assert(![...reached].some(id => /split/.test(nodes.get(id)?.owner||'')), 'Unrelated split result reached');
}
if (locals.length) assert(msortPaths>0,'No msort recursive result-region path');
console.log('IR11: '+checks+' binding/scope/range checks and offline native IR navigation passed');
}
