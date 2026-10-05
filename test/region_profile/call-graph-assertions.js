// A diamond, recursion, indirect calls, and same labels in distinct units.
{
 const savedDocs=[...irDocuments],savedAlloc=profile.allocations,savedSession=profile.allocation_session;
 irDocuments.clear();
 const edge=(kind,caller,callee='')=>({kind,caller,callee});
 irDocuments.set('graph',{unit:'graph',source:'/src/g.sml',calls:[
  ...['root','left','right','leaf','recA','recB'].map(f=>edge('function',f)),
  edge('direct','root','left'),edge('direct','root','right'),edge('direct','left','leaf'),edge('direct','right','leaf'),
  edge('direct','leaf','recA'),edge('direct','recA','recB'),edge('direct','recB','recA'),edge('indirect','right')
 ]});
 profile.allocation_session={enabled:'1',selector:'graph:42',build_id:'graph'};
 profile.allocations=[{unit:'graph',function:'leaf',source:'/src/g.sml',definition:'801',site:'1',count:'7',bytes:'112'},
 {unit:'graph',function:'recA',source:'/src/g.sml',definition:'802',site:'2',count:'3',bytes:'48'}];
 el('allocation-group').value='calls';allocationTable();
 const graph=el('allocation-call-graph'),text=graph.textContent;
 assert(text.includes('root'));assert(text.includes('Recursive group:'));assert(text.includes('Indirect call target unknown'));
 assert.equal(text.split('112 bytes (exclusive)').length,2);assert.equal(text.split('48 bytes (exclusive)').length,2);
 const ref=graph.querySelectorAll('button').find(b=>b.textContent.includes('(shared; totals shown once)'));assert(ref);ref.listeners.click();
 assert(graph.querySelectorAll('summary').some(s=>s.focused&&s.scrolled));
 assert.equal(graph.querySelectorAll('button').filter(b=>b.textContent==='Site 1').length,1);
 assert(el('allocation-table').hidden);assert(!graph.hidden);
 // Closure creators supply context without becoming measured callers.
 const doc=irDocuments.get('graph');doc.closure_edges=true;
 doc.calls.push(edge('function','factory'),edge('function','factory2'),edge('closure','factory','leaf'),edge('closure','factory2','leaf'));
 allocationTable();assert(!graph.textContent.includes('factory'));
 el('allocation-group').value='creators';allocationTable();
 assert(graph.textContent.includes('factory'));assert(graph.textContent.includes('creates closure:'));
 assert.equal(graph.textContent.split('112 bytes (exclusive)').length,2);
 assert.equal(graph.querySelectorAll('button').filter(b=>b.textContent==='Site 1').length,1);
 doc.calls.push(edge('direct','leaf','factory'));allocationTable();
 assert(graph.textContent.includes('Cyclic group:'));assert(graph.textContent.includes('Creates closure within this group:'));
 el('allocation-group').value='calls';allocationTable();
 // Identical function text in two units must not merge their counters.
 irDocuments.set('other',{unit:'other',source:'/other/g.sml',calls:[edge('function','leaf')]});
 profile.allocations.push({unit:'other',function:'leaf',source:'/other/g.sml',definition:'803',site:'3',count:'2',bytes:'32'});
 allocationTable();assert.equal(graph.textContent.split('32 bytes (exclusive)').length,2);
 assert.equal(graph.textContent.split('112 bytes (exclusive)').length,2);
 irDocuments.clear();allocationTable();assert(graph.textContent.includes('unavailable'));
 el('allocation-group').value='function';allocationTable();assert(!el('allocation-table').hidden);assert(graph.hidden);
 irDocuments.clear();for(const [k,v] of savedDocs)irDocuments.set(k,v);profile.allocations=savedAlloc;profile.allocation_session=savedSession;
}
console.log('Static call graph: shared callees, recursion, exclusive counts, navigation and legacy fallback passed');
