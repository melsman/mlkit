assert.equal(allocationSize('1023'),'1023 bytes');
assert.equal(allocationSize('1024'),'1.00 KiB');
assert.equal(allocationSize('1048576'),'1.00 MiB');
assert.equal(allocationSize('1073741824'),'1.00 GiB');
assert.equal(allocationSize('9007199254740993'),'8.00 PiB');
// Shared paths and cycles must never duplicate the measured site totals.
{
 const saved={flow:profile.region_flow,allocations:profile.allocations,session:profile.allocation_session,region:profile.allocation_region};
 const key=(unit,r)=>JSON.stringify([unit,String(r)]),root=key('u',163),a=key('u',139),b=key('v',45),c=key('v',17),unrelated=key('u',149);
 const node=(id,owner)=>({id,unit:JSON.parse(id)[0],region:JSON.parse(id)[1],owner,role:id===root?'local':'formal',source:'/source/test.sml'});
 const edge=(actual,formal)=>({actual,formal,caller:'caller',callee:'callee',mode:'sat',point:'0',identity:'flow-fixture'});
 profile.allocation_session={enabled:'1',selector:'u:163',build_id:'flow'};profile.allocation_region={unit:'u',binding:'163'};
 profile.region_flow={available:true,nodes:[node(root,'msort'),node(a,'msort'),node(b,'merge'),node(c,'cp'),node(unrelated,'split')],
  edges:[edge(root,a),edge(a,b),edge(a,c),edge(b,c),edge(c,c),edge(unrelated,b)],
  points:[{identity:'flow-fixture',point:'1',node:c},{identity:'flow-fixture',point:'2',node:a}],issues:[]};
 const record=(site,point,fn,count)=>({unit:'u',function:fn,source:'/source/test.sml',site,definition:site,point,ir_identity:'flow-fixture',count,bytes:String(Number(count)*16)});
 profile.allocations=[record('201','1','anon','3'),record('202','2','msort','5'),record('203','99','generated','2')];
 irDocuments.set('flow-fixture',{identity:'flow-fixture',unit:'u',source:'/source/test.sml',calls:[{kind:'closure',caller:'factory',callee:'anon'},{kind:'closure',caller:'factory2',callee:'anon'}]});
 el('allocation-view').value='flow';allocationTable();
 const host=el('allocation-flow');
 assert(!host.hidden&&el('allocation-table').hidden&&!el('allocation-view-control').hidden);
 assert(host.textContent.includes('r163'));assert(host.textContent.includes('r139'));assert(host.textContent.includes('r45'));assert(host.textContent.includes('r17'));
 assert(!host.textContent.includes('split'));assert(host.textContent.includes('fun '));
 assert(host.textContent.includes('LETREGION r163'));assert(host.textContent.includes('factory2'));assert(host.textContent.includes('Sites without a resolved path'));
 assert.equal(host.querySelectorAll('button').filter(b=>b.className?.split(' ').includes('allocation-site')).length,3);
 const reference=host.querySelectorAll('button').find(b=>b.title?.includes('Single argument relationship'));reference.listeners.click();
 assert(host.querySelectorAll('summary').some(s=>s.focused));
 // Two source calls with the same caller/callee must remain distinct.
 const d=key('v',19);profile.region_flow.nodes.push(node(d,'cp'));
 for(const n of profile.region_flow.nodes)if(n.role==='formal')n.position=n.id===d?'1':'0';
 profile.region_flow.edges=[
  {...edge(root,c),caller:'msort',callee:'cp',occurrence:'10',position:'0'},
  {...edge(root,d),caller:'msort',callee:'cp',occurrence:'10',position:'1'},
  {...edge(root,c),caller:'msort',callee:'cp',occurrence:'11',position:'0'},
  {...edge(root,a),caller:'msort',callee:'msort',occurrence:'12',position:'0'}];
 profile.region_flow.points.push({identity:'flow-fixture',point:'3',node:d});
 profile.allocations.push(record('204','3','cp','1'));
 irDocuments.get('flow-fixture').region_data=[{kind:'function',label:'msort',parent:'outer',flavor:'named'}];allocationTable();
 const outer=host.querySelectorAll('summary').find(s=>s.textContent.startsWith('fun outer'));
 assert(outer.parentElement.querySelectorAll('summary').some(s=>s.textContent.startsWith('fun msort')));
 const summaries=host.querySelectorAll('summary');
 assert(summaries.findIndex(s=>s.textContent.startsWith('fun cp '))<summaries.indexOf(outer),'Nested calls order helpers before their enclosing caller');
 const callLines=host.querySelectorAll('div').filter(b=>b.className==='slice-call'&&b.textContent.startsWith('cp['));
 assert.equal(callLines.length,2);
 assert(!host.querySelectorAll('summary').some(s=>s.textContent==='IR'),'No separate IR disclosure');
 assert(callLines.some(b=>b.textContent==='cp[r17:=sat r163, r19:=sat r163]'));
 assert(callLines.some(b=>b.textContent==='cp[r17:=sat r163, ...]'));
 assert.equal(host.querySelectorAll('summary').filter(s=>s.textContent==='fun cp [r17, r19]').length,1);
 const cpSummary=host.querySelectorAll('summary').find(s=>s.textContent==='fun cp [r17, r19]');
 cpSummary.parentElement.open=false;cpSummary.parentElement.listeners.toggle();
 assert.equal(cpSummary.textContent,'fun cp [r17, r19] · 4 allocations · 64 bytes');
 outer.parentElement.open=false;outer.parentElement.listeners.toggle();
 assert(outer.textContent.includes('5 allocations · 80 bytes'),'Lexical totals exclude called functions');
 cpSummary.parentElement.open=true;cpSummary.parentElement.listeners.toggle();
 assert.equal(cpSummary.textContent,'fun cp [r17, r19]');
 profile.allocations.pop();
 el('allocation-view').value='site';allocationTable();assert(host.hidden&&!el('allocation-table').hidden);assert.equal(el('allocation-rows').children.length,3);
 assert.equal(el('allocation-rows').children.reduce((sum,row)=>sum+BigInt(row.children[3].textContent),0n),160n);
 profile.region_flow.issues=['Missing companion'];el('allocation-view').value='flow';allocationTable();assert(host.textContent.includes('Incomplete region-flow metadata'));
 profile.region_flow.available=false;allocationTable();assert(host.hidden&&el('allocation-view-control').hidden);assert(el('allocation-flow-note').textContent.includes('Showing allocation sites'));
 Object.assign(profile,{region_flow:saved.flow,allocations:saved.allocations,allocation_session:saved.session,allocation_region:saved.region});irDocuments.delete('flow-fixture');
}
