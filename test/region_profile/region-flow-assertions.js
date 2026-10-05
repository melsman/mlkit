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
 assert(!host.textContent.includes('split'));assert(host.textContent.includes('Recursive region group'));
 assert(host.textContent.includes('shared; totals shown once'));assert(host.textContent.includes('factory2'));assert(host.textContent.includes('Sites without a resolved path'));
 assert.equal(host.querySelectorAll('button').filter(b=>b.textContent.includes(' · site ')).length,3);
 const reference=host.querySelectorAll('button').find(b=>b.textContent.startsWith('↪'));reference.listeners.click();
 assert(host.querySelectorAll('summary').some(s=>s.focused));
 el('allocation-view').value='site';allocationTable();assert(host.hidden&&!el('allocation-table').hidden);assert.equal(el('allocation-rows').children.length,3);
 assert.equal(el('allocation-rows').children.reduce((sum,row)=>sum+BigInt(row.children[3].textContent),0n),160n);
 profile.region_flow.issues=['Missing companion'];el('allocation-view').value='flow';allocationTable();assert(host.textContent.includes('Incomplete region-flow metadata'));
 profile.region_flow.available=false;allocationTable();assert(host.hidden&&el('allocation-view-control').hidden);assert(el('allocation-flow-note').textContent.includes('Showing allocation sites'));
 Object.assign(profile,{region_flow:saved.flow,allocations:saved.allocations,allocation_session:saved.session,allocation_region:saved.region});irDocuments.delete('flow-fixture');
}
