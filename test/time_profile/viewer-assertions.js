assert(hasTime);
el('metric').value='time';draw();
for(const sample of timeSamples){
 assert(sample.attribution,'Offline attribution is embedded');
 if(String(sample.state)==='1')assert.equal(sample.attribution.status,'recorder');
 if(String(sample.state)==='3')assert.equal(sample.attribution.status,'gc');
}

assert(!el('time-section').hidden);
assert(el('time-note').textContent.includes('sampled estimates'));
assert(el('time-status').textContent.includes('Session-wide loss'));
// Exact timestamps above Number's precision; independent snapshot boundaries.
timeSamples.splice(0,timeSamples.length,
 {time:timeLast/4n,thread:'0',stream:'0',attribution:{status:'function',unit:'u',function:'f',source:'s',ir_identity:'ir'}},
 {time:timeLast/2n,thread:'0',stream:'0',attribution:{status:'gc'}},
 {time:timeLast*3n/4n,thread:'0',stream:'0',attribution:{status:'unknown-pc'}});
timeStart=0;timeEnd=10000;draw();
assert.equal(el('time-rows').children.length,3);
assert(el('time-rows').textContent.includes('33.33%'));
timeStart=4000;timeEnd=6000;draw();
assert.equal(el('time-rows').children.length,1);
assert(el('time-rows').textContent.includes('Garbage collection'));
assert(el('time-rows').textContent.includes('100.00%'));
assert(visibleSamples().every(inTime));
timeStart=9000;timeEnd=9500;draw();
assert(el('time-rows').textContent.includes('No recorded time samples'));
resetSnapshotRange();assert.equal(timeEnd,10000);
console.log('PASS: time report categories, denominator, independent interval filtering and empty windows');

timeStart=2500;timeEnd=7500;
if(samples.length){el('metric').value='total';draw();assert(el('time-section').hidden);}
el('metric').value='time';draw();assert(!el('time-section').hidden);
assert(document.querySelectorAll('.region-only').every(n=>n.hidden));
assert(el('show-type').closest('.control-heading').hidden);
assert(el('show-peak').closest('.control-heading').hidden);
if(!samples.length)assert.deepStrictEqual(el('metric').options.map(o=>o.value),['time']);
assert.equal(timeStart,2500);assert.equal(timeEnd,7500);
// Function navigation must reject a different unit, even with an identity match.
const doc={identity:'test-function',unit:'u',text:'fun f(x) = x\n',code_start:0,code_bytes:13,source:'s'};
irDocuments.set(doc.identity,doc);
const record={function:'f',ir_identity:doc.identity,unit:'u',source:'s'};
assert(functionLocation(record));assert.equal(functionLocation({...record,unit:'other'}),null);
showIR({...record,time_function:true},el('time-heading'));
assert(!el('ir-panel').hidden);assert(el('ir-title').textContent.includes('Function f'));

const repeated={identity:'repeated',unit:'u',text:'fun exists(x) = x\nfun exists(y) = y\n',code_start:0,code_bytes:36,
 region_data:[{kind:'function',flavor:'named',label:'exists10'},{kind:'function',flavor:'named',label:'exists11'}]};
irDocuments.set(repeated.identity,repeated);
const first=functionLocation({function:'exists10',unit:'u',ir_identity:'repeated'});
const second=functionLocation({function:'exists11',unit:'u',ir_identity:'repeated'});
assert(first&&second);assert(first.spans[0].start<second.spans[0].start);
repeated.region_data[1].label='other11';assert.equal(functionLocation({function:'exists10',unit:'u',ir_identity:'repeated'}),null);

timeSamples.splice(0,timeSamples.length,{time:timeLast/2n,thread:'0',stream:'0',attribution:{status:'function',...record}});
timeStart=0;timeEnd=10000;timeReport();
const functionLink=el('time-rows').querySelector('button');assert(functionLink);
assert(functionLink.title.includes('Function: f'));assert(functionLink.title.includes('Source: s'));
assert(functionLink.title.includes('Unit: u'));assert(functionLink.title.includes('IR identity: test-function'));
