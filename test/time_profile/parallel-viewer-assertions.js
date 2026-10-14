assert(hasTime);assert.equal(String(timeSession.time_version),'3');
el('metric').value='time';draw();assert(!el('time-thread-section').hidden);assert.equal(el('time-thread-rows').children.length,9);
assert.equal(el('time-estimate-heading').textContent,'Time share');
const worker=timeSamples.find(s=>String(s.thread)!=='0');el('time-stream').value=worker.thread+':'+worker.stream;timeReport();
assert.equal(el('time-thread-rows').children.length,1);assert(el('time-summary').textContent.startsWith(timeSamples.filter(s=>s.thread===worker.thread).length.toLocaleString()+' samples'));
timeStart=0;timeEnd=5000;draw();assert(timeSamples.filter(s=>inTime(s)).length<timeSamples.length);
el('time-stream').value='all';timeStart=0;timeEnd=10000;draw();
const linked=timeSamples.find(s=>s.attribution.status==='function'&&s.attribution.function.includes('spin__noinline')&&functionLocation(s.attribution));assert(linked,'parallel worker IR link');showIR({...linked.attribution,time_function:true},null);assert(activeIR);assert(el('ir-code').querySelectorAll('mark').length>0);
console.log('PASS: per-thread details, stream filtering, shared elapsed-time range and worker IR');
