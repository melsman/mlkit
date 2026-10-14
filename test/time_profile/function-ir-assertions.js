assert(hasTime);
el('metric').value='time';draw();
const functionSample=timeSamples.find(s=>s.attribution?.status==='function' && s.attribution.function.includes('compute__noinline') && functionLocation(s.attribution));
assert(functionSample,'Time function has matching IR independently of region snapshots');
showIR({...functionSample.attribution,time_function:true},null);
assert(activeIR);assert(el('ir-code').querySelectorAll('mark').length>0);
assert(!el('ir-panel').hidden);
console.log('PASS: time function navigates to matching IR without allocation-site metadata');
