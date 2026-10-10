assert(hasTime);
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
