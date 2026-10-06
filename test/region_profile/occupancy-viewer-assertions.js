// The reset fixture has 1,000 objects, then resets and retains only ten.
el('allocation-view').value = 'site';
allocationTable();
assert(el('allocation-rows').textContent.includes('1000'));
rangeStart = 1;
rangeEnd = 1;
draw();
assert(el('allocation-rows').textContent.includes('10'));
assert(!el('allocation-rows').textContent.includes('1000'));
assert(el('allocation-note').textContent.includes('snapshot 2'));
rangeStart = 0;
draw();
assert(el('allocation-rows').textContent.includes('1000'));
assert(el('allocation-note').textContent.includes('snapshot 1'));
console.log('Occupancy snapshot slider: PASS');
