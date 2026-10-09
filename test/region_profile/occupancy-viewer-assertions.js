// The reset fixture has 1,000 objects, then resets and retains only ten.
el('allocation-view').value = 'site';
el('allocation-value').value = 'count';
allocationTable();
assert(el('allocation-rows').textContent.includes((1000n).toLocaleString()));
rangeStart = 1;
rangeEnd = 1;
draw();
assert(el('allocation-rows').textContent.includes('10'));
assert(!el('allocation-rows').textContent.includes((1000n).toLocaleString()));
assert(el('allocation-note').textContent.includes('snapshot 2'));
rangeStart = 0;
draw();
assert(el('allocation-rows').textContent.includes((1000n).toLocaleString()));
assert(el('allocation-note').textContent.includes('snapshot 1'));
console.log('Occupancy snapshot slider: PASS');
