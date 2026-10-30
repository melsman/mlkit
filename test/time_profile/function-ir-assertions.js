assert(hasTime);
el('metric').value='time';draw();
// Test IR navigation from recorded function metadata. A timer need not land in
// this particular function, especially when its integer modulo enters C code.
const timeFunction=profile.code_functions.find(f=>f.function.includes('compute__noinline'));
assert(timeFunction,'Compute function is present in time metadata');
assert(functionLocation(timeFunction),'Time function has matching IR independently of region snapshots');
showIR({...timeFunction,status:'function',time_function:true},null);
assert(activeIR);assert(el('ir-code').querySelectorAll('mark').length>0);
assert(!el('ir-panel').hidden);
console.log('PASS: time function navigates to matching IR without allocation-site metadata');
