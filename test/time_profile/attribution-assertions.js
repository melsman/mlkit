const fs = require('fs'), assert = require('assert');
for (const mode of ['gc', 'no_gc']) {
  const rows = fs.readFileSync(`${process.argv[2]}/${mode}.json`, 'utf8').trim().split('\n').map(JSON.parse);
  const image = rows.find(r => r.type === 'code_image');
  const functions = rows.filter(r => r.type === 'code_function');
  const samples = rows.filter(r => r.type === 'time_sample');
  function resolve(pc) {
    const offset = BigInt(pc) - BigInt(image.load_address);
    return functions.find(f => BigInt(f.start) <= offset && offset < BigInt(f.end));
  }
  assert.equal(rows.find(r => r.type === 'time_session').time_version, 2);
  const foreign = samples.filter(s => s.state === 2);
  assert(foreign.length > 0);
  for (const name of ['outer__noinline', 'hook__noinline'])
    assert(foreign.some(s => resolve(s.origin_pc)?.function.includes(name)), `no C samples charged to ${name}`);
  assert(foreign.every(s => resolve(s.origin_pc)), 'foreign origin outside ML ranges');
  const outer = foreign.filter(s => resolve(s.origin_pc)?.function.includes('outer__noinline'));
  const callbackML = samples.filter(s => resolve(s.pc)?.function.includes('compute__noinline') &&
    s.time > outer[0].time && s.time < outer.at(-1).time);
  assert(callbackML.length > 0, 'no ML samples inside callback extent');
  assert(callbackML.every(s => s.state === 0), 'callback ML retained enclosing C state');
  assert(samples.filter(s => s.state !== 2).every(s => s.origin_pc === 0));
  if (mode === 'gc') assert(samples.some(s => s.state === 3), 'no GC samples');
  else assert(!samples.some(s => s.state === 3), 'GC samples in non-GC runtime');
  assert.equal(rows.filter(r => r.type === 'time_status').at(-1).dropped, 0);
}
