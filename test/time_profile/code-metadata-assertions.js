const fs = require('fs'), assert = require('assert'), cp = require('child_process');
const [out, viewer] = process.argv.slice(2);
const rows = name => fs.readFileSync(`${out}/${name}.json`, 'utf8').trim().split('\n').map(JSON.parse);
function resolve(name, pc, uuid) {
  const result = cp.spawnSync(viewer, [`${out}/${name}.rp`, '--resolve-pc', `0x${pc.toString(16)}`,
    '--image-build', uuid], {encoding: 'utf8'});
  assert.equal(result.status, 0, result.stderr);
  return JSON.parse(result.stdout);
}
function metadata(name) {
  const r = rows(name), image = r.find(x => x.type === 'code_image');
  assert.equal(r.find(x => x.type === 'code_metadata').scope, 'static-executable');
  assert.match(image.build_id, /^[a-f0-9]{32}$/);
  const functions = r.filter(x => x.type === 'code_function');
  assert(functions.length > 0);
  assert(functions.some(f => f.source.endsWith('measure.sml')));
  assert(functions.some(f => f.source.endsWith('Initial.sml')), 'linked Basis metadata missing');
  assert(functions.some(f => f.source.endsWith('List.sml')), 'linked List metadata missing');
  assert(functions.every(f => f.unit.length > 0));
  assert(functions.filter(f => f.source !== "<runtime>").every(f => f.ir_identity.length > 0));
  return {image, functions};
}
for (const mode of ['gc', 'no_gc']) {
  const a = metadata(`${mode}-first`), b = metadata(`${mode}-second`);
  assert.equal(a.image.build_id, b.image.build_id, 'ASLR changed build identity');
  assert.notEqual(a.image.load_address, b.image.load_address, 'test launches did not exercise ASLR');
  assert.deepEqual(a.functions, b.functions, 'ASLR changed image-relative ranges');
  const symbols = fs.readFileSync(`${out}/${mode}-symbols.txt`, 'utf8').trim().split('\n')
    .map(line => line.trim().split(/\s+/)).filter(parts => parts[1] === 'T' && parts[2].startsWith('_F.'));
  // nm uses preferred addresses; Mach-O executable base is 0x100000000.
  for (const [address, , symbol] of symbols) {
    const f = a.functions.find(f => `_F.${f.function.replace(/[^a-zA-Z0-9_.]/g, '_')}` === symbol);
    assert(f, `no range for ${symbol}`);
    assert.equal(BigInt(f.start), BigInt(`0x${address}`) - 0x100000000n);
  }
  const functions = [...a.functions].sort((x,y) => x.start-y.start);
  let previous = 0;
  for (const f of functions) { assert(f.start >= previous && f.end > f.start); previous = f.end; }
  const loop = functions.find(f => f.source.endsWith('measure.sml') && f.function.startsWith('compute__noinline'));
  assert(loop, 'tail-recursive loop optimized away unexpectedly');
  for (const run of ['first', 'second']) {
    const name = `${mode}-${run}`, m = metadata(name), base = BigInt(m.image.load_address);
    const start = base + BigInt(loop.start), end = base + BigInt(loop.end);
    assert.equal(resolve(name, start, m.image.build_id).function, loop.function);
    assert.equal(resolve(name, end-1n, m.image.build_id).function, loop.function);
    const atEnd = resolve(name, end, m.image.build_id);
    assert(atEnd.status === 'unknown-pc' || atEnd.function !== loop.function);
    assert.equal(resolve(name, 0n, m.image.build_id).status, 'unknown-pc');
    const inLoop = rows(name).find(r => r.type === 'time_sample' && BigInt(r.pc) >= start && BigInt(r.pc) < end);
    assert(inLoop, 'no production PC captured inside ML loop');
    assert.equal(resolve(name, BigInt(inLoop.pc), m.image.build_id).function, loop.function);
  }
  const c = a, busy = c.functions.find(f => f.function === 'tp_measure_busy');
  assert(busy, 'missing C range');
  const cPC = rows(`${mode}-first`).find(r => r.type === 'time_sample' &&
    BigInt(r.pc) >= BigInt(c.image.load_address)+BigInt(busy.start) && BigInt(r.pc) < BigInt(c.image.load_address)+BigInt(busy.end));
  assert(cPC, 'no foreign-C sample');
  assert.equal(resolve(`${mode}-first`, BigInt(cPC.pc), c.image.build_id).function, 'tp_measure_busy');
  const other = metadata(`${mode}-other`);
  assert.notEqual(other.image.build_id, a.image.build_id);
  assert.equal(resolve(`${mode}-first`, BigInt(a.image.load_address)+BigInt(loop.start),
    other.image.build_id).status, 'build-mismatch');
}
const gc = metadata('gc-first');
const gcSample = rows('gc-first').find(r => r.type === 'time_sample' && r.state === 3 &&
 gc.functions.some(f => ['gc','evacuate','allocGen'].includes(f.function) &&
  BigInt(r.pc) >= BigInt(gc.image.load_address)+BigInt(f.start) && BigInt(r.pc) < BigInt(gc.image.load_address)+BigInt(f.end)));
assert(gcSample, 'no production GC PC');
assert.equal(resolve('gc-first', BigInt(gcSample.pc), gc.image.build_id).status, 'function');
const repl = rows('repl');
assert.equal(repl.find(r => r.type === 'code_metadata').scope, 'unsupported-repl');
assert(!repl.some(r => r.type === 'code_image' || r.type === 'code_function'));
assert.equal(resolve('repl', 0n, gc.image.build_id).status, 'metadata-unavailable');
const unbound = cp.spawnSync(viewer, [`${out}/gc-first.rp`, '--resolve-pc', '0'], {encoding: 'utf8'});
assert.notEqual(unbound.status, 0);
assert.match(unbound.stderr, /requires --image-build/);
console.log('Native/Basis ranges, tail recursion, captured PCs, ASLR, named C/GC ranges, REPL and build mismatches passed');
