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
  assert(functions.some(f => f.source.endsWith('workload.sml')));
  assert(functions.some(f => f.source.endsWith('Initial.sml')), 'linked Basis metadata missing');
  assert(functions.some(f => f.source.endsWith('List.sml')), 'linked List metadata missing');
  assert(functions.every(f => f.ir_identity.length > 0 && f.unit.length > 0));
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
  const loop = functions.find(f => f.source.endsWith('workload.sml') && f.function.startsWith('loop'));
  assert(loop, 'tail-recursive loop optimized away unexpectedly');
  for (const run of ['first', 'second']) {
    const name = `${mode}-${run}`, m = metadata(name), base = BigInt(m.image.load_address);
    const start = base + BigInt(loop.start), end = base + BigInt(loop.end);
    assert.equal(resolve(name, start, m.image.build_id).function, loop.function);
    assert.equal(resolve(name, end-1n, m.image.build_id).function, loop.function);
    const atEnd = resolve(name, end, m.image.build_id);
    assert(atEnd.status === 'unknown-pc' || atEnd.function !== loop.function);
    assert.equal(resolve(name, 0n, m.image.build_id).status, 'unknown-pc');
    const pcs = fs.readFileSync(`${out}/${name}.pcs`, 'utf8').trim().split('\n').map(line => line.split(' '));
    const inLoop = pcs.find(parts => parts[4] === `F.${loop.function}`);
    assert(inLoop, 'no interrupted PC captured inside ML loop');
    assert.equal(resolve(name, BigInt(inLoop[1]), m.image.build_id).function, loop.function);
  }
  const c = metadata(`${mode}-c`);
  const cPCs = fs.readFileSync(`${out}/${mode}-c.pcs`, 'utf8').trim().split('\n').map(line => line.split(' '));
  const cPC = cPCs.find(parts => parts[4] === 'tp_busy');
  assert(cPC, 'no foreign-C sample');
  assert.equal(resolve(`${mode}-c`, BigInt(cPC[1]), c.image.build_id).status, 'unknown-pc');
  const other = metadata(`${mode}-other`);
  assert.notEqual(other.image.build_id, a.image.build_id);
  assert.equal(resolve(`${mode}-first`, BigInt(a.image.load_address)+BigInt(loop.start),
    other.image.build_id).status, 'build-mismatch');
}
const gc = metadata('gc-allocation');
const gcPCs = fs.readFileSync(`${out}/gc-allocation.pcs`, 'utf8').trim().split('\n').map(line => line.split(' '));
const gcPC = gcPCs.find(parts => ['gc', 'evacuate', 'allocGen'].includes(parts[4]));
assert(gcPC, 'no interrupted GC PC');
assert.equal(resolve('gc-allocation', BigInt(gcPC[1]), gc.image.build_id).status, 'unknown-pc');
const repl = rows('repl');
assert.equal(repl.find(r => r.type === 'code_metadata').scope, 'unsupported-repl');
assert(!repl.some(r => r.type === 'code_image' || r.type === 'code_function'));
assert.equal(resolve('repl', 0n, gc.image.build_id).status, 'metadata-unavailable');
const unbound = cp.spawnSync(viewer, [`${out}/gc-first.rp`, '--resolve-pc', '0'], {encoding: 'utf8'});
assert.notEqual(unbound.status, 0);
assert.match(unbound.stderr, /requires --image-build/);
console.log('Native/Basis ranges, tail recursion, captured PCs, ASLR, C/GC unknowns, REPL and build mismatches passed');
