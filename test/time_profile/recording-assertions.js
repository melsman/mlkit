const fs = require('fs');
const assert = require('assert');
const dir = process.argv[2];
for (const mode of ['gc', 'no_gc']) {
  for (const scenario of ['normal', 'overflow', 'combined']) {
    const rows = fs.readFileSync(`${dir}/${mode}-${scenario}.json`, 'utf8').trim().split('\n').map(JSON.parse);
    const m = {time_session: rows.find(r => r.type === 'time_session'),
      time_samples: rows.filter(r => r.type === 'time_sample'),
      time_status: rows.filter(r => r.type === 'time_status')};
    const snapshots = rows.filter(r => r.type === 'sample_begin');
    assert(m.time_session, 'time session missing');
    assert(m.time_samples.length > 0, 'no time samples');
    const final = m.time_status.at(-1);
    assert.equal(final.final, 1);
    assert.equal(final.active, 0);
    assert.equal(Number(final.recorded), m.time_samples.length);
    if (scenario === 'overflow') assert(Number(final.dropped) > 0, 'overflow invisible');
    else assert.equal(Number(final.dropped), 0, 'unexpected overflow');
    if (scenario !== 'combined') assert.equal(snapshots.length, 0, 'time-only region snapshots');
    else {
      assert(snapshots.length > 0, 'shared region timer inactive');
      assert(m.time_samples.length > snapshots.length, 'time frequency coupled to region snapshots');
    }
    if (scenario !== 'combined') {
      assert(rows.filter(r => r.type === 'allocation_session').every(r => r.enabled === 0));
      assert(!rows.some(r => r.type === 'allocation' || r.type === 'ir_object'));
    }
    assert(m.time_samples.every(s => BigInt(s.pc) > 0n), 'missing PC');
  }
}

const slow = fs.readFileSync(`${dir}/slow.json`, 'utf8').trim().split('\n').map(JSON.parse);
assert(slow.some(r => r.type === 'time_sample' && r.state === 1), 'no samples during drain');
assert(slow.filter(r => r.type === 'time_status').at(-1).dropped > 0, 'slow output loss invisible');
// Mutate independently parsed wire fields; malformed lifecycle must be rejected.
const cp = require('child_process');
const original = fs.readFileSync(`${dir}/gc-normal.rp`);
const records = [];
for (let offset = 8; offset < original.length;) {
  const size = original.readUInt32LE(offset);
  records.push({tag: original[offset+4], offset, size});
  offset += 4+size;
}
const sample = records.find(r => r.tag === 24);
const statuses = records.filter(r => r.tag === 25);
const session = records.find(r => r.tag === 23);
for (const [name, record, field, value] of [
  ['origin', sample, 6, 1n], ['sequence', sample, 0, 2n], ['state', sample, 5, 4n],
  ['version', session, 0, 3n], ['capacity', session, 2, 1n],
  ['final', statuses.at(-1), 5, 0n], ['count', statuses.at(-1), 1, 0n]
]) {
  const bytes = Buffer.from(original);
  bytes.writeBigUInt64LE(value, record.offset+5+field*8);
  const path = `${dir}/invalid-${name}.rp`;
  fs.writeFileSync(path, bytes);
  const result = cp.spawnSync(process.argv[3], [path, '--format', 'json'], {encoding:'utf8'});
  assert.notEqual(result.status, 0, `accepted invalid ${name}`);
}

// Version-1 raw-PC recordings remain readable.
const legacy = Buffer.from(original);
legacy.writeBigUInt64LE(1n, session.offset+5);
for (const record of records.filter(r => r.tag === 24)) {
  legacy.writeBigUInt64LE(0n, record.offset+5+5*8);
  legacy.writeBigUInt64LE(0n, record.offset+5+6*8);
}
fs.writeFileSync(`${dir}/legacy.rp`, legacy);
assert.equal(cp.spawnSync(process.argv[3], [`${dir}/legacy.rp`, '--format', 'json']).status, 0);
