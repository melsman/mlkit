#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
CC=${CC:-cc}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rp-metadata.XXXXXX")
echo "Linker metadata test artifacts: $OUT"
for mode in absent linked; do
  set --
  [ "$mode" != linked ] || set -- -DLINKED_METADATA
  # Optimization is essential: GCC can infer bounds from weak definitions.
  $CC -O2 -std=gnu99 -Wall -Wextra -Werror -DPROFILING "$@" \
    -iquote "$ROOT/src/Runtime" "$ROOT/src/Runtime/RegionProfile.c" \
    "$ROOT/src/Runtime/tests/region-profile-metadata.c" -o "$OUT/$mode"
  "$OUT/$mode" "$OUT/$mode.rp"
  "$RPVIEW" "$OUT/$mode.rp" --format json -o "$OUT/$mode.json"
done
node - "$OUT" <<'JS'
const fs = require('fs'), assert = require('assert');
const read = name => fs.readFileSync(`${process.argv[2]}/${name}.json`, 'utf8')
  .trim().split('\n').map(JSON.parse);
for (const mode of ['absent', 'linked']) {
  const rows = read(mode);
  const objects = rows.filter(r => r.type === 'ir_object');
  const bindings = rows.filter(r => r.type === 'binding');
  assert.equal(bindings.length, 2);
  if (mode === 'linked') {
    assert.deepEqual(objects.map(r => [r.ir_identity, r.ir_object]),
      [['first', 'first.o'], ['second', 'second.o']]);
    assert.deepEqual(bindings.map(r => r.region_type).sort(), ['pair', 'string']);
  } else {
    assert.equal(objects.length, 0);
    assert(bindings.every(r => r.region_type === 'unavailable'));
  }
}
JS
echo 'Linker metadata checks passed'
