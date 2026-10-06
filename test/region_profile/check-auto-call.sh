#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit-arm64}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-auto-call.XXXXXX")
echo "Automatic foreign-call artifacts: $OUT"
cat > "$OUT/foreign.c" <<'C'
#include <stdint.h>
int64_t auto_boxed(int64_t n) { return n + 1; }
C
$CC -c "$OUT/foreign.c" -o "$OUT/foreign.o"
ar rcs "$OUT/libforeign.a" "$OUT/foreign.o"
cat > "$OUT/main.sml" <<'SML'
fun foreign__noinline (x:Int64.int) : Int64.int = prim ("@auto_boxed", x)
val unboxed : int = prim ("@auto_boxed", (41:int))
val results = List.tabulate (10, fn n => foreign__noinline (Int64.fromInt n))
fun sample () : unit = prim ("mlkit_rp_sample", ())
val () = sample ()
val () = if unboxed = 42 andalso List.last results = Int64.fromInt 10 then print "auto call ok\n"
         else raise Fail "foreign result"
SML
printf '%s\n' '$(SML_LIB)/basis/basis.mlb' main.sml > "$OUT/main.mlb"
"$MLKIT" -gc -rp -libdirs "$OUT" -libs foreign,m,c,dl -o "$OUT/program" "$OUT/main.mlb" > "$OUT/build.log" 2>&1
"$OUT/program" +RTS -rp -rp_interval 0 -rp_region all -rp_file "$OUT/profile.rp" -RTS
"$RPVIEW" "$OUT/profile.rp" --format json > "$OUT/profile.json"
node - "$OUT/profile.json" <<'JS'
const fs = require('fs'), assert = require('assert');
const rows = fs.readFileSync(process.argv[2], 'utf8').trim().split('\n').map(JSON.parse);
const sites = rows.filter(r => r.type === 'allocation_site' && r.function.includes('foreign__noinline'));
assert(sites.length > 0, 'Missing boxed foreign-call allocation');
assert(sites.every(r => r.location_kind === 1), 'Foreign result lost its call location');
assert(rows.some(r => r.type === 'allocation' && sites.some(s => s.definition === r.definition) && r.count > 0));
console.log('Automatic foreign-call result attribution passed');
JS
"$RPVIEW" "$OUT/profile.rp" -o "$OUT/profile.html" > /dev/null
{
  cat "$ROOT/test/region_profile/graph-prelude.js"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/profile.html" | sed '1d;$d'
  cat <<'JS'
for (const row of profile.allocations.filter(r => r.function.includes('foreign__noinline'))) {
  assert.equal(irSites.get(String(row.definition)).status, 'available');
  showIR(row, null);
  assert(el('ir-code').querySelectorAll('mark')[0].textContent.startsWith('$'));
}
console.log('Automatic foreign-call IR navigation passed');
JS
} > "$OUT/viewer.js"
node "$OUT/viewer.js"
