#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-par-blocking.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
cc -O2 -Wall -Wextra -Werror -c "$ROOT/test/time_profile/parallel-blocking.c" -o "$OUT/blocking.o"
ar rcs "$OUT/libblocking.a" "$OUT/blocking.o"
cat > "$OUT/main.sml" <<'SML'
fun sleep masked : int = prim ("tp_parallel_sleep",masked)
val () = Thread.spawn (fn () => sleep 0) (fn unmasked =>
         Thread.spawn (fn () => sleep 1) (fn masked =>
         if Thread.get unmasked+Thread.get masked = 0 then print "blocking ok\n" else raise Fail "blocking"))
SML
printf '$(SML_LIB)/basis/basis.mlb\n$(SML_LIB)/basis/par.mlb\nmain.sml\n' > "$OUT/main.mlb"
"${MLKIT:-$ROOT/bin/mlkit}" -no_gc -par -libdirs "$OUT" -libs blocking,m,c,dl -o "$OUT/test" "$OUT/main.mlb" > "$OUT/build.log" 2>&1
"$OUT/test" +RTS -tp -tp_file "$OUT/test.rp" > "$OUT/run.log"
"${RPVIEW:-$ROOT/bin/rpview}" "$OUT/test.rp" --format json -o "$OUT/test.json" >/dev/null
node - "$OUT/test.json" <<'JS'
const fs=require('fs'),a=require('assert'),r=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse),s=r.filter(x=>x.type==='time_sample'),events=r.filter(x=>x.type==='time_thread'),final=r.filter(x=>x.type==='time_status').at(-1);
const counts=[1,2].map(id=>s.filter(x=>x.thread===id).length);
a(counts[0]>10,'Sleeping thread not targeted');a(counts[1]<counts[0]/2,'Masking did not coalesce delivery');
const lifetime=id=>{const e=events.filter(x=>x.thread===id);return e.find(x=>x.event==='end').time-e.find(x=>x.event==='start').time;};
a(lifetime(1)>200000000);a(lifetime(2)>200000000);a.equal(final.dropped,0);a.equal(final.routing_dropped,0);
console.log('PASS: sleeping threads sampled; masked gaps preserve real clock duration without inventing samples');
JS
