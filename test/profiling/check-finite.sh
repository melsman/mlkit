#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-finite.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
for variant in plain gc gengc tagged-pairs; do
  case "$variant" in
    plain) flags='-no_gc';;
    gc) flags='-gc';;
    gengc) flags='-gengc';;
    tagged-pairs) flags='-gc -tag_pairs';;
  esac
  "$MLKIT" $flags -rp -mlb-subdir unified -o "$OUT/program" test/profiling/finite-stack.mlb > "$OUT/build.log" 2>&1 || { cat "$OUT/build.log"; exit 1; }
  "$OUT/program" +RTS -rp -rp_interval 0 -rp_file "$OUT/profile.rp" > "$OUT/output"
  printf 'finite stack: OK\n' > "$OUT/expected"
  cmp "$OUT/expected" "$OUT/output"
  "$RPVIEW" "$OUT/profile.rp" --format json > "$OUT/profile.json"
  node - "$OUT/profile.json" <<'JS'
const fs=require('fs'),assert=require('assert');
const records=fs.readFileSync(process.argv[2],'utf8').trim().split('\n').map(JSON.parse);
const stacks=records.filter(r=>r.type==='stack');
assert(stacks.length);
assert(stacks.some(r=>r.active_bytes>=800*8));
assert(stacks.every(r=>r.active_bytes===r.stack_bytes&&r.finite_bytes===0));
assert(!records.some(r=>r.kind==='finite'));
JS
  echo "finite regions counted as stack: $variant passed"
done
