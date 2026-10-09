#!/bin/sh
# Validate fresh all-region recordings; pass one or more .rp files.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
[ "$#" -gt 0 ] || { echo 'Usage: check-ir11.sh PROFILE.rp ...' >&2; exit 1; }
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-ir11.XXXXXX")
cleanup () {
 status=$?
 if [ "$status" -eq 0 ]; then rm -rf "$OUT"
 else echo "IR11 validation artifacts retained at $OUT" >&2
 fi
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM
for profile do
 "$RPVIEW" "$profile" --format json -o "$OUT/profile.json" > /dev/null
 node "$ROOT/test/region_profile/all-regions-assertions.js" "$OUT/profile.json" general
 "$RPVIEW" "$profile" -o "$OUT/profile.html" > /dev/null
 "$RPVIEW" "$profile" --sites --regions 12 -o "$OUT/profile.svg" > /dev/null
 # Check the actual HTML controls; also verify the report remains standalone.
 node - "$OUT/profile.html" "$OUT/profile.svg" <<'JS'
const fs=require('fs'),assert=require('assert');
const html=fs.readFileSync(process.argv[2],'utf8'),svg=fs.readFileSync(process.argv[3],'utf8');
const choices=html.match(/<select id="allocation-view"[^>]*>(.*?)<\/select>/)[1];
assert.deepStrictEqual([...choices.matchAll(/<option[^>]*>(.*?)<\/option>/g)].map(m=>m[1]),['Region slice','Allocation sites']);
assert(choices.includes('value="flow" selected'));
assert(!/<(?:script|link)[^>]*(?:src|href)=/.test(html),'Report must not load external resources');
assert(svg.includes('Site contributions across all regions'));
assert(!/NaN|Infinity/.test(svg));
JS
 {
  sh "$ROOT/test/region_profile/viewer-prelude.sh" "$OUT/profile.html"
  sed -n '/^<script>$/,/^<\/script>/p' "$OUT/profile.html" | sed '1d;$d'
  cat "$ROOT/test/region_profile/ir11-assertions.js"
 } > "$OUT/check.js"
 # Only the standalone JS is available in this process; the viewer has no IO API.
 node - "$OUT/check.js" <<'JS'
const fs=require('fs'),vm=require('vm');
vm.runInNewContext(fs.readFileSync(process.argv[2],'utf8'),{console,TextEncoder,TextDecoder},{timeout:300000});
JS
 echo "IR11 passed: $profile"
done
