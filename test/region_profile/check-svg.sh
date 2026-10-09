#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-svg.XXXXXX")
cleanup () {
  status=$?
  if [ "$status" -eq 0 ]; then rm -rf "$OUT"
  else
    echo "Failed test artifacts retained at $OUT" >&2
    for file in "$OUT/stdout" "$OUT/stderr"; do
      if [ -f "$file" ]; then cat "$file" >&2; fi
    done
  fi
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM
cp "$ROOT/test/region_profile/svg-fixture.json" "$OUT/profile.rp"
run() { sh "$ROOT/test/region_profile/encode-fixture.sh" "$OUT/profile.rp" "$OUT/input.rp"; PATH=/nonexistent "$RPVIEW" "$OUT/input.rp" -o "$OUT/graph.svg" "$@" > "$OUT/stdout" 2> "$OUT/stderr"; }
svg() {
    run "$@"
    awk '{gsub(/></,">\n<"); print}' "$OUT/graph.svg" > "$OUT/lines"
    grep -q '<svg xmlns="http://www.w3.org/2000/svg"' "$OUT/lines"
    ! grep -Eq 'NaN|Infinity|ML stack band|<script|<foreignObject' "$OUT/lines"
    if command -v xmllint >/dev/null 2>&1; then xmllint --noout "$OUT/graph.svg"; fi
}
count() { [ "$(grep -c '<polygon ' "$OUT/lines")" -eq "$1" ]; }
contains() { grep -Fq "$1" "$OUT/lines"; }
reject() { if run "$@"; then echo 'Unexpected success' >&2; exit 1; fi; }
reject --sites
svg
[ "$(grep -c 'data-tick="snapshot"' "$OUT/lines")" -eq 2 ]
grep 'data-tick="snapshot"' "$OUT/lines" | grep -q 'stroke="#2563eb"'
count 4
! grep -q 'data-tick="gc"' "$OUT/lines"
sed 's/"reason":"explicit"/"reason":"after_gc"/g' "$ROOT/test/region_profile/svg-fixture.json" > "$OUT/profile.rp"
svg
[ "$(grep -c 'data-tick="gc"' "$OUT/lines")" -eq 2 ]
grep 'data-tick="gc"' "$OUT/lines" | grep -q 'stroke="#dc2626"'
sed '2s/"reason":"explicit"/"reason":"before_gc"/;11s/"reason":"explicit"/"reason":"after_gc"/' "$ROOT/test/region_profile/svg-fixture.json" > "$OUT/profile.rp"
svg
[ "$(grep -c 'data-gc="duration"' "$OUT/lines")" -eq 1 ]
! grep -q 'data-tick="gc"' "$OUT/lines"
contains 'width="880.00" height="4" fill="#dc2626"'
cp "$ROOT/test/region_profile/svg-fixture.json" "$OUT/profile.rp"
svg
contains 'Region profile for main.sml (GC enabled)'
contains 'Metric: Regions + ML stack · View: All threads · Garbage collections: 17'
contains 'Samples: 2'
[ "$(grep -c 'Sampled maximum:' "$OUT/lines")" -eq 1 ]
contains 'Memory (EiB)'
grep '<polygon ' "$OUT/lines" | sed 's/ points=.*//' > "$OUT/colors"
[ "$(sed 's/.*fill="//' "$OUT/colors" | sort -u | wc -l | tr -d ' ')" -eq 4 ]
svg --metric stack --show-peak
count 1
contains 'Sampled maximum: 64.00 bytes'
contains 'Metric: ML stack + finite regions'
! grep -q 'stroke-dasharray="8 4"' "$OUT/lines"
svg
for scope in thread:1 worker:0 cpu:4; do
    svg --scope "$scope"
    count 3
    grep '<polygon ' "$OUT/lines" | sed 's/ points=.*//' > "$OUT/filtered"
    [ "$(grep -Fxf "$OUT/colors" "$OUT/filtered" | wc -l | tr -d ' ')" -eq 3 ]
done
svg --regions 1 --show-base --show-type
count 3
contains 'Other (2 regions)'
contains 'test.sml'
contains 'pair'
for metric in pages page_footprint large_bytes finite_bytes descriptor_bytes; do svg --metric "$metric" --regions 0; count 3; done
svg --metric pages --show-peak
contains 'Peak page capacity: 32.00 KiB'
contains 'stroke-dasharray='
svg --metric page_footprint --show-peak --scope thread:1
! grep -q 'stroke-dasharray=' "$OUT/lines"
caption='Custom </script> <&> "caption" __DATA__ __META__ __OPTIONS__'
svg --caption "$caption"
contains '<title>Custom &lt;/script&gt; &lt;&amp;&gt; &quot;caption&quot; __DATA__ __META__ __OPTIONS__</title>'
run --format html --caption "$caption" --regions 2 --show-base --show-type --show-peak --metric pages --group region --scope thread:1
for setting in '"limit":2' '"show-base":true' '"show-type":true' '"show-peak":true' '"metric":"pages"' '"group":"region"' '"scope":"thread:1"' '__OPTIONS__'; do
    grep '^const defaults=' "$OUT/graph.svg" | grep -Fq "$setting"
done
reject --legend-below
reject --legend-right
reject --regions -1
reject --metric bad
reject --scope core:1
reject --scope thread:999
reject --format pdf
reject --caption
reject -o "$OUT/input.rp"
# Retain one committed snapshot.
sed '/"type":"sample_begin","sample":2/,$d' "$ROOT/test/region_profile/svg-fixture.json" > "$OUT/profile.rp"
svg
contains 'single snapshot'
sed -n '1p' "$ROOT/test/region_profile/svg-fixture.json" > "$OUT/profile.rp"
reject
grep -q 'no completed snapshots' "$OUT/stderr"
echo 'SML SVG: filters, aggregation, colours, units, caption, CLI defaults and empty/single profiles passed'
