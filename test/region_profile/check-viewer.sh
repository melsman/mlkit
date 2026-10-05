#!/bin/sh
# Exact literal fixtures keep uint64 checks independent of awk numeric precision.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-viewer.XXXXXX")
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
cp "$RPVIEW" "$OUT/rpview"
cp "$ROOT/test/region_profile/graph-fixture.json" "$OUT/profile.rp"
# Resolve optional native input paths before entering the relocated directory.
for profile in "$@"; do PATH=/nonexistent "$OUT/rpview" "$profile" -o "$OUT/native.html" > /dev/null; done
cd "$OUT"
run() { sh "$ROOT/test/region_profile/encode-fixture.sh" profile.rp binary.rp >stdout 2>stderr && PATH=/nonexistent ./rpview binary.rp -o profile.html "$@" >stdout 2>stderr; }
reject() { if run "$@"; then echo 'Unexpected success' >&2; exit 1; fi; }
run
cp profile.rp original.rp
cp profile.html original.html
[ "$(grep -c '^<script>$' profile.html)" -eq 1 ]
grep -Fq '\u003c/script>' profile.html
grep -Fq '"1152921504606846977"' profile.html
! grep -Eq 'fetch\(|<script src=' profile.html
for alias in binary.rp hardlink.rp symlink.rp; do
    case "$alias" in hardlink.rp) ln binary.rp "$alias";; symlink.rp) ln -s binary.rp "$alias";; esac
    if PATH=/nonexistent ./rpview binary.rp -o "$alias" >stdout 2>stderr; then exit 1; fi
    grep -q 'input and output' stderr
done
printf '{"type":' >> profile.rp
run
cmp original.html profile.html
printf '\n' >> profile.rp
reject
sed 's/1152921504606846977/18446744073709551615/g' original.rp > profile.rp
run
grep -Fq '"18446744073709551615"' profile.html
sed 's/1152921504606846977/18446744073709551616/g' original.rp > profile.rp
reject
# An uncommitted sample must not enter the output.
sed '/"type": "sample_end", "sample": 2/,$d' original.rp > profile.rp
run
! grep '^const samples=' profile.html | grep -Fq '"sample":"2"'
for version in 0 1 2 3 4 9; do
    sed -e "s/\"version\": 5/\"version\": $version/" -e '/"type": "stack"/d' original.rp > profile.rp
    reject
    grep -q 'unsupported profile version' stderr
done
for invalid in '{"type":"header","type":"header"}' '{"type":"header","format":"mlkit-region-profile","version":03,"page_bytes":8192}' '{"type":"header","format":"\uD800"}' '[true,]'; do
    printf '%s\n' "$invalid" > profile.rp
    reject
done
cat > profile.rp <<'DATA'
{"type":"header","format":"mlkit-region-profile","version":5,"page_bytes":8192,"main_source":"/tmp/__DATA__ __META__ </script>.sml","gc_enabled":true}
{"type":"session_end","gc_collections":1152921504606846979,"max_pages":0}
DATA
run
grep -Fq 'const samples=[];' profile.html
grep '^const profile=' profile.html | grep -Fq '"gc_collections":"1152921504606846979","complete":true'
grep '^const profile=' profile.html | grep -Fq '__DATA__ __META__ \u003c/script>.sml'
sed -n '1p' profile.rp > header.rp
cp header.rp profile.rp
run
grep '^const profile=' profile.html | grep -Fq '"gc_collections":null,"complete":false'
sed 's/"gc_enabled":true/"gc_enabled":"yes"/' header.rp > profile.rp
reject
cp header.rp profile.rp
printf '%s\n' '{"type":"session_end","gc_collections":-1,"max_pages":0}' >> profile.rp
reject
# Current records require stack data and source/type/summary metadata.
sed '/"type": "stack"/d' original.rp > profile.rp
reject
grep -q 'missing stack records' stderr
for field in source region_type g0_pages max_pages gc_collections cache_bytes main_source gc_enabled; do
    sed "s/\"$field\":/\"removed_$field\":/g" original.rp > profile.rp
    reject
    grep -q "missing $field" stderr
done
# Syntactically valid but inconsistent records must still be rejected.
sed 's/"active_bytes": 64/"active_bytes": 65/' original.rp > profile.rp
reject
sed 's/"pages": 0/"pages": 1/' original.rp > profile.rp
reject
awk '{print; if ($0 ~ /"type": "stack", "sample": 1, "thread": 1/) print}' original.rp > profile.rp
reject
sed '/"type": "region"/s/"sample": 1/"sample": 99/' original.rp > profile.rp
reject
# JSON string escapes and a surrogate pair must survive the HTML boundary.
sed 's/same <\/script> name/\\\"\\\\\\n\\u0000é λ \\uD83D\\uDE00 <\/script> __DATA__ __META__/' original.rp > profile.rp
run
grep -Fq '😀' profile.html
grep -Fq '\u0000' profile.html
cp "$ROOT/test/region_profile/allocation-fixture.json" profile.rp
run
grep -q '"allocation_session"' profile.html
grep -q '"9007199254740993"' profile.html
cp profile.rp allocation-original.rp
sed 's/"thread":1,"definition":1/"thread":1,"definition":999/' allocation-original.rp > profile.rp
reject
grep -q 'unknown allocation site' stderr
sed 's/"depth":1/"depth":2/' allocation-original.rp > profile.rp
reject
grep -q 'unsupported allocation mode' stderr
sed '/"type":"allocation_session"/d' allocation-original.rp > profile.rp
reject
grep -q 'allocation record outside enabled session' stderr
echo 'Offline viewer: uint64, escaping, metadata, current format and unsupported versions, truncation, malformed input and aliases passed'
# Definitions precede use, are immutable, and replace inline static metadata.
sed '/"type": "binding"/d' original.rp > profile.rp
reject
grep -q 'unknown binding definition' stderr
awk '{print; if ($0 ~ /"type": "binding"/) print}' original.rp > profile.rp
reject
grep -q 'duplicate binding definition' stderr
sed '/"type": "region"/s/"definition":/"unit": "override", "definition":/' original.rp > profile.rp
reject
grep -q 'static metadata in region record' stderr
# Definition IDs are independent of source-level binding numbers and exact uint64s.
sed 's/"definition": 1/"definition": 18446744073709551615/g' original.rp > profile.rp
run
# A later snapshot may introduce a newly loaded unit with a reused binding number.
awk '
 /"type": "binding", "definition": 3/ {
   later=$0; sub(/"definition": 3/,"\"definition\": 4",later)
   sub(/"unit": "<global>"/,"\"unit\": \"later-unit\"",later)
 }
 /"type": "sample_begin", "sample": 2/ {print later}
 /"type": "region", "sample": 2/ {sub(/"definition": 3/,"\"definition\": 4")}
 {print}
' original.rp > profile.rp
run
grep '^const samples=' profile.html | grep -Fq 'later-unit'
echo 'Binding definitions: reuse, late discovery, uint64 IDs and invalid references passed'

# Version-6 allocation profiles remain usable without IR metadata.
cp "$ROOT/test/region_profile/allocation-fixture.json" profile.rp
run
grep -q '"status":"legacy-profile"' profile.html
echo 'Legacy allocation profiles: counters retained, IR navigation unavailable'

# Version 8 carries a startup object manifest, including units without sites.
cat > profile.rp <<'DATA'
{"type":"header","format":"mlkit-region-profile","version":8,"page_bytes":8192,"main_source":"test.sml","gc_enabled":false}
{"type":"allocation_session","enabled":1,"depth":1,"build_id":"manifest-test","selector":"unit:9"}
{"type":"ir_object","ir_identity":"missing-build","ir_object":"/missing/library.o"}
{"type":"session_end","gc_collections":0,"max_pages":0}
DATA
run
grep -q '"ir_objects":\[{"type":"ir_object"' profile.html
grep -q 'Missing or mismatched IR: /missing/library.o' profile.html
sed 's/"version":8/"version":7/' profile.rp > old.rp
mv old.rp profile.rp
reject
grep -q 'IR manifest requires version 8' stderr
echo 'Version 8 object manifest and legacy rejection passed'
