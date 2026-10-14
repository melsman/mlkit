#!/bin/sh
# Exact literal fixtures keep uint64 checks independent of awk numeric precision.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PVIEW=${PVIEW:-$ROOT/bin/pview}
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
cp "$PVIEW" "$OUT/pview"
cp "$ROOT/test/region_profile/graph-fixture.json" "$OUT/profile.prof"
# Resolve optional native input paths before entering the relocated directory.
for profile in "$@"; do PATH=/nonexistent "$OUT/pview" "$profile" -o "$OUT/native.html" > /dev/null; done
cd "$OUT"
run() { sh "$ROOT/test/region_profile/encode-fixture.sh" profile.prof binary.rp >stdout 2>stderr && PATH=/nonexistent ./pview binary.rp -o profile.html "$@" >stdout 2>stderr; }
reject() { if run "$@"; then echo 'Unexpected success' >&2; exit 1; fi; }
run
cp profile.prof original.rp
cp profile.html original.html
[ "$(grep -c '^<script>$' profile.html)" -eq 1 ]
grep -Fq '\u003c/script>' profile.html
grep -Fq '"1152921504606846977"' profile.html
! grep -Eq 'fetch\(|<script src=' profile.html
for alias in binary.rp hardlink.rp symlink.rp; do
    case "$alias" in hardlink.rp) ln binary.rp "$alias";; symlink.rp) ln -s binary.rp "$alias";; esac
    if PATH=/nonexistent ./pview binary.rp -o "$alias" >stdout 2>stderr; then exit 1; fi
    grep -q 'input and output' stderr
done
printf '{"type":' >> profile.prof
run
cmp original.html profile.html
printf '\n' >> profile.prof
reject
sed 's/1152921504606846977/18446744073709551615/g' original.rp > profile.prof
run
grep -Fq '"18446744073709551615"' profile.html
sed 's/1152921504606846977/18446744073709551616/g' original.rp > profile.prof
reject
# An uncommitted sample must not enter the output.
sed '/"type": "sample_end", "sample": 2/,$d' original.rp > profile.prof
run
! grep '^const samples=' profile.html | grep -Fq '"sample":"2"'
for version in 0 1 2 3 4 5 6 7 8 9 11; do
    sed -e "s/\"version\": 10/\"version\": $version/" -e '/"type": "stack"/d' original.rp > profile.prof
    reject
    grep -q 'unsupported profile version' stderr
done
for invalid in '{"type":"header","type":"header"}' '{"type":"header","format":"mlkit-region-profile","version":03,"page_bytes":8192}' '{"type":"header","format":"\uD800"}' '[true,]'; do
    printf '%s\n' "$invalid" > profile.prof
    reject
done
cat > profile.prof <<'DATA'
{"type":"header","format":"mlkit-region-profile","version":10,"page_bytes":8192,"main_source":"/tmp/__DATA__ __META__ </script>.sml","gc_enabled":true}
{"type":"session_end","gc_collections":1152921504606846979,"max_pages":0}
DATA
run
grep -Fq 'const samples=[];' profile.html
grep '^const profile=' profile.html | grep -Fq '"gc_collections":"1152921504606846979","complete":true'
grep '^const profile=' profile.html | grep -Fq '__DATA__ __META__ \u003c/script>.sml'
sed -n '1p' profile.prof > header.rp
cp header.rp profile.prof
run
grep '^const profile=' profile.html | grep -Fq '"gc_collections":null,"complete":false'
sed 's/"gc_enabled":true/"gc_enabled":"yes"/' header.rp > profile.prof
reject
cp header.rp profile.prof
printf '%s\n' '{"type":"session_end","gc_collections":-1,"max_pages":0}' >> profile.prof
reject
# Current records require stack data and source/type/summary metadata.
sed '/"type": "stack"/d' original.rp > profile.prof
reject
grep -q 'missing stack records' stderr
for field in source region_type g0_pages max_pages gc_collections cache_bytes main_source gc_enabled; do
    sed "s/\"$field\":/\"removed_$field\":/g" original.rp > profile.prof
    reject
    grep -q "missing $field" stderr
done
# Syntactically valid but inconsistent records must still be rejected.
sed 's/"active_bytes": 64/"active_bytes": 65/' original.rp > profile.prof
reject
sed 's/"pages": 0/"pages": 1/' original.rp > profile.prof
reject
awk '{print; if ($0 ~ /"type": "stack", "sample": 1, "thread": 1/) print}' original.rp > profile.prof
reject
sed '/"type": "region"/s/"sample": 1/"sample": 99/' original.rp > profile.prof
reject
# JSON string escapes and a surrogate pair must survive the HTML boundary.
sed 's/same <\/script> name/\\\"\\\\\\n\\u0000é λ \\uD83D\\uDE00 <\/script> __DATA__ __META__/' original.rp > profile.prof
run
grep -Fq '😀' profile.html
grep -Fq '\u0000' profile.html
echo 'Offline viewer: uint64, escaping, metadata, current format and unsupported versions, truncation, malformed input and aliases passed'
# Definitions precede use, are immutable, and replace inline static metadata.
sed '/"type": "binding"/d' original.rp > profile.prof
reject
grep -q 'unknown binding definition' stderr
awk '{print; if ($0 ~ /"type": "binding"/) print}' original.rp > profile.prof
reject
grep -q 'duplicate binding definition' stderr
sed '/"type": "region"/s/"definition":/"unit": "override", "definition":/' original.rp > profile.prof
reject
grep -q 'static metadata in region record' stderr
# Definition IDs are independent of source-level binding numbers and exact uint64s.
sed 's/"definition": 1/"definition": 18446744073709551615/g' original.rp > profile.prof
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
' original.rp > profile.prof
run
grep '^const samples=' profile.html | grep -Fq 'later-unit'
echo 'Binding definitions: reuse, late discovery, uint64 IDs and invalid references passed'

# The current format carries a startup object manifest.
cat > profile.prof <<'DATA'
{"type":"header","format":"mlkit-region-profile","version":10,"page_bytes":8192,"main_source":"test.sml","gc_enabled":false}
{"type":"allocation_session","enabled":1,"depth":1,"build_id":"manifest-test","selector":"unit:9"}
{"type":"ir_object","ir_identity":"missing-build","ir_object":"/missing/library.o"}
{"type":"session_end","gc_collections":0,"max_pages":0}
DATA
run
grep -q '"ir_objects":\[{"type":"ir_object"' profile.html
grep -q 'Missing or mismatched IR: /missing/library.o' profile.html
sed 's/"version":10/"version":9/' profile.prof > old.rp
mv old.rp profile.prof
reject
grep -q 'unsupported profile version' stderr
echo 'Current object manifest and old-format rejection passed'
