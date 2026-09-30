#!/bin/sh
# Exact literal fixtures keep uint64 checks independent of awk numeric precision.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-viewer.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
cp "$RPVIEW" "$OUT/rpview"
cp "$ROOT/test/region_profile/graph-fixture.rp" "$OUT/profile.rp"
# Resolve optional native input paths before entering the relocated directory.
for profile in "$@"; do PATH=/nonexistent "$OUT/rpview" "$profile" -o "$OUT/native.html" > /dev/null; done
cd "$OUT"
run() { PATH=/nonexistent ./rpview profile.rp -o profile.html "$@" >stdout 2>stderr; }
reject() { if run "$@"; then echo 'Unexpected success' >&2; exit 1; fi; }
run
cp profile.rp original.rp
cp profile.html original.html
[ "$(grep -c '^<script>$' profile.html)" -eq 1 ]
grep -Fq '\u003c/script>' profile.html
grep -Fq '"1152921504606846977"' profile.html
! grep -Eq 'fetch\(|<script src=' profile.html
for alias in profile.rp hardlink.rp symlink.rp; do
    case "$alias" in hardlink.rp) ln profile.rp "$alias";; symlink.rp) ln -s profile.rp "$alias";; esac
    reject -o "$alias"
    cmp original.rp profile.rp
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
for version in 1 2; do
    sed -e "s/\"version\": 3/\"version\": $version/" -e '/"type": "stack"/d' original.rp > profile.rp
    run
    grep '^const samples=' profile.html | grep -Fq '"stacks":null'
done
for invalid in '{"type":"header","type":"header"}' '{"type":"header","format":"mlkit-region-profile","version":03,"page_bytes":8192}' '{"type":"header","format":"\uD800"}' '[true,]'; do
    printf '%s\n' "$invalid" > profile.rp
    reject
done
cat > profile.rp <<'DATA'
{"type":"header","format":"mlkit-region-profile","version":3,"page_bytes":8192,"main_source":"/tmp/__DATA__ __META__ </script>.sml","gc_enabled":true}
{"type":"session_end","gc_collections":1152921504606846979}
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
printf '%s\n' '{"type":"session_end","gc_collections":-1}' >> profile.rp
reject
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
echo 'Offline viewer: uint64, escaping, metadata, v1/v2/v3, truncation, malformed input and aliases passed'
