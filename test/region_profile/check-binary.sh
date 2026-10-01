#!/bin/sh
# Hand-encoded wire bytes are independent of the SML fixture encoder.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-binary.XXXXXX")
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
cd "$OUT"
# Magic, 37-byte header: word=8, page=8192, GC=false, source=test.sml.
{
 printf 'MLKRP\000\005\000\045\000\000\000\001'
 printf '\010\000\000\000\000\000\000\000'
 printf '\000\040\000\000\000\000\000\000'
 printf '\000\000\000\000\000\000\000\000'
 printf '\010\000\000\000test.sml'
} > header.rp
# 33-byte session_end: time=1, samples=0, max_pages=UINT64_MAX, GC count=0.
{
 cat header.rp
 printf '\041\000\000\000\004'
 printf '\001\000\000\000\000\000\000\000'
 printf '\000\000\000\000\000\000\000\000'
 printf '\377\377\377\377\377\377\377\377'
 printf '\000\000\000\000\000\000\000\000'
} > complete.rp
"$RPVIEW" complete.rp --format json > complete.json
[ "$(wc -l < complete.json | tr -d ' ')" -eq 2 ]
grep -Fq '"main_source":"test.sml"' complete.json
grep -Fq '"page_bytes":8192' complete.json
grep -Fq '"word_bytes":8' complete.json
grep -Fq '"time":1' complete.json
grep -Fq '"max_pages":18446744073709551615' complete.json
"$RPVIEW" complete.rp -o inferred.json > /dev/null
cmp complete.json inferred.json
"$RPVIEW" complete.rp --format json -o - > stdout.json
cmp complete.json stdout.json
"$RPVIEW" header.rp --format json > header.json
# Every partial prefix/payload of the final record leaves the header readable.
n=49
while [ "$n" -lt 86 ]; do
 dd if=complete.rp of=partial.rp bs=1 count="$n" 2>/dev/null
 "$RPVIEW" partial.rp --format json > partial.json
 cmp header.json partial.json
 n=$((n+1))
done
reject() { if "$RPVIEW" invalid.rp --format json > stdout 2>stderr; then echo 'Accepted corrupt binary' >&2; exit 1; fi; }
cp complete.rp invalid.rp
printf '\004' | dd of=invalid.rp bs=1 seek=6 conv=notrunc 2>/dev/null
reject
grep -q 'unsupported binary profile header' stderr
cp complete.rp invalid.rp
printf '\377' | dd of=invalid.rp bs=1 seek=12 conv=notrunc 2>/dev/null
reject
grep -q 'unknown binary record tag' stderr
cp complete.rp invalid.rp
printf '\002' | dd of=invalid.rp bs=1 seek=29 conv=notrunc 2>/dev/null
reject
grep -q 'invalid GC enabled flag' stderr
cp complete.rp invalid.rp
printf '\377\377\377\377' | dd of=invalid.rp bs=1 seek=37 conv=notrunc 2>/dev/null
reject
grep -q 'short binary string' stderr
cp complete.rp invalid.rp
printf '\000\000\000\000' | dd of=invalid.rp bs=1 seek=49 conv=notrunc 2>/dev/null
reject
grep -q 'empty binary record' stderr
cp complete.rp invalid.rp
printf '\001\000\000\000' | dd of=invalid.rp bs=1 seek=49 conv=notrunc 2>/dev/null
reject
grep -q 'short binary record' stderr
# JSON text is output only, not a second input format.
cp complete.json invalid.rp
reject
# JSON output gets the same alias protection as HTML and SVG.
ln complete.rp alias.rp
if "$RPVIEW" complete.rp --format json -o alias.rp >stdout 2>stderr; then exit 1; fi
grep -q 'input and output' stderr
echo 'Binary: golden bytes, uint64, JSON/stdout, truncation, malformed frames and aliases passed'
