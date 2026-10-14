#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT_ARM64:-$ROOT/bin/mlkit}
PVIEW=${PVIEW:-$ROOT/bin/pview}
CC=${CC:-cc}
export SML_LIB=${SML_LIB:-$ROOT}
case "$(uname -s)/$(uname -m)" in
  Darwin/arm64) ;;
  *) echo 'Real auto-conversion execution checks require Apple Silicon' >&2; exit 1 ;;
esac
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-real-auto-native.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$OUT"/*/*.log; do [ ! -f "$log" ] || tail -60 "$log"; done
fi
if [ "${REAL_AUTO_KEEP_OUTPUTS:-0}" = 1 ]; then echo "Real auto-conversion outputs: $OUT"
else rm -rf "$OUT"; fi
exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
$CC -arch arm64 -O2 -Wall -Wextra -Werror -c "$ROOT/test/real_auto_conversion/native.c" -o "$OUT/plain.o"
$CC -arch arm64 -O2 -Wall -Wextra -Werror -DREAL_AUTO_GC -c "$ROOT/test/real_auto_conversion/native.c" -o "$OUT/gc.o"
printf 'real auto-conversion passed\n' > "$OUT/expected"
for mode in plain gc gengc prof gcprof gengcprof gcspill gcnoopt plainnoopt forced explicit; do
  case "$mode" in
    plain) flags=-no_gc; obj=plain ;;
    gc) flags=-gc; obj=gc ;;
    gengc) flags=-gengc; obj=gc ;;
    prof) flags='-no_gc -rp'; obj=plain ;;
    gcprof) flags='-gc -rp'; obj=gc ;;
    gengcprof) flags='-gengc -rp'; obj=gc ;;
    gcspill) flags='-gc --no_register_allocation'; obj=gc ;;
    gcnoopt) flags='-gc -no_opt'; obj=gc ;;
    plainnoopt) flags='-no_gc -no_opt'; obj=plain ;;
    forced) flags='-gc -extra_gc_checks'; obj=gc ;;
    explicit) flags='--reml -no_gc'; obj=plain ;;
  esac
  mkdir "$OUT/$mode"
  if [ "$mode" = explicit ]; then fixture=explicit-native; else fixture=native; fi
  cp "$ROOT/test/real_auto_conversion/$fixture.sml" "$OUT/$mode/main.sml"
  case "$mode" in
    *prof)
      # Sample while the result list is still live, before its traversal.
      awk '/^val \(\) = roots__noinline/ {
        print "val () : unit = prim (\"mlkit_rp_sample\", ())"
      } { print }' "$OUT/$mode/main.sml" > "$OUT/$mode/sampled.sml"
      mv "$OUT/$mode/sampled.sml" "$OUT/$mode/main.sml"
      ;;
  esac
  (cd "$OUT/$mode" && "$MLKIT" $flags --no_basislib --no_delete_target_files \
    -ldexe "$CC -arch arm64 $OUT/$obj.o" -o program main.sml > build.log 2>&1)
  case "$mode" in
    *prof)
      "$OUT/$mode/program" +RTS -rp -rp_interval 0 -rp_region all -p_file "$OUT/$mode/profile.prof" -RTS > "$OUT/$mode/actual"
      "$PVIEW" "$OUT/$mode/profile.prof" --format json > "$OUT/$mode/profile.json"
      node - "$OUT/$mode/profile.json" <<'JS'
const fs = require('fs'), assert = require('assert');
const rows = fs.readFileSync(process.argv[2], 'utf8').trim().split('\n').map(JSON.parse);
const sites = rows.filter(r => r.type === 'allocation_site' && r.location_kind === 1 &&
                             r.function.includes('collect__noinline'));
assert(sites.length > 0, 'Missing boxed real result allocation sites');
assert(rows.some(r => r.type === 'allocation' && sites.some(s => s.definition === r.definition) && r.count === 500),
       'Missing recorded allocation of boxed real results');
JS
      ;;
    *) "$OUT/$mode/program" > "$OUT/$mode/actual" ;;
  esac
  cmp "$OUT/expected" "$OUT/$mode/actual"
  echo "Arm64 real auto-conversion: $mode passed"
done
# Runtime checks establish C argument values and exact double bit patterns.
# Assembly checks additionally establish FP-register placement and no boxing
# on the foreign boundary. Both paired and single loads are valid.
asm="$OUT/gc/MLB/ARM64_RI_GC/main.sml.s"
awk '
  /^_F.identity__noinline/ { emit = 1 }
  emit && /^\.globl / { emit = 0 }
  emit { print }
' "$asm" > "$OUT/identity.s"
grep -q 'ldr d0, \[sp,' "$OUT/identity.s"
grep -q 'bl _real_auto_identity' "$OUT/identity.s"
grep -Eq 'str d0,|fmov d[0-9]+, d0' "$OUT/identity.s"
awk '
  { lines[NR % 120] = $0 }
  /bl _real_auto_mixed$/ {
    for (i = NR - 110; i <= NR; i++) if (i > 0) print lines[i % 120]
  }
' "$asm" > "$OUT/mixed.s"
for reg in 0 1 2 3 4 5 6 7; do
  grep -Eq "ld(r|p).*d$reg[, ]" "$OUT/mixed.s"
  grep -Eq "ld(r|p).*x$reg[, ]" "$OUT/mixed.s"
done
for offset in 0 8 16 24; do
  grep -q "add x17, sp, #$offset$" "$OUT/mixed.s"
done
grep -q 'str x16, \[x17, #0\]' "$OUT/mixed.s"
# Optimised live__noinline must actually exercise unboxed values across C.
awk '
  /^_F.live__noinline/ { emit = 1 }
  emit && /^\.globl / { emit = 0 }
  emit { print }
' "$asm" > "$OUT/live.s"
grep -q 'bl _real_auto_clobber' "$OUT/live.s"
grep -q 'bl _real_auto_nine' "$OUT/live.s"
grep -Eq 'fadd d[0-9]+,' "$OUT/live.s"
echo 'Arm64 real auto-conversion assembly checks passed'
