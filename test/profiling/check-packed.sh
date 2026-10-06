#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-packed.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
for variant in plain gc gengc; do
  case "$variant" in
    plain) defs='';;
    gc) defs='-DENABLE_GC -DTAG_VALUES -DTAG_FREE_PAIRS';;
    gengc) defs='-DENABLE_GC -DENABLE_GEN_GC -DTAG_VALUES -DTAG_FREE_PAIRS';;
  esac
  ${CC:-cc} -std=gnu99 -Wall -Wextra -DPROFILING $defs -I"$ROOT/src/Runtime" \
    "$ROOT/test/profiling/packed-descriptor.c" -o "$OUT/descriptor"
  "$OUT/descriptor"
done
if [ -n "${MLKIT:-}" ]; then
  export SML_LIB=${SML_LIB:-$ROOT}
  for variant in plain gc gengc tagged-pairs; do
    case "$variant" in
      plain) flags='-no_gc';;
      gc) flags='-gc';;
      gengc) flags='-gc -generational_garbage_collection';;
      tagged-pairs) flags='-gc -tag_pairs';;
    esac
    "$MLKIT" $flags -rp -mlb-subdir packed -o "$OUT/program" test/profiling/packed.mlb > "$OUT/build.log" 2>&1 || { cat "$OUT/build.log"; exit 1; }
    if [ "$variant" = plain ]; then gcopts=''; else gcopts='-report_gc'; fi
    "$OUT/program" -rp -rp_interval 0 -rp_file "$OUT/profile.rp" $gcopts > "$OUT/output" 2> "$OUT/runtime.log" || { cat "$OUT/runtime.log"; exit 1; }
    if [ "$variant" != plain ]; then
      cat "$OUT/runtime.log"
      grep -Eq '[1-9][0-9]* collections' "$OUT/runtime.log"
    fi
    printf 'packed descriptors: OK\n' > "$OUT/expected"
    cmp "$OUT/expected" "$OUT/output"
    [ -s "$OUT/profile.rp" ]
    echo "packed descriptors: $variant passed"
  done
fi
