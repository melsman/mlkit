#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/ir-link-map.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
"${MLKIT:-$ROOT/bin/mlkit}" -mlb-subdir PrettyPrintTests -gc \
  -o "$OUT/generate" "$ROOT/test/prettyprint/link-map.mlb"
"$OUT/generate" "$OUT"
cat > "$OUT/check.c" <<'C'
#include <assert.h>
#include <stddef.h>
#include <string.h>
extern const char *const mlkit_rp_ir_objects[][2];
int main(int argc, char **argv) {
#ifdef EMPTY
  (void)argc; (void)argv;
  assert(!mlkit_rp_ir_objects[0][0] && !mlkit_rp_ir_objects[0][1]);
#else
  assert(argc == 3);
  assert(!strcmp(mlkit_rp_ir_objects[0][0], "first"));
  assert(!strcmp(mlkit_rp_ir_objects[0][1], argv[1]));
  assert(!strcmp(mlkit_rp_ir_objects[1][0], "second"));
  assert(!strcmp(mlkit_rp_ir_objects[1][1], argv[2]));
  assert(!mlkit_rp_ir_objects[2][0] && !mlkit_rp_ir_objects[2][1]);
#endif
  return 0;
}
C
case $(uname -s) in Darwin) platform=darwin;; *) platform=elf;; esac
# CC may include target arguments for a cross-architecture check.
${CC:-cc} "$OUT/$platform.s" "$OUT/check.c" -o "$OUT/check"
"$OUT/check" "$(cat "$OUT/first-path")" "$(cat "$OUT/second-path")"
${CC:-cc} -DEMPTY "$OUT/$platform-empty.s" "$OUT/check.c" -o "$OUT/empty"
"$OUT/empty"
echo 'IR map assembly: pointers, path bytes, missing companions and terminator passed'
