#!/bin/sh
set -eu
: "${MLKIT_ARM64:?Set MLKIT_ARM64 to the ARM64-emitting compiler}"
: "${SML_LIB:?Set SML_LIB to the MLKit source/install directory}"
cd "$(dirname "$0")"
output=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-arm64-switches.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$output"/*.log; fi
if [ "${ARM64_KEEP_TEST_OUTPUTS:-0}" = 1 ]; then echo "Switch outputs: $output"; else rm -rf "$output"; fi
exit "$status"' EXIT
cp switches.sml switch-boundaries.sml "$output/"
cd "$output"
printf 'OK\n' > expected
for mode in no_gc gc; do
  "$MLKIT_ARM64" -"$mode" --no_basislib --no_delete_target_files \
    -o switches switches.sml > "switches-$mode.log" 2>&1
  ./switches > actual
  cmp expected actual
  "$MLKIT_ARM64" -"$mode" --mlb-subdir "${ARM64_SWITCH_CACHE:-SwitchTests}" \
    --no_delete_target_files -o boundaries switch-boundaries.sml > "boundaries-$mode.log" 2>&1
  ./boundaries > actual
  cmp expected actual
  # Exercise relative table entries after loading a separate REPL image.
  awk '/^val \(\) = check/ && !switched { print ";"; switched=1 }
       { print } END { print ";\n:quit;" }' switches.sml > repl-input
  "$MLKIT_ARM64" -"$mode" --no_basislib < repl-input > "repl-$mode.log" 2>&1
  grep -q 'OK' "repl-$mode.log"
  if grep -Eq 'Compile error|uncaught exception' "repl-$mode.log"; then exit 1; fi
done
grep -q 'ldr x16, \[x17, x16, lsl #3\]' MLB/ARM64_*/switches.sml.s
grep -q '^.quad .* - ' MLB/ARM64_*/switches.sml.s
printf 'ARM64 dense/sparse/default, constructor, precision-boundary, and REPL switches passed\n'
