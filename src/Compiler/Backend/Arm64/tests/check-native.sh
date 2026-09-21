#!/bin/sh
set -eu
: "${MLKIT_ARM64:?Set MLKIT_ARM64 to the MLKit ARM64 compiler executable}"
: "${REML_ARM64:?Set REML_ARM64 to the ReML ARM64 compiler executable}"
: "${SML_LIB:?Set SML_LIB to the configured MLKit source/install directory}"
case "$(uname -s)/$(uname -m)" in
  Darwin/arm64) ;;
  *) echo 'Native execution checks require Apple Silicon' >&2; exit 1 ;;
esac
cd "$(dirname "$0")"
arm_test_dir=$(mktemp -d /tmp/mlkit-native-arm64.XXXXXX)
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$arm_test_dir"/*.log; do [ ! -f "$log" ] || cat "$log"; done
fi; rm -rf "$arm_test_dir"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cp input.sml output.sml native.mlb calls.sml branch.sml "$arm_test_dir/"
cd "$arm_test_dir"
printf 'A\n' > expected
check_compiler () {
  compiler=$1
  prefix=$2
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-native" native.mlb > "$prefix.log" 2>&1
  printf '@' | "./$prefix-native" > actual
  cmp expected actual
  file "$prefix-native" | grep -q 'Mach-O 64-bit executable arm64'
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-calls" calls.sml >> "$prefix.log" 2>&1
  printf '@' | "./$prefix-calls" > actual
  cmp expected actual
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-branch" branch.sml >> "$prefix.log" 2>&1
  printf '@' | "./$prefix-branch" > actual
  cmp expected actual
  printf 'B\n' > other
  printf 'A' | "./$prefix-branch" > actual
  cmp other actual
}
check_compiler "$MLKIT_ARM64" mlkit
# Reuse the source tree to exercise ReML's separate cache variant.
check_compiler "$REML_ARM64" reml
# Ensure the noinline sample really exercised nested ML calls and a tail branch.
grep -q 'bl _F.step__noinline' MLB/ARM64_*/calls.sml.s
grep -q 'b _F.step__noinline' MLB/ARM64_*/calls.sml.s
# Every generated object is ARM64, and X64 cache paths were not populated.
find MLB -name '*.o' -exec file {} \; > objects
if grep -v 'Mach-O 64-bit object arm64' objects; then exit 1; fi
[ ! -d MLB/RI ]
# Unsupported GC must fail explicitly, without producing an executable.
if "$MLKIT_ARM64" --no_basislib -gc -o unsupported native.mlb > unsupported.log 2>&1; then
  echo 'ARM GC was incorrectly accepted' >&2; exit 1
fi
grep -q 'ARM64 milestone 3 does not support garbage_collection' unsupported.log
[ ! -e unsupported ]
printf 'Native ARM64 MLKit/ReML arithmetic, branches, ML/tail/C calls, cross-unit data, and target guards passed\n'
