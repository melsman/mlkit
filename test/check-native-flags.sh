#!/bin/sh
# Usage: check-native-flags.sh MLKIT_ARM64 MLKIT_X64 BARRY SMLTOJS
set -eu
[ "$#" -eq 4 ] || { echo "Usage: $0 MLKIT_ARM64 MLKIT_X64 BARRY SMLTOJS" >&2; exit 2; }
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-native-flags.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
options='region_profile region_inference print_region_flow_graph print_all_program_points rp_paused rp_report rp_gc_samples rp_file rp_interval'
for compiler in "$1" "$2"; do
  "$compiler" -help > "$OUT/help" 2>&1
  for option in $options; do
    grep -q -- "--$option" "$OUT/help"
  done
done
shift 2
for compiler in "$@"; do
  "$compiler" -help > "$OUT/help" 2>&1
  for option in $options; do
    ! grep -q -- "--$option" "$OUT/help"
  done
  for option in $options rp ri no_ri Prfg Ppp; do
    if "$compiler" "-$option" > "$OUT/error" 2>&1; then
      echo "$compiler unexpectedly accepted -$option" >&2
      exit 1
    fi
    grep -q 'unknown option' "$OUT/error"
  done
done
echo 'Native options are shared by ARM64/X64 and absent from Barry/SMLtoJs'
