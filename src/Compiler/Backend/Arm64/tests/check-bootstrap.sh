#!/bin/sh
# Force every stage to compile from source; never reuse a previous stage's cache.
set -eu
: "${SML_LIB:?Set SML_LIB to the source checkout}"
: "${BOOTSTRAP_COMPILER:?Set BOOTSTRAP_COMPILER to a working MLKit}"
BOOTSTRAP_TARGET=${BOOTSTRAP_TARGET:-arm64}
BOOTSTRAP_JOBS=${BOOTSTRAP_JOBS:-1}
case "$BOOTSTRAP_TARGET" in
  arm64) source=mlkitarm64.mlb
    default_linker='gcc -arch arm64 -Wl,-stack_size,0x10000000' ;;
  x86_64) source=mlkit64.mlb
    # Match the existing X64 bootstrap rule: the new linker can reorder GOT
    # entries between otherwise identical links.
    default_linker='gcc -arch x86_64 -Wl,-ld_classic,-stack_size,0x10000000' ;;
  *) echo 'Unsupported bootstrap target' >&2; exit 1 ;;
esac
BOOTSTRAP_LINKER=${BOOTSTRAP_LINKER:-$default_linker}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-bootstrap.XXXXXX")
echo "Bootstrap outputs: $scratch"
compiler=$BOOTSTRAP_COMPILER
# Include the unique run identity, so rerunning this script cannot reuse caches.
identity=$(basename "$scratch" | tr -cd '[:alnum:]')
cd "$SML_LIB"
for stage in 1 2 3; do
  mkdir "$scratch/stage$stage"
  output="$scratch/stage$stage/mlkit"
  if ! "$compiler" -gc -j "$BOOTSTRAP_JOBS" -ldexe "$BOOTSTRAP_LINKER" --mlb-subdir "${identity}Stage${stage}" \
       -o "$output" "src/Compiler/$source" > "$scratch/stage$stage.log" 2>&1; then
    tail -80 "$scratch/stage$stage.log" >&2; exit 1
  fi
  [ "$(lipo -archs "$output")" = "$BOOTSTRAP_TARGET" ]
  "$output" --version
  compiler=$output
done
# Strip copies, preserving the runnable stage outputs and their logs.
# Apple strip recreates ARM's ad-hoc signature using the filename as its
# identifier. Keep the same basename so that signatures compare as well.
mkdir "$scratch/compare2" "$scratch/compare3"
cp "$scratch/stage2/mlkit" "$scratch/compare2/mlkit"
cp "$scratch/stage3/mlkit" "$scratch/compare3/mlkit"
strip "$scratch/compare2/mlkit" "$scratch/compare3/mlkit"
cmp "$scratch/compare2/mlkit" "$scratch/compare3/mlkit"
echo "$BOOTSTRAP_TARGET bootstrap fixed point passed."
