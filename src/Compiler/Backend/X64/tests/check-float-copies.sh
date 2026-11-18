#!/bin/sh
# Check instruction selection as well as results: partial-register copies are
# numerically correct but can serialise otherwise independent loop iterations.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
compiler=${MLKIT_X64:-$root/bin/mlkit}
export SML_LIB=${SML_LIB:-$root}
out=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-float-copies.XXXXXX")
echo "X64 floating-point copy outputs: $out"
for mode in no_gc gc gengc; do
  for allocation in register_allocation no_register_allocation; do
    dir=$out/$mode-$allocation
    mkdir "$dir"
    cp "$root/src/Compiler/Backend/X64/tests/float-copies.sml" "$dir/main.sml"
    "$compiler" -"$mode" -no_par --"$allocation" --no_delete_target_files \
      -o "$dir/run" "$dir/main.sml" > "$dir/build.log" 2>&1
    "$dir/run" > "$dir/output"
    grep -qx 'X64 floating-point copies passed' "$dir/output"
    asm=$(find "$dir/MLB" -name main.sml.s)
    if [ "$allocation" = register_allocation ]; then
      grep -Eq 'movapd[[:space:]]+%xmm[0-9]+,[[:space:]]*%xmm[0-9]+' "$asm"
    fi
    if grep -Eq 'movsd[[:space:]]+%xmm[0-9]+,[[:space:]]*%xmm[0-9]+' "$asm"; then
      echo 'Unexpected partial-register floating-point copy' >&2
      exit 1
    fi
  done
done
echo 'X64 floating-point copy checks passed'
