#!/bin/sh
# MLKIT_X64 may be a wrapper that selects an isolated Rosetta runtime.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
compiler=${MLKIT_X64:-$root/bin/mlkit}
export SML_LIB=${SML_LIB:-$root}
out=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-local-loops.XXXXXX")
echo "Local-loop outputs: $out"
for setting in on off checked; do
  case $setting in
    on) flags=--local_recursive_loops ;;
    off) flags=--no_local_recursive_loops ;;
    checked) flags='--local_recursive_loops --extra_gc_checks' ;;
  esac
  mkdir "$out/$setting"
  cp "$root/test/native-local-loops.sml" "$out/$setting/main.sml"
  "$compiler" -no_gc -no_par $flags --no_delete_target_files \
    -o "$out/$setting/run" "$out/$setting/main.sml" > "$out/$setting/build.log" 2>&1
  "$out/$setting/run" > "$out/$setting/output"
  diff -u "$root/test/native-local-loops.sml.out.ok" "$out/$setting/output"
  asm=$(find "$out/$setting/MLB" -name main.sml.s)
  if [ "$setting" = on ]; then
    grep -q '^L_local_loop_' "$asm"
    grep -q 'jmp L_local_loop_' "$asm"
  elif grep -q '^L_local_loop_' "$asm"; then
    echo "Unexpected local-loop emission: $setting" >&2; exit 1
  fi
done
echo 'X64 local-loop execution, selection, and fallback checks passed'
