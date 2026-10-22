#!/bin/sh
# Correctness only: exercise emitted shared reset helpers against real C reset.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
fixtures=$SML_LIB/src/Compiler/Backend
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-reset.XXXXXX")
echo "Reset outputs: $scratch"
cd "$scratch"
case $(uname -sm) in
  'Darwin arm64') arch=Arm64; runtime=$SML_LIB/lib/darwin-arm64; prefix=_; bridge=$fixtures/Arm64/tests/reset-bridge.s; symbol=mlkit_arm64_reset_preserving;;
  'Linux x86_64') arch=X64; runtime=$SML_LIB/lib; prefix=; bridge=$fixtures/X64/tests/reset-bridge.S; symbol=__reset_region;;
  *) echo 'Requires native ARM64 macOS or X64 Linux' >&2; exit 1;;
esac
printf 'val _:unit = prim("putchar",0w65)\n' > helper.sml
for mode in no_gc gc gengc prof gcprof genprof par parprof; do
  flags=-$mode; defines=; lib=runtimeSystem
  case $mode in
    gc) defines=-DENABLE_GC; lib=runtimeSystemGC;;
    gengc) defines='-DENABLE_GC -DENABLE_GEN_GC'; lib=runtimeSystemGenGC;;
    prof) flags='-no_gc -rp'; defines=-DPROFILING; lib=runtimeSystemProf;;
    gcprof) flags='-gc -rp'; defines='-DENABLE_GC -DPROFILING'; lib=runtimeSystemGCProf;;
    genprof) flags='-gengc -rp'; defines='-DENABLE_GC -DENABLE_GEN_GC -DPROFILING'; lib=runtimeSystemGenGCProf;;
    par) flags='-no_gc -par'; defines=-DPARALLEL; lib=runtimeSystemPar;;
    parprof) flags='-no_gc -par -rp'; defines='-DPARALLEL -DPROFILING'; lib=runtimeSystemParProf;;
  esac
  "$MLKIT" $flags --no_basislib --no_delete_target_files --mlb-subdir "Reset$mode" \
    -o helper helper.sml > "$mode-compile.log" 2>&1 || { cat "$mode-compile.log"; exit 1; }
  link=$(find MLB -path "*Reset$mode/*" -name base-link_objects.s)
  [ -n "$link" ]
  awk -v start=".globl $prefix$symbol" -v arch="$arch" '
    $0 == start {emit=1; print ".text"}
    emit && /^.globl / && $0 != start && $0 != ".globl _mlkit_arm64_reset_slow" {exit}
    emit {print}
  ' "$link" > "$mode-helper.s"
  awk -v lab="$prefix$symbol:" '
    $0 == lab {emit=1}
    emit {print}
    emit && /(^|[[:space:]])ret([[:space:]]|$)/ {exit}
  ' "$mode-helper.s" > "$mode-fast.s"
  if grep -Eq '%rsp|push|pop|\[sp|sp,' "$mode-fast.s"; then
    echo "Reset fast path uses explicit stack storage ($mode)" >&2; exit 1
  fi
  # Rename only the C target, leaving the generated helper instructions intact.
  sed 's/resetRegion/counted_reset/g' "$mode-helper.s" > counted.s
  if [ "$arch" = X64 ]; then printf '\n.section .note.GNU-stack,"",@progbits\n' >> counted.s; fi
  ${CC:-cc} -O2 -Wall -Wextra -Werror $defines -DFAST_RESET \
    -iquote "$SML_LIB/src/Runtime" "$fixtures/tests/reset-region.c" \
    "$bridge" counted.s "$runtime/$lib.a" -lm -pthread -o reset
  ./reset > actual
  grep -qx 'generated/C reset passed' actual
  echo "Shared reset ($mode): passed"
done
