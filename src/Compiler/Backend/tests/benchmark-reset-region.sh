#!/bin/sh
# Manual only. Build both variants before collecting paired monotonic timings.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT_BEFORE:?Set MLKIT_BEFORE}"
: "${MLKIT_AFTER:?Set MLKIT_AFTER}"
fixtures=$SML_LIB/src/Compiler/Backend
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-reset-bench.XXXXXX")
echo "Reset benchmark outputs: $scratch"
cd "$scratch"
case $(uname -sm) in
  'Darwin arm64') arch=Arm64; runtime=$SML_LIB/lib/darwin-arm64; prefix=_; symbol=mlkit_arm64_reset_preserving
    printf '.text\n.globl _generated_reset\n_generated_reset:\n stp x29,x30,[sp,#-16]!\n mov x16,x0\n bl _mlkit_arm64_reset_preserving\n mov x0,x16\n ldp x29,x30,[sp],#16\n ret\n.globl _raise_exn\n_raise_exn:\n b _abort\n' > bridge.s;;
  'Linux x86_64') arch=X64; runtime=$SML_LIB/lib; prefix=; symbol=__reset_region
    printf '.text\n.globl generated_reset\ngenerated_reset:\n subq $8,%%rsp\n movq %%rdi,%%r11\n call __reset_region\n movq %%r11,%%rax\n addq $8,%%rsp\n ret\n.globl raise_exn\nraise_exn:\n jmp abort\n.section .note.GNU-stack,"",@progbits\n' > bridge.s;;
  *) exit 1;;
esac
for mode in ${RESET_MODES:-no_gc par prof gengc}; do
  flags=-$mode; defines=; lib=runtimeSystem
  case $mode in
    gengc) defines='-DENABLE_GC -DENABLE_GEN_GC'; lib=runtimeSystemGenGC;;
    prof) flags='-no_gc -rp'; defines=-DPROFILING; lib=runtimeSystemProf;;
    par) flags='-no_gc -par'; defines=-DPARALLEL; lib=runtimeSystemPar;;
  esac
  for variant in before after; do
    compiler=$MLKIT_BEFORE; [ "$variant" != after ] || compiler=$MLKIT_AFTER
    mkdir "$mode-$variant"; cd "$mode-$variant"
    printf 'val _:unit = prim("putchar",0w65)\n' > helper.sml
    "$compiler" $flags --no_basislib --no_delete_target_files -o helper helper.sml > compile.log 2>&1
    link=$(find MLB -name base-link_objects.s)
    awk -v start=".globl $prefix$symbol" '
      $0 == start {emit=1; print ".text"}
      emit && /^.globl / && $0 != start && $0 != ".globl _mlkit_arm64_reset_slow" {exit}
      emit {print}
    ' "$link" > reset.s
    [ "$arch" != X64 ] || printf '\n.section .note.GNU-stack,"",@progbits\n' >> reset.s
    ${CC:-cc} -O2 -Wall -Wextra -Werror $defines -iquote "$SML_LIB/src/Runtime" \
      "$fixtures/tests/reset-benchmark.c" ../bridge.s reset.s "$runtime/$lib.a" -lm -pthread -o reset
    size reset > size.txt
    cd ..
  done
done
printf 'mode,scenario,round,variant,seconds\n' > samples.csv
for mode in ${RESET_MODES:-no_gc par prof gengc}; do
  for scenario in single multi large mixed; do
    for round in 1 2 3 4 5; do
      order='before after'; [ $((round%2)) != 0 ] || order='after before'
      for variant in $order; do
        set --; [ "$arch" != X64 ] || set -- taskset -c "${BENCH_CPU:-0}"
        RESET_SCENARIO=$scenario "$@" "./$mode-$variant/reset" > actual 2> seconds
        grep -qx 'reset benchmark passed' actual
        printf '%s,%s,%s,%s,%s\n' "$mode" "$scenario" "$round" "$variant" "$(cat seconds)" >> samples.csv
      done
    done
  done
done
cat samples.csv
