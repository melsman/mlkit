#!/bin/sh
# Execute compiler-generated X64 equality and inequality leaf functions. No X64
# ML runtime is needed: wrappers translate the ML return register to the C ABI.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
MLKIT_X64=${MLKIT_X64:-$root/bin/mlkit}
export SML_LIB=${SML_LIB:-$root}
CC=${CC:-cc}
case "$(uname -s)" in
  Darwin) archflags='-arch x86_64'; prefix=_ ;;
  *) archflags=; prefix= ;;
esac
out=$(mktemp -d)
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$out"/*.log; do [ ! -f "$log" ] || tail -40 "$log"; done
fi
if [ "${KEEP_REAL_EQUALITY:-0}" = 1 ]; then echo "X64 equality outputs: $out"
else rm -rf "$out"; fi
exit "$status"' EXIT
cd "$out"
for mode in no_gc gc gengc; do
  mkdir "$mode"
  cp "$root/test/real-equality.sml" "$mode/main.sml"
  # Only assemble here; the runtime-independent leaf functions are linked below.
  "$MLKIT_X64" -"$mode" -no_par --no_delete_target_files \
    -as "$CC $archflags -c" -ldexe true -o "$mode/unused" "$mode/main.sml" > "$mode.log" 2>&1
  asm=$(find "$mode/MLB" -name main.sml.s)
  [ -n "$asm" ]
  for kind in equal branch unequal unequalBranch; do
    awk -v kind="$kind" '
      $0 ~ "^_?F[.]" kind "__noinline" { active=1 }
      $0 ~ "^_?mlkit_tp_end_" kind "__noinline" { active=0 }
      active { print }
    ' "$asm" > "$mode/$kind.s"
    [ "$(grep -c ucomisd "$mode/$kind.s")" = 1 ]
    grep -Eq '(jp |cmovp)' "$mode/$kind.s"
  done
  {
    printf '.text\n'
    for kind in equal branch unequal unequalBranch; do
      leaf_label=$(sed -n '1s/:$//p' "$mode/$kind.s")
      case "$kind" in
        equal) wrapper=equality_boolean ;;
        branch) wrapper=equality_branch ;;
        unequal) wrapper=inequality_boolean ;;
        unequalBranch) wrapper=inequality_branch ;;
      esac
      cat "$mode/$kind.s"
      printf '.globl %s%s\n%s%s:\n' "$prefix" "$wrapper" "$prefix" "$wrapper"
      printf '\tpushq %%rbx\n\tcall %s\n\tpopq %%rbx\n\tmovq %%rdi,%%rax\n' "$leaf_label"
      case "$kind" in
        branch|unequalBranch) [ "$mode" = no_gc ] || printf '\tsarq $1,%%rax\n' ;;
      esac
      printf '\tret\n'
    done
  } > "$mode/leaves.s"
  $CC $archflags -O2 -Wall -Wextra -Werror \
    "$root/src/Compiler/Backend/X64/tests/real-equality.c" "$mode/leaves.s" -o "$mode/check"
  "$mode/check"
  echo "Generated Real.==/!= ($mode): OK"
done
