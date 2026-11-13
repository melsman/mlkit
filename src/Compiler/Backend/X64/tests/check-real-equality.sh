#!/bin/sh
# Check selection guards and execute generated X64 leaf functions. No X64
# ML runtime is needed: wrappers translate the ML return register to the C ABI.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
MLKIT_HOST=${MLKIT_HOST:-$root/bin/mlkit}
MLKIT_X64=${MLKIT_X64:-$MLKIT_HOST}
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
"$MLKIT_HOST" -no_gc -no_par -o "$out/emitter" \
  "$root/src/Compiler/Backend/X64/tests/real-equality.mlb" > "$out/emitter.log" 2>&1
"$out/emitter" "$out/probe"
$CC $archflags -O2 -Wall -Wextra -Werror \
  "$root/src/Compiler/Backend/X64/tests/real-equality.c" \
  "$out/probe-boolean.s" "$out/probe-branch.s" -o "$out/probe"
"$out/probe"
cd "$out"
for mode in no_gc gc gengc; do
  mkdir "$mode"
  cp "$root/test/real-equality.sml" "$mode/main.sml"
  # Only assemble here; the runtime-independent leaf functions are linked below.
  "$MLKIT_X64" -"$mode" -no_par --no_delete_target_files \
    -as "$CC $archflags -c" -ldexe true -o "$mode/unused" "$mode/main.sml" > "$mode.log" 2>&1
  asm=$(find "$mode/MLB" -name main.sml.s)
  [ -n "$asm" ]
  for kind in equal branch; do
    awk -v kind="$kind" '
      $0 ~ "^_?F[.]" kind "__noinline" { active=1 }
      $0 ~ "^_?mlkit_tp_end_" kind "__noinline" { active=0 }
      active { print }
    ' "$asm" > "$mode/$kind.s"
    [ "$(grep -c ucomisd "$mode/$kind.s")" = 1 ]
    grep -q 'jb ' "$mode/$kind.s"
  done
  eq_label=$(sed -n '1s/:$//p' "$mode/equal.s")
  branch_label=$(sed -n '1s/:$//p' "$mode/branch.s")
  {
    printf '.text\n'
    cat "$mode/equal.s" "$mode/branch.s"
    printf '.globl %sequality_boolean\n%sequality_boolean:\n' "$prefix" "$prefix"
    printf '\tpushq %%rbx\n\tcall %s\n\tpopq %%rbx\n\tmovq %%rdi,%%rax\n\tret\n' "$eq_label"
    printf '.globl %sequality_branch\n%sequality_branch:\n' "$prefix" "$prefix"
    printf '\tpushq %%rbx\n\tcall %s\n\tpopq %%rbx\n\tmovq %%rdi,%%rax\n' "$branch_label"
    [ "$mode" = no_gc ] || printf '\tsarq $1,%%rax\n'
    printf '\tret\n'
  } > "$mode/leaves.s"
  $CC $archflags -O2 -Wall -Wextra -Werror \
    "$root/src/Compiler/Backend/X64/tests/real-equality.c" "$mode/leaves.s" -o "$mode/check"
  "$mode/check"
  echo "Generated Real.== ($mode): OK"
done
