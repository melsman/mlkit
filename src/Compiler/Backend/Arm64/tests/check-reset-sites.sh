#!/bin/sh
# Correctness guard: compiled eligible resets must never enter the C fallback.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${REML:?Set REML}"
case $(uname -sm) in 'Darwin arm64') ;; *) exit 1;; esac
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-reset-sites.XXXXXX")
echo "Reset-site outputs: $scratch"
cd "$scratch"
cat > unexpected.c <<'C'
#include <stdlib.h>
void *unexpected_reset(void *r) { (void)r; abort(); }
C
cc -arch arm64 -O2 -Wall -Wextra -Werror -c unexpected.c -o unexpected.o
cat > assembler <<'SH'
#!/bin/sh
set -eu
[ "$#" = 3 ] && [ "$1" = -o ]
sed 's/_resetRegion/_unexpected_reset/g' "$3" > "$3.checked.s"
exec cc -arch arm64 -c -o "$2" "$3.checked.s"
SH
chmod +x assembler
for mode in seq par prof; do
  mkdir "$mode"; cd "$mode"
  cp "$SML_LIB/src/Compiler/Backend/Arm64/tests/reset-sites.sml" reset.sml
  flags=-no_par
  [ "$mode" != par ] || flags=-par
  [ "$mode" != prof ] || flags='-no_par -rp'
  "$REML" $flags --no_basislib --no_delete_target_files \
    --assembler "$scratch/assembler" -ldexe "cc -arch arm64 $scratch/unexpected.o" \
    -o reset reset.sml > compile.log 2>&1 || { cat compile.log; exit 1; }
  grep -q '_unexpected_reset' MLB/*/base-link_objects.s.checked.s
  ./reset > actual
  grep -qx 'reset sites passed' actual
  echo "Eligible compiled reset sites ($mode): no C fallback"
  cd ..
done
