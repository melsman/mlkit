#!/bin/sh
# Native Linux X64 correctness only; retain outputs for generated-code review.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
fixtures=$SML_LIB/src/Compiler/Backend/X64/tests
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-c-preservation.XXXXXX")
echo "C preservation outputs: $scratch"
cd "$scratch"
cp "$fixtures/c-call-preservation.sml" .
${CC:-cc} -c "$fixtures/c-call-preservation.S" -o clobber.o
for mode in no_gc gc gengc prof gcprof par; do
  flags=-$mode
  case $mode in
    prof) flags='-no_gc -rp';;
    gcprof) flags='-gc -rp';;
    par) flags='-no_gc -par';;
  esac
  ${CC:-cc} -O2 -Wall -Wextra -Werror -c "$fixtures/c-call-preservation.c" -o probe.o
  "$MLKIT" --no_basislib --no_delete_target_files --mlb-subdir "Preserve$mode" \
    $flags -ldexe "${CC:-cc} probe.o clobber.o" -o probe c-call-preservation.sml > "$mode.log" 2>&1 || {
      cat "$mode.log"; exit 1;
    }
  ./probe > actual
  grep -qx 'C preservation passed' actual
  echo "C preservation ($mode): passed"
done
