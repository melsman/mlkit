#!/bin/sh
# Confirm the screened fast/slow offsets with independent nine-run samples.
set -eu
: "${LAYOUT_BENCHMARK:?Set LAYOUT_BENCHMARK to the experiment output directory}"
cd "$LAYOUT_BENCHMARK"
printf 'phase,run,method,offset,real,user,sys\n' > confirmation.csv
for run in 1 2 3 4 5 6 7 8 9; do
  variants='reference-old reference-current fp-0 fp-17 fp-38 fp-44'
  [ $((run % 2)) -ne 0 ] || variants='fp-44 fp-38 fp-17 fp-0 reference-current reference-old'
  for variant in $variants; do
    method=fp;offset=${variant#fp-}
    case $variant in reference-*) method=$variant;offset=-1;; esac
    /usr/bin/time -p "./$variant" > actual 2> timing
    grep -qx ok actual
    awk -v r="$run" -v m="$method" -v o="$offset" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "confirmation,%s,%s,%s,%s,%s,%s\n",r,m,o,real,user,sys}
    ' timing >> confirmation.csv
  done
  printf 'Confirmation round %s passed.\n' "$run"
done
