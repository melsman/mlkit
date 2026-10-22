#!/bin/sh
# Repeat the one recorded sample that crossed system suspend, without deleting it.
set -eu
: "${LAYOUT_BENCHMARK:?Set LAYOUT_BENCHMARK to the experiment output directory}"
cd "$LAYOUT_BENCHMARK"
./fp-11 > actual
grep -qx ok actual
/usr/bin/time -p ./fp-11 > actual 2> replacement-timing
grep -qx ok actual
printf 'phase,run,method,offset,real,user,sys\n' > replacement.csv
awk '
  $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
  END {printf "alignment,2,fp,11,%s,%s,%s\n",real,user,sys}
' replacement-timing >> replacement.csv
cat replacement.csv
