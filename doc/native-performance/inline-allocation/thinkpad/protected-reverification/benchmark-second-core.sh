#!/bin/sh
# Additional long-workload confirmation on the noisier second performance core.
set -eu
: "${REVERIFY_BENCHMARK:?Set REVERIFY_BENCHMARK}"
cd "$REVERIFY_BENCHMARK"
for variant in baseline current; do
  taskset -c 2 "./long/$variant" > actual
  grep -qx ok actual
done
printf 'workload,cpu,block,position,variant,real,user,sys\n' > second-core-samples.csv
for block in 1 2 3 4 5; do
  order='baseline current current baseline'
  [ $((block % 2)) -ne 0 ] || order='current baseline baseline current'
  position=0
  for variant in $order; do
    position=$((position+1))
    taskset -c 2 /usr/bin/time -p "./long/$variant" > actual 2> timing
    grep -qx ok actual
    awk -v b="$block" -v p="$position" -v v="$variant" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "long,2,%s,%s,%s,%s,%s,%s\n",b,p,v,real,user,sys}
    ' timing >> second-core-samples.csv
  done
  printf 'Long second-core block %s passed.\n' "$block"
done
