#!/bin/sh
# Reproducible diagnostic comparison; no CI timing threshold.
set -eu
[ "$#" -eq 4 ] || { echo 'Usage: benchmark-allocation.sh RP_ONLY SELECTIVE GLOBAL RPVIEW' >&2; exit 1; }
plain=$1 selective=$2 global=$3 viewer=$4
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-allocation-bench.XXXXXX")
echo "Allocation benchmark artifacts: $OUT"
export AP_ITERATIONS=${AP_ITERATIONS:-10000000}
for mode in rp-only disabled selected unselected global-selected global-unselected; do
  exe=$selective
  case $mode in rp-only) exe=$plain;; global-*) exe=$global;; esac
  "$exe" -rp -rp_interval 0 -rp_file "$OUT/discovery.rp" > /dev/null
  "$viewer" "$OUT/discovery.rp" --format json > "$OUT/discovery.json"
  selector=$(sed -n 's/.*"binding":\([0-9]*\),"unit":"\([^"]*\)","name":"`r".*/\2:\1/p' "$OUT/discovery.json" | head -1)
  [ -n "$selector" ]
  set --
  case $mode in
    *unselected) set -- -rp -rp_interval 0 -rp_region '<global>:3';;
    *selected) set -- -rp -rp_interval 0 -rp_region "$selector";;
  esac
  [ "$#" -eq 0 ] || set -- "$@" -rp_report -rp_file "$OUT/$mode.rp"
  : > "$OUT/$mode.times"
  i=0
  while [ "$i" -lt 7 ]; do
    /usr/bin/time -p sh -c 'report=$1; shift; exec "$@" 2> "$report"' sh "$OUT/$mode.report" "$exe" "$@" > /dev/null 2> "$OUT/time"
    awk '$1=="real" {print $2}' "$OUT/time" >> "$OUT/$mode.times"
    i=$((i+1))
  done
  median=$(sort -n "$OUT/$mode.times" | sed -n '4p')
  bytes=0
  [ ! -f "$OUT/$mode.rp" ] || bytes=$(wc -c < "$OUT/$mode.rp" | tr -d ' ')
  printf '%s median_seconds=%s profile_bytes=%s executable_bytes=%s\n' "$mode" "$median" "$bytes" "$(wc -c < "$exe" | tr -d ' ')"
  cat "$OUT/$mode.report"
done
