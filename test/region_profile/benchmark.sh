#!/bin/sh
# Seven timed runs per mode. POSIX time reports seconds; small differences below
# its platform-dependent resolution are not meaningful.
set -eu
[ "$#" -eq 2 ] || { echo "Usage: sh benchmark.sh PLAIN INSTRUMENTED" >&2; exit 1; }
plain=$1
instrumented=$2
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-bench.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
for mode in plain instrumented-disabled paused 1ms 10ms 100ms; do
    exe=$instrumented
    set --
    case "$mode" in
        plain) exe=$plain ;;
        instrumented-disabled) ;;
        paused) set -- -rp -rp_paused ;;
        *) set -- -rp -rp_interval "$mode" ;;
    esac
    [ "$#" -eq 0 ] || set -- +RTS "$@" -rp_file "$OUT/profile.rp" -rp_report -RTS
    rm -f "$OUT/profile.rp"
    : > "$OUT/times"
    i=0
    while [ "$i" -lt 7 ]; do
        # Keep runtime stderr separate from time's stderr without depending on
        # the nonportable time -o option.
        LC_ALL=C /usr/bin/time -p sh -c 'report=$1; shift; exec "$@" 2> "$report"' sh "$OUT/report" env RP_ITERATIONS=1000000000 "$exe" "$@" > "$OUT/stdout" 2> "$OUT/time"
        awk '$1=="real" {print $2}' "$OUT/time" >> "$OUT/times"
        i=$((i+1))
    done
    median=$(sort -n "$OUT/times" | sed -n '4p')
    bytes=0
    if [ "$#" -ne 0 ]; then
        [ -f "$OUT/profile.rp" ]
        bytes=$(wc -c < "$OUT/profile.rp" | tr -d ' ')
    fi
    printf '%s median_seconds=%s bytes=%s\n' "$mode" "$median" "$bytes"
    cat "$OUT/report"
done
