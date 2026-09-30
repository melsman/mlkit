#!/bin/sh
# Portable watchdog; no GNU timeout dependency. Only signals children we own.
set -u
seconds=$1
shift
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-timeout.XXXXXX")
exec 3<&0
"$@" <&3 &
child=$!
watch=
cleanup() {
    [ -z "$watch" ] || kill "$watch" 2>/dev/null || :
    kill "$child" 2>/dev/null || :
    wait 2>/dev/null || :
    rm -rf "$OUT"
}
trap cleanup EXIT
trap 'exit 130' HUP INT TERM
(
    timer=
    trap '[ -z "$timer" ] || kill "$timer" 2>/dev/null; exit' HUP INT TERM
    sleep "$seconds" & timer=$!
    wait "$timer"
    : > "$OUT/expired"
    kill "$child" 2>/dev/null || :
) &
watch=$!
status=0
wait "$child" || status=$?
[ ! -f "$OUT/expired" ] || status=124
exit "$status"
