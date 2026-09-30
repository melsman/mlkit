#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d /tmp/rp-live.XXXXXX)
child=
trap '[ -z "$child" ] || kill "$child" 2>/dev/null || :; rm -rf "$OUT"' EXIT HUP INT TERM
${CC:-cc} -std=gnu99 -Wall -Wextra -Werror "$ROOT/test/region_profile/live-client.c" -o "$OUT/client"
RP_ITERATIONS=1000000000 sh "$ROOT/test/region_profile/with-timeout.sh" 20 "$1" -rp -rp_paused -rp_interval 0 -rp_control "$OUT/control.sock" -rp_file "$OUT/live.rp" > "$OUT/stdout" 2> "$OUT/stderr" &
child=$!
if ! "$OUT/client" "$OUT/control.sock"; then cat "$OUT/stderr" >&2; exit 1; fi
wait "$child"
child=
[ ! -e "$OUT/control.sock" ]
awk -f "$ROOT/test/region_profile/records.awk" -f - "$OUT/live.rp" <<'AWK'
f["type"]=="sample_begin" { reasons=reasons " " f["reason"] }
END { need(reasons==" explicit start pause explicit","control sequence") }
AWK
echo 'Live commands and socket cleanup passed'
