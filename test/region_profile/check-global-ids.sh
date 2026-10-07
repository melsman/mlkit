#!/bin/sh
# Check the built-in global keys against Effect's toplevel region ordering.
set -eu
for mapping in top:1 string:3 pair:4 array:5 ref:6 triple:7; do
  type=${mapping%:*}
  id=${mapping#*:}
  grep '"type":"binding"' "$1" | grep '"unit":"<global>"' |
    grep "\"region_type\":\"$type\"" | grep -q "\"binding\":$id,"
done
