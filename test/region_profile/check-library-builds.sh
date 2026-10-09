#!/bin/sh
# Check make's cache-level scheduling without compiling the Basis.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-library-builds.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
mkdir -p "$OUT/basis" "$OUT/kitlib"
cp "$ROOT/basis/Makefile" "$OUT/basis/Makefile"
cat > "$OUT/compiler" <<'SH'
#!/bin/sh
set -eu
variant=ri
profile=
for arg do
  case "$arg" in
    -gc) variant=gc ;;
    -par) variant=par ;;
    -rp|-region_profile) profile=_rp ;;
    *.mlb) source=$arg ;;
  esac
done
variant=$variant$profile
case "$source" in
  repl.mlb) stage=basis ;;
  ../kitlib/region-profile.mlb) stage=api ;;
  reml.mlb) stage=reml ;;
  kitlib.mlb) stage=kit ;;
  *) exit 1 ;;
esac
# Overlapping stages in the same variant would race on compiler caches.
mkdir "$OUT/active-$variant"
printf 'start %s %s\n' "$variant" "$stage" >> "$OUT/events"
sleep 0.1
printf 'end %s %s\n' "$variant" "$stage" >> "$OUT/events"
rmdir "$OUT/active-$variant"
SH
chmod +x "$OUT/compiler"
export OUT
# Match the top-level make's serialized compiler build and recursive submake.
cat > "$OUT/Makefile" <<'MAKE'
.NOTPARALLEL:
all:
	$(MAKE) -C basis mlkit_basislibs mlkit_kitlibs MLKIT=../compiler REML=../compiler
MAKE
make -j6 -C "$OUT" > "$OUT/build.log" 2>&1 || { cat "$OUT/build.log"; exit 1; }
awk '
  $1 == "start" {
    active++; if(active>peak) peak=active;
    if($3=="basis" && done[$2]!="") exit 1;
    if($3=="api" && done[$2]!="basis") exit 2;
    if($3=="reml" && done[$2]!="api") exit 3;
    if($3=="kit" && done[$2]!=($2 ~ /^gc/ ? "api" : "reml")) exit 4;
    starts++;
  }
  $1 == "end" { active--; done[$2]=$3; if($3=="kit") completed++ }
  END { if(active || peak<2 || starts!=22 || completed!=6) exit 5;
        print "Library builds: concurrent variants, ordered stages, all six configurations passed" }
' "$OUT/events"
