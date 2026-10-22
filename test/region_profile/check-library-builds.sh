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
parallel=0
unprotected=0
gc=0
case "$0" in *compiler-reml) parallel=1; unprotected=1;; esac
profile=
for arg do
  case "$arg" in
    -gc) gc=1 ;;
    -no_gc) gc=0 ;;
    -par) parallel=1 ;;
    -no_par) parallel=0 ;;
    -par0)
      case "$0" in *compiler-mlkit) echo 'MLKit cannot build PAR0' >&2; exit 1;; esac
      unprotected=1 ;;
    -no_par0) echo 'Protected ReML is not a prepared library variant' >&2; exit 1 ;;
    -rp|-region_profile) profile=_rp ;;
    *.mlb) source=$arg ;;
  esac
done
variant=ri
if [ "$gc" = 1 ]; then variant=gc
elif [ "$parallel" = 1 ]; then
  variant=par
  [ "$unprotected" = 0 ] || variant=par0
fi
variant=$variant$profile
case "$source" in
  repl.mlb) stage=basis ;;
  ../kitlib/region-profile.mlb) stage=api ;;
  ../kitlib/time-profile.mlb) stage=time-api ;;
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
ln -s compiler "$OUT/compiler-mlkit"
ln -s compiler "$OUT/compiler-reml"
export OUT
# Match the top-level make's serialized compiler build and recursive submake.
cat > "$OUT/Makefile" <<'MAKE'
.NOTPARALLEL:
all:
	$(MAKE) -C basis mlkit_basislibs mlkit_kitlibs MLKIT=../compiler-mlkit REML=../compiler-reml
MAKE
make -j6 -C "$OUT" > "$OUT/build.log" 2>&1 || { cat "$OUT/build.log"; exit 1; }
awk '
  $1 == "start" {
    active++; if(active>peak) peak=active;
    if($3=="basis" && done[$2]!="") exit 1;
    if($3=="api" && done[$2]!="basis") exit 2;
    if($3=="time-api" && done[$2]!="api") exit 3;
    if($3=="reml" && done[$2]!="time-api") exit 3;
    if($3=="kit" && done[$2]!=($2 ~ /^(ri|ri_rp|par0|par0_rp)$/ ? "reml" : "time-api")) exit 4;
    starts++;
  }
  $1 == "end" { active--; done[$2]=$3; if($3=="kit") completed++ }
  END { if(active || peak<2 || starts!=36 || completed!=8) exit 5;
        print "Library builds: concurrent variants, ordered stages, all eight configurations passed" }
' "$OUT/events"
