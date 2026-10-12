#!/bin/sh
# Run against a staged, read-only installation, after build_basislibs,
# install_basis and install_mlkit_basislibs. Clients use their own writable cache.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
: "${SML_LIB:?Set SML_LIB to a read-only installation}"
MLKIT=${MLKIT:-$ROOT/bin/mlkit-arm64}
REML=${REML:-$ROOT/bin/reml-arm64}
PVIEW=${PVIEW:-$ROOT/bin/pview}
[ ! -w "$SML_LIB/basis" ] && [ ! -w "$SML_LIB/kitlib" ] || {
  echo 'Basis and kitlib must be read-only for this check' >&2; exit 1;
}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-installed-rp.XXXXXX")
echo "Installed profiler API artifacts: $OUT"
find "$SML_LIB/basis" "$SML_LIB/kitlib" -type f -exec cksum {} + | sort > "$OUT/before"
cat > "$OUT/client.mlb" <<'MLB'
$(SML_LIB)/kitlib/region-profile.mlb
$(SML_LIB)/kitlib/time-profile.mlb
$(SML_LIB)/basis/basis.mlb
client.sml
MLB
cat > "$OUT/client.sml" <<'SML'
val () = TimeProfile.start ()
val () = TimeProfile.pause ()
val () = TimeProfile.flush ()
val () = RegionProfile.start ()
val () = RegionProfile.mark "installed API"
val () = RegionProfile.sample ()
val () = RegionProfile.pause ()
val () = RegionProfile.flush ()
val () = print "profile api ok\n"
SML
printf 'profile api ok\n' > "$OUT/expected"
n=0
run () {
  compiler=$1
  shift
  n=$((n+1))
  (cd "$OUT"
   "$compiler" "$@" -o client client.mlb > "build-$n.log" 2>&1
   case " $* " in
     *' -rp '*) ./client +RTS -rp -rp_interval 0 -p_file "profile-$n.rp" > actual
       "$PVIEW" "profile-$n.rp" --format json > "profile-$n.json"
       grep -q 'sample_begin' "profile-$n.json" ;;
     *) ./client > actual ;;
   esac
   cmp expected actual)
  echo "Installed API passed: $compiler $*"
}
run "$MLKIT" -no_gc
run "$MLKIT" -no_gc -par
run "$MLKIT" -gc
run "$MLKIT" -no_gc -rp
run "$MLKIT" -no_gc -par -rp
run "$MLKIT" -gc -rp
# Time-only activation must also work without modifying installed caches.
case $(uname -sm) in
 'Darwin arm64'|'Linux x86_64')
  for flags in '-no_gc -no_par' '-gc -no_par' '-no_gc -par'; do
   n=$((n+1))
   (cd "$OUT"
    "$MLKIT" $flags -o time-client client.mlb > "build-$n.log" 2>&1
    ./time-client +RTS -tp -tp_paused -p_file "time-$n.prof" > actual
    cmp expected actual
    "$PVIEW" "time-$n.prof" --format json > "time-$n.json"
    grep -q 'time_session' "time-$n.json"
    if grep -q 'sample_begin' "time-$n.json"; then exit 1; fi
    "$PVIEW" "time-$n.prof" --ir-dir "$SML_LIB/basis/MLB" --ir-dir "$SML_LIB/kitlib/MLB" --ir-dir MLB -o "time-$n.html" > /dev/null)
   echo "Installed time API passed: $MLKIT $flags"
  done ;;
esac
# ReML must also reuse the explicit-region API, including region parameters.
printf '$(SML_LIB)/basis/reml.mlb\nregion.sml\n' >> "$OUT/client.mlb"
cat > "$OUT/region.sml" <<'SML'
val () = if Region.getPageSizeBytes () > 0 then () else raise Fail "page size"
val () =
  let with r
      val a = Array.array (8, 1)`r
      val n = Region.numPagesOfRegion `[r] ()
  in if n >= 0 andalso Array.sub (a, 0) = 1 then ()
     else raise Fail "explicit region"
  end
SML
run "$REML" -no_par
run "$REML" -no_par -rp
run "$REML" -par
run "$REML" -par -rp
find "$SML_LIB/basis" "$SML_LIB/kitlib" -type f -exec cksum {} + | sort > "$OUT/after"
cmp "$OUT/before" "$OUT/after"
echo 'Read-only installed region/time APIs and Region checks passed; installed files unchanged'
