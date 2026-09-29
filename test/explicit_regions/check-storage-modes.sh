#!/bin/sh
# Exercise site annotations with and without Lambda optimisation.
set -eu
: "${REML:?Set REML to the ReML compiler executable}"
: "${MLKIT:?Set MLKIT to the Standard ML compiler executable}"
: "${SML_LIB:?Set SML_LIB to the source or installation directory}"
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-storage-modes.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$work"/*.log; do [ ! -f "$log" ] || cat "$log"; done
fi; rm -rf "$work"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cd "$work"
cat > good.sml <<'SML'
infix 6 +
infix 4 >
fun print (s:string) : unit = prim("printStringML",s)
fun deref (x:'a ref) : 'a = prim("!",x)
fun direct () =
    let with rs rp rr
        val a = "A"`atbot rs
        val b = "B"`attop rs
        val p = (a,b)`atbot rp
        val r = ref`atbot rr p
        val (a,b) = deref r
    in print a; print b
    end
fun f `r () : real = 5.4`sat r
fun g `r () : real = f `sat r ()
fun pair `[r1 r2] () : real * real = (5.4`sat r1,6.4`sat r2)
fun mix `r () : real =
    let with r2
        val (x,y) = pair `[sat r, atbot r2] ()
    in x + y
    end
fun calls () =
    let with r r2
        val x = g `atbot r ()
        val y = f `attop r ()
        val (a,b) = pair `[attop r, atbot r2] ()
        val z = mix `attop r ()
    in if x + y + a + b + z > 30.0 then print "C" else print "FAIL"
    end
fun branches flag =
    let with r
        val s = if flag then "D"`attop r else "E"`attop r
    in print s
    end
val _ = (direct (); calls (); branches true)
SML
cat > live.sml <<'SML'
infix +
fun f () =
    let with r
        val x = 5.4`r
        val y = 6.4`atbot r
    in x + y
    end
SML
cat > local_sat.sml <<'SML'
infix +
fun f () = let with r in 5.4`sat r + 1.0 end
SML
cat > formal_atbot.sml <<'SML'
fun f `r () : real = 5.4`atbot r
SML
cat > live_sat.sml <<'SML'
infix +
fun f `r () : real =
    let val x = 5.4`r
        val y = 6.4`sat r
    in x + y
    end
SML
cat > call_live.sml <<'SML'
infix +
fun f `r () : real = 5.4`r
fun g () =
    let with r
        val x = 5.4`r
        val y = f `atbot r ()
    in x + y
    end
SML
cat > call_local_sat.sml <<'SML'
infix +
fun f `r () : real = 5.4`r
fun g () = let with r in f `sat r () + 1.0 end
SML
cat > call_formal_atbot.sml <<'SML'
fun f `r () : real = 5.4`r
fun g `r () : real = f `atbot r ()
SML
cat > override.sml <<'SML'
infix +
fun f () = let with r in 5.4`attop r + 1.0 end
SML
# A bracketed actual-region list may retain the existing whitespace syntax.
cat > old.sml <<'SML'
infix +
fun f `[r1 r2] () : real * real = (5.4`r1,6.4`r2)
fun g () = let with r1 r2 val (x,y) = f `[r1 r2] () in x + y end
SML
for source in *.sml; do printf '%s\n' "$source" > "${source%.sml}.mlb"; done
for optimise in -no_opt -opt; do
  "$REML" -no_basislib "$optimise" -o good good.mlb > good.log 2>&1
  ./good > actual
  printf ABCD > expected
  cmp expected actual
  "$REML" -no_basislib "$optimise" -c old.mlb > old.log 2>&1
  for sample in live local_sat formal_atbot live_sat call_live call_local_sat call_formal_atbot; do
    if "$REML" -no_basislib "$optimise" -c "$sample.mlb" > "$sample.log" 2>&1; then
      echo "Incorrectly accepted $sample ($optimise)" >&2; exit 1
    fi
    grep -q 'disagrees with inferred storage mode' "$sample.log"
    grep -Eq "$sample.sml, line [0-9]+, column [0-9]+" "$sample.log"
    if grep -Eq 'Impossible:|uncaught exception' "$sample.log"; then exit 1; fi
  done
done
# Verify that an explicit attop survives even when the analysis would reset.
"$REML" -no_basislib -no_opt -print_regions -Psme -c override.mlb > override.log 2>&1
grep -q 'attop' override.log
if "$REML" -no_basislib -no_opt -disable_atbot_analysis -c live.mlb > disabled.log 2>&1; then
  echo 'atbot overrode disabled storage mode analysis' >&2; exit 1
fi
grep -q 'disagrees with inferred storage mode attop' disabled.log
printf 'val x = 5.4`attop r\n' > standard.sml
printf 'standard.sml\n' > standard.mlb
if "$MLKIT" -no_basislib -c standard.mlb > standard.log 2>&1; then
  echo 'Standard ML accepted ReML storage modes' >&2; exit 1
fi
printf 'val atbot = 1 val sat = 2 val attop = 3\n' > names.sml
printf 'names.sml\n' > names.mlb
"$MLKIT" -no_basislib -c names.mlb > names.log 2>&1
printf 'Storage mode annotation checks passed\n'
