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
datatype box = Box of string | Other of word
fun records () =
    let with rs rr rc
        val a = "E"`atbot rs
        val r = {text=a,flag=true}`atbot rr
        val b = Box`atbot rc (#text r)
    in case b of Box s => print s | Other _ => print "FAIL"
    end
val _ = (direct (); calls (); branches true; records ())
SML
cat > live.sml <<'SML'
infix +
fun f () =
    let with r
        val x = 5.4`r
        val y = 6.4`atbot r
    in prim("storage_mode_consume",(x,y)) : unit
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
fun f `r () : unit =
    let val x = 5.4`r
        val y = 6.4`sat r
    in prim("storage_mode_consume",(x,y)) : unit
    end
SML
cat > call_live.sml <<'SML'
infix +
fun f `r () : real = 5.4`r
fun g () =
    let with r
        val x = 5.4`r
        val y = f `atbot r ()
    in prim("storage_mode_consume",(x,y)) : unit
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
cat > alias.sml <<'SML'
fun f `[r1 r2] () : real * real = (5.4`r1,6.4`r2)
fun g () =
    let with r
        val (x,y) = f `[atbot r, attop r] ()
    in prim("storage_mode_consume",(x,y)) : unit
    end
SML
cat > external.sml <<'SML'
fun external `r () : real = 5.4`sat r
fun externalPair `[r1 r2] () : real * real = (5.4`r1,6.4`r2)
SML
cat > external_call.sml <<'SML'
infix +
fun caller () = let with r in external `attop r () + 1.0 end
SML
cat > external_bad.sml <<'SML'
infix +
fun caller () = let with r in external `sat r () + 1.0 end
SML
cat > external_alias.sml <<'SML'
fun caller () =
    let with r
        val (x,y) = externalPair `[atbot r, attop r] ()
    in prim("storage_mode_consume",(x,y)) : unit
    end
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
printf 'external.sml\nexternal_call.sml\n' > external_call.mlb
printf 'external.sml\nexternal_bad.sml\n' > external_bad.mlb
printf 'external.sml\nexternal_alias.sml\n' > external_alias.mlb
for optimise in -no_opt -opt; do
  "$REML" -no_basislib "$optimise" -o good good.mlb > good.log 2>&1
  ./good > actual
  printf ABCDE > expected
  cmp expected actual
  "$REML" -no_basislib "$optimise" -c old.mlb > old.log 2>&1
  "$REML" -no_basislib "$optimise" -c external_call.mlb > external_call.log 2>&1
  for sample in live local_sat formal_atbot live_sat call_live call_local_sat call_formal_atbot external_bad external_alias; do
    if "$REML" -no_basislib "$optimise" -c "$sample.mlb" > "$sample.log" 2>&1; then
      echo "Incorrectly accepted $sample ($optimise)" >&2; exit 1
    fi
    grep -q 'disagrees with inferred storage mode' "$sample.log"
    grep -Eq "$sample.sml, line [0-9]+, column [0-9]+" "$sample.log"
    if grep -Eq 'Impossible:|uncaught exception' "$sample.log"; then exit 1; fi
  done
done
# Aliased actuals retain separate modes, in the quantified parameter order.
"$REML" -no_basislib -no_opt -log_to_file -print_regions -no_print_control_abbrev_layout -Psme -c alias.mlb > alias.log 2>&1
grep -Eq 'f.*atbot.*attop' alias.sml.log
# Verify that an explicit attop survives even when the analysis would reset.
"$REML" -no_basislib -no_opt -log_to_file -print_regions -no_print_control_abbrev_layout -Psme -c override.mlb > override.log 2>&1
grep -Eq '5\.4.*attop' override.sml.log
if "$REML" -no_basislib -no_opt -disable_atbot_analysis -c live.mlb > disabled.log 2>&1; then
  echo 'atbot overrode disabled storage mode analysis' >&2; exit 1
fi
grep -q 'disagrees with inferred storage mode attop' disabled.log
printf 'val x = 5.4`attop r\n' > standard.sml
printf 'standard.sml\n' > standard.mlb
if "$MLKIT" -no_basislib -c standard.mlb > standard.log 2>&1; then
  echo 'Standard ML accepted ReML storage modes' >&2; exit 1
fi
cat > names.sml <<'SML'
infix +
infix 4 >
fun print (s:string) : unit = prim("printStringML",s)
type atbot = int
structure Constructors = struct datatype sat = attop of atbot end
structure atbot = struct val sat = 7 end
fun sat (Constructors.attop atbot) = atbot
fun example () =
    let val atbot = 34
        val sat = sat (Constructors.attop atbot)
        val attop = {atbot=sat, sat=atbot.sat, attop=1}
    in #atbot attop + #sat attop + #attop attop
    end
val _ = if example () > 41 then
            (if 43 > example () then print "OK" else print "FAIL")
        else print "FAIL"
SML
printf 'names.sml\n' > names.mlb
for compiler in "$MLKIT" "$REML"; do
  "$compiler" --mlb-subdir "StorageNames$(basename "$compiler")" -no_basislib -o names names.mlb > names.log 2>&1
  ./names > actual
  printf OK > expected
  cmp expected actual
done
# The same words can name explicit regions and appear in region types.
cat > region_names.sml <<'SML'
fun print (s:string) : unit = prim("printStringML",s)
fun f `atbot () : string`atbot = "OK"`sat atbot
fun g () = let with sat in print (f `atbot sat ()) end
fun triple `[atbot sat attop] () : string * string * string =
    ("A"`atbot,"B"`sat,"C"`attop)
fun bare () =
    let with atbot sat attop
        val (a,b,c) = triple `[atbot,sat,attop] ()
    in print a; print b; print c
    end
val _ = (g (); bare ())
SML
printf 'region_names.sml\n' > region_names.mlb
"$REML" -no_basislib -o region_names region_names.mlb > region_names.log 2>&1
./region_names > actual
printf OKABC > expected
cmp expected actual
printf 'Storage mode annotation checks passed\n'
