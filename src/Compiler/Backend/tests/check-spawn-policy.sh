#!/bin/sh
# Link-object policy and cache reuse; correctness only, no timing collection.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
: "${REML:?Set REML}"
fixtures=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-spawn-policy.XXXXXX")
echo "Spawn policy outputs: $scratch"
cd "$scratch"
cat > spawner.sml <<'SML'
fun retained_spawn__noinline (f:unit->unit) : foreignptr =
  prim("spawnone",(prim("pointer",f):foreignptr))
val _:unit = prim("mlkit_spawn_unit_keep",())
SML
cat > shared.sml <<'SML'
fun verify_policy (e:word) : unit =
  prim("mlkit_spawn_policy",((prim("__get_ctx",()):word),e))
SML
printf 'val () = verify_policy 0w1\n' > spawned.sml
printf 'val () = verify_policy 0w0\n' > sequential.sml
printf 'spawner.sml\nshared.sml\nspawned.sml\n' > spawned.mlb
printf 'shared.sml\nsequential.sml\n' > sequential.mlb
for compiler in "$MLKIT" "$REML"; do
  for mode in normal forced runtime profile separated; do
    [ "$mode" != separated ] || [ "$compiler" = "$REML" ] || continue
    flags=
    defines=
    case "$mode" in
      forced) flags=--alloc_protect_always;;
      runtime) flags=--no_inline_allocation;;
      profile) flags=-rp; defines=-DPROFILING;;
    esac
    ${CC:-cc} -O2 -Wall -Wextra -Werror -DPARALLEL $defines \
      -iquote "$SML_LIB/src/Runtime" -c "$fixtures/spawn-policy.c" -o policy.o
    # -no_par0 is ReML-only. MLKit remains in its normal protected mode.
    protection=-no_gc
    [ "$compiler" != "$REML" ] || protection=-no_par0
    [ "$mode" != separated ] || protection=""
    for program in spawned spawned sequential sequential; do
      "$compiler" $protection -par $flags --no_basislib --no_delete_target_files \
        --mlb-subdir SpawnPolicy -ldexe "${CC:-cc} policy.o" \
        --assembler "${CC:-cc} -c" -o program "$program.mlb" > "$program-$mode.log" 2>&1
      ./program > actual
    done
    for program in spawned sequential; do
      if grep -Eq 'reading source file:.*(spawner|shared)\.sml' "$program-$mode.log"; then
        echo 'Expected true and false link metadata to be reused from cache.' >&2; exit 1
      fi
    done
  done
done
# Exercise the normal prepared Basis variants too, without requesting any new
# cache suffix or a ReML -no_par0 library preparation.
${CC:-cc} -O2 -DPARALLEL -iquote "$SML_LIB/src/Runtime" \
  -c "$fixtures/spawn-policy.c" -o policy.o
printf '$(SML_LIB)/basis/basis.mlb\nshared.sml\nsequential.sml\n' > basis-client.mlb
for compiler in "$MLKIT" "$REML"; do
  flags=-no_gc
  [ "$compiler" != "$REML" ] || flags=""
  "$compiler" $flags -par -ldexe "${CC:-cc} policy.o" --assembler "${CC:-cc} -c" \
    -o program basis-client.mlb > basis-client.log 2>&1
  ./program > actual
done
if [ "${SPAWN_POLICY_REPL:-1}" = 1 ]; then
  ${CC:-cc} -O2 -fPIC -DPARALLEL -iquote "$SML_LIB/src/Runtime" \
    -c "$fixtures/spawn-policy.c" -o policy.o
  case $(uname -s) in
    Darwin) ${CC:-cc} -dynamiclib -undefined dynamic_lookup policy.o -o "$scratch/libspawnpolicy.dylib"
            shared="${CC:-cc} -L$scratch -lspawnpolicy -Wl,-rpath,$scratch";;
    *) ${CC:-cc} -shared policy.o -o "$scratch/libspawnpolicy.so"
       shared="${CC:-cc} -Wl,--no-as-needed -L$scratch -lspawnpolicy -Wl,-rpath,$scratch";;
  esac
  cat > repl.txt <<'SML'
val _:unit = prim("mlkit_spawn_policy",((prim("__get_ctx",()):word),0w1));
fun spawn_later (f:unit->unit) : unit =
  let val rf = ref f
      val t:foreignptr = prim("spawnone",(prim("pointer",(prim("!",rf):unit->unit)):foreignptr))
      val v:unit = prim("thread_get",t)
  in if prim("!",ref true) then v else (prim("!",rf):unit->unit)()
  end;
val () = spawn_later (fn () => ());
val _:unit = prim("mlkit_spawn_policy",((prim("__get_ctx",()):word),0w1));
val _:unit = prim("printStringML","REPL POLICY OK\n");
:quit;
SML
  # Keep the closure live through joining, as basis/Thread.sml does: the raw
  # foreignptr type hides its region dependencies from inference.
  for compiler in "$MLKIT" "$REML"; do
    protection=-no_gc
    [ "$compiler" != "$REML" ] || protection=-no_par0
    env LD_LIBRARY_PATH="$scratch${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      DYLD_LIBRARY_PATH="$scratch${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" \
      "$compiler" $protection -par --no_basislib --mlb-subdir SpawnPolicyRepl \
      --libs m,c,dl,spawnpolicy --libdirs "$scratch" --assembler "${CC:-cc} -c" \
      --link_shared "$shared" -ldexe "${CC:-cc} -Wl,-rpath,$scratch" < repl.txt > repl.log 2>&1
    grep -q 'REPL POLICY OK' repl.log
    if grep -Eq 'Compile error|unbound|Assertion|assertion' repl.log; then cat repl.log; exit 1; fi
  done
fi
echo 'Standalone spawn policy, cached true/false metadata, globals and runtime fallback passed.'
