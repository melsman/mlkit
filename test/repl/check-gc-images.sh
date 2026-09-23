#!/bin/sh
# Run with a freshly compiled Basis cache; GC changes affect generated objects.
set -eu
: "${MLKIT:?Set MLKIT to the compiler executable}"
: "${SML_LIB:?Set SML_LIB to the configured source tree}"
: "${CC:=cc}"
: "${GC_FLAGS:=-gc}"
gc_sources=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
gc_work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-repl-gc.XXXXXX")
echo "REPL GC outputs: $gc_work"
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$gc_work/output.log" "$gc_work/error.log"; do
    [ ! -f "$log" ] || cat "$log" >&2
  done
fi; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
# CC may contain target-selection options, e.g. 'gcc -arch x86_64'.
$CC -Wall -Wextra -Werror -fPIC -c "$gc_sources/gc-images.c" -o "$gc_work/probe.o"
cat > "$gc_work/link-shared" <<'LINK'
#!/bin/sh
set -eu
for arg do
  case "$arg" in */libruntime.so) exec $CC "$@" "$REPL_GC_PROBE" ;; esac
done
exec $CC "$@"
LINK
chmod +x "$gc_work/link-shared"
REPL_GC_PROBE=$gc_work/probe.o
export CC REPL_GC_PROBE
cd "$gc_work"
# Optional shared cache for already rebuilt Basis units; local phrases remain fresh.
gc_cache=${GC_CACHE:-$(basename "$gc_work" | tr -cd '[:alnum:]')}
"$MLKIT" $GC_FLAGS --mlb-subdir "$gc_cache" -ldshared "$gc_work/link-shared" \
  < "$gc_sources/gc-images.cmd" > output.log 2> error.log
if grep -Ei 'Garbage collection disabled|uncaught|error:|lost earlier|lost updated|lost exception|lost printed' output.log; then
  exit 1
fi
grep -q 'REPL GC images: OK' output.log
printf 'REPL collections across images passed (%s)\n' "$GC_FLAGS"
