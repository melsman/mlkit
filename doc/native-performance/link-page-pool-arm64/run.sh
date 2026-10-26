#!/bin/sh
set -eu
out=/private/tmp/mlkit-link-page-pool
bench=/var/folders/yz/hm8jnsk51kbfr00mqpd5ml_w0000gn/T/mlkit-mac-par.aCbt0s/par/benchmarks
timer=/var/folders/yz/hm8jnsk51kbfr00mqpd5ml_w0000gn/T/mlkit-mac-par.aCbt0s/timer
printf 'program,round,variant,user_seconds,sys_seconds,wall_seconds,max_rss_kib\n' > "$out/samples.csv"
cd "$bench"
for name in barnes-hut fft fib37 kbc lexgen life logic mandelbrot mlyacc mpuz msort nucleic professor ratio ray simple tak tsp vliw zebra; do
 for round in 1 2 3 4 5; do
  case $round in 1|3|5) order='context nospawn';; *) order='nospawn context';; esac
  for variant in $order; do
   "$timer" "$out/$name-$variant-$round.out" "$out/$name-$variant.exe" > "$out/$name-$variant-$round.time" 2> "$out/$name-$variant-$round.err"
   cmp "$name.expected" "$out/$name-$variant-$round.out"
   printf '%s,%s,%s,%s\n' "$name" "$round" "$variant" "$(cat "$out/$name-$variant-$round.time")" >> "$out/samples.csv"
  done
 done
 echo "$name: five pairs passed"
done
