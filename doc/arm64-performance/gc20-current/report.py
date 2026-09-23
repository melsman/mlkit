from pathlib import Path
import json,math,statistics
r=Path('/private/tmp/mlkit-gc20-current');d=json.loads((r/'measurements.json').read_text());rows=d['results'];assert len(rows)==20
ratios={k:[x['medians']['arm64'][k]/x['medians']['x64'][k] for x in rows] for k in ['seconds','peak_rss_bytes']}
gm={k:math.exp(statistics.mean(math.log(v) for v in vv)) for k,vv in ratios.items()}
wins={k:sum(v<1 for v in vv) for k,vv in ratios.items()}
times=['| Benchmark | X64 / Rosetta 2 (s) | ARM64 (s) | ARM / X64 |','|---|---:|---:|---:|']
mem=['| Benchmark | X64 / Rosetta 2 (MiB) | ARM64 (MiB) | ARM / X64 |','|---|---:|---:|---:|']
for x in rows:
 m=x['medians'];a=m['arm64'];b=m['x64'];name=x['benchmark']
 times.append(f"| {name} | {b['seconds']:.4f} | {a['seconds']:.4f} | {a['seconds']/b['seconds']:.2f} |")
 mem.append(f"| {name} | {b['peak_rss_bytes']/2**20:.1f} | {a['peak_rss_bytes']/2**20:.1f} | {a['peak_rss_bytes']/b['peak_rss_bytes']:.2f} |")
summary=f"ARM64 is faster on {wins['seconds']}/20 benchmarks and uses less peak RSS on {wins['peak_rss_bytes']}/20. Geometric mean ARM64/X64 ratios: {gm['seconds']:.3f} for time and {gm['peak_rss_bytes']:.3f} for peak RSS."
body='''# Current ARM64 vs X64: 20 GC benchmarks

Measured September 23, 2026 on the same Apple M2 Max (32 GiB RAM), macOS
26.5.1. Compiler source revision: `36f68fd`. Both target compilers are built
with MLKit `-gc`, and every benchmark is compiled with `-gc`. The compiler
executables themselves are X64-hosted, with the established 1 GiB host stack;
compiler execution is outside the measurements. No MLton is used.

Both GC runtimes are rebuilt from the current source. The complete Basis and
all benchmark programs are compiled into initially absent, separate
`GC20Currentarm64` and `GC20Currentx64` caches. X64 programs run through
Rosetta 2, ARM64 programs natively. The source checkout is restored to its
original Darwin X64 configuration after building the runtime archives.

The 20 programs and original input sizes are the same as the
[earlier milestone-11 comparison](arm64-runtime-performance.md): ten from
`debs-icfp24` at `03d473d`, and ten from `mlkit-bench` at `5bff5c9`.

Each backend/benchmark gets one warmup and five measured executions, with
backend order shuffled deterministically. All compilation finishes before
measurement. Tables report medians of the five samples, not best times.
Elapsed time includes startup and benchmark I/O; all executions use
`-report_gc`. Every one of the 240 executions exits successfully and matches
expected stdout byte for byte.

Memory is the peak resident set size of each separate benchmark process,
reported by macOS `wait4`/`getrusage` as `ru_maxrss` in bytes, converted to MiB
(1 MiB = 1,048,576 bytes). We take the median of five per-run peaks. This
includes resident code, stack, heap and process-associated Rosetta memory;
it is not the ML heap size, allocation volume, virtual address reservation,
or a system-wide accounting of all translation overhead. Time uses Python's
monotonic `perf_counter`; CPU time is also retained in the raw samples.

'''+summary+'''

A ratio below 1 favours ARM64 in both tables. Small differences should not
be interpreted as precise backend effects; this compares native execution
with the complete X64/Rosetta configuration.

## Execution time

'''+ '\n'.join(times)+'''

## Peak resident memory

'''+ '\n'.join(mem)+'''

[Raw samples, build manifest, compiler/runtime/executable hashes and scripts](arm64-performance/gc20-current/)
are retained. `run.py build` creates the isolated source copies and fresh
caches; `run.py measure` checks every output and records per-child resource
usage. Paths refer to this machine's checkouts and require adaptation elsewhere.
The build and runtime logs identify the compilers and archives used. The raw
memory measurement is taken for each child individually, not the cumulative
high-water mark across previous subprocesses.
'''
(r/'report.md').write_text(body);(r/'tables.md').write_text(summary+'\n\n'+'\n'.join(times)+'\n\n'+'\n'.join(mem)+'\n');print((r/'tables.md').read_text())
