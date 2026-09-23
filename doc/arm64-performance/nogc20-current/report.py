from pathlib import Path
import json,math,statistics
r=Path('/private/tmp/mlkit-nogc20-current');d=json.loads((r/'measurements.json').read_text());rows=d['results'];assert len(rows)==20
ratios={k:[x['medians']['arm64'][k]/x['medians']['x64'][k] for x in rows] for k in ['seconds','peak_rss_bytes']}
gm={k:math.exp(statistics.mean(math.log(v) for v in vv)) for k,vv in ratios.items()}
wins={k:sum(v<1 for v in vv) for k,vv in ratios.items()}
times=['| Benchmark | X64 / Rosetta 2 (s) | ARM64 (s) | ARM / X64 |','|---|---:|---:|---:|']
mem=['| Benchmark | X64 / Rosetta 2 (MiB) | ARM64 (MiB) | ARM / X64 |','|---|---:|---:|---:|']
for x in rows:
 a=x['medians']['arm64'];b=x['medians']['x64'];name=x['benchmark']
 times.append(f"| {name} | {b['seconds']:.4f} | {a['seconds']:.4f} | {a['seconds']/b['seconds']:.2f} |")
 mem.append(f"| {name} | {b['peak_rss_bytes']/2**20:.1f} | {a['peak_rss_bytes']/2**20:.1f} | {a['peak_rss_bytes']/b['peak_rss_bytes']:.2f} |")
summary=f"ARM64 is faster on {wins['seconds']}/20 benchmarks and uses less peak RSS on {wins['peak_rss_bytes']}/20. Geometric mean ARM64/X64 ratios: {gm['seconds']:.3f} for time and {gm['peak_rss_bytes']:.3f} for peak RSS."
body=f'''# ARM64 vs X64: 20 benchmarks with `-no_gc`

Measured September 23, 2026 on Apple M2 Max (32 GiB RAM), macOS 26.5.1.
Source revision: `{d['metadata']['revision']}`. ARM64 programs run natively;
X64 programs run through Rosetta 2 on the same machine.

Both target compilers are rebuilt with MLKit `-gc`, using the established
1 GiB host stack. Compiler execution is outside the measurements. All
benchmark programs and both complete Basis libraries are compiled with
**`-no_gc`**, into initially absent `NoGC20Currentarm64` and
`NoGC20Currentx64` caches. No MLton is used.

Both non-GC runtimes (`runtimeSystem.a`) are rebuilt from source. The
checkout's original Darwin X64 configuration is restored afterward.
Executable architectures are checked, and neither `_gc` nor `_time_to_gc`
is present in the benchmark symbol tables. Both targets use a 256 MiB stack
reservation; memory measurements report resident pages, not that reservation.

This is the normal non-GC compiler configuration: region inference and
region-based allocation/reclamation remain enabled, and values are untagged.
It is not a GC executable run with `-disable_gc`. Because `-no_gc` also changes
representation and compiler settings, a comparison with GC results would not
isolate collector overhead alone.

The workload is the same 20 benchmarks and original input sizes as the
[GC comparison](arm64-gc20-current.md) and
[earlier milestone-11 report](arm64-runtime-performance.md): ten from
`debs-icfp24` at `03d473d` and ten from `mlkit-bench` at `5bff5c9`.

Each executable receives one warmup and five measured executions. Backend
order is shuffled deterministically. All compilation finishes before
measurement. Every one of the 240 executions exits successfully and matches
the benchmark's expected stdout byte for byte. Tables report medians of the
five samples. Timing includes process startup and benchmark I/O.

{summary}

A ratio below 1 favors ARM64. Small differences should not be interpreted as
precise architectural effects; this compares native ARM64 with X64 under
Rosetta 2, including their different runtime and code footprints.

## Execution time

'''+ '\n'.join(times)+'''

## Peak resident memory

DLX RSS varies appreciably across runs: X64 ranges from 164.7 to 226.7 MiB,
and ARM64 from 68.8 to 89.5 MiB. The table reports their medians.

Memory is each process's maximum resident set size from macOS
`wait4`/`getrusage` (`ru_maxrss` in bytes), divided by 1,048,576 to obtain MiB.
The table takes the median of five per-run maxima. It includes resident code,
stack, heap, and process-associated Rosetta memory. It does not measure total
allocation, virtual address reservation, or only the ML heap.

'''+ '\n'.join(mem)+'''

[Raw samples, build manifest, hashes and scripts](arm64-performance/nogc20-current/)
are retained. `prepare.py` builds the host compilers and runtimes;
`run.py build` creates isolated source copies and fresh Basis caches;
`run.py measure` checks output and captures time and memory for each child
individually. Local paths require adaptation on another machine.
'''
(r/'report.md').write_text(body);(r/'tables.md').write_text(summary+'\n\n'+'\n'.join(times)+'\n\n'+'\n'.join(mem)+'\n');print((r/'tables.md').read_text())
