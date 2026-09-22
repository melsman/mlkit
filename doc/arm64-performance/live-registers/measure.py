"""Small GC-only comparison; use matching pre-change Basis objects in both builds."""
from pathlib import Path
import argparse
import hashlib
import json
import random
import re
import resource
import statistics
import subprocess
import time

parser = argparse.ArgumentParser()
parser.add_argument('--old', type=Path, required=True)
parser.add_argument('--new', type=Path, required=True)
parser.add_argument('--bench', type=Path, required=True)
parser.add_argument('--output', type=Path, required=True)
args = parser.parse_args()
results = []
for name, source in [('mlyacc', 'mlyacc.mlb'), ('professor', 'professor.sml')]:
    expected = (args.bench / (source + '.out.ok')).read_bytes()
    executables = {key: root / (name + '-gc.exe')
                   for key, root in [('old', args.old), ('new', args.new)]}
    samples = {key: [] for key in executables}
    rng = random.Random(223)
    for repetition in range(6):
        order = list(executables)
        rng.shuffle(order)
        for key in order:
            before = resource.getrusage(resource.RUSAGE_CHILDREN)
            start = time.perf_counter()
            run = subprocess.run([str(executables[key]), '-report_gc'],
                                 cwd=args.bench, capture_output=True, timeout=30)
            elapsed = time.perf_counter() - start
            after = resource.getrusage(resource.RUSAGE_CHILDREN)
            assert run.returncode == 0 and run.stdout == expected, (name, key, run.stderr)
            gc = re.search(r'GC\(([\d.]+)ms\): (\d+) collections', run.stderr.decode())
            assert gc, run.stderr
            if repetition:
                samples[key].append({
                    'wall': elapsed,
                    'cpu': after.ru_utime + after.ru_stime - before.ru_utime - before.ru_stime,
                    'gc_cpu': float(gc[1]) / 1000,
                    'collections': int(gc[2]),
                })
    medians = {key: {field: statistics.median(row[field] for row in rows)
                     for field in rows[0]} for key, rows in samples.items()}
    results.append({'benchmark': name, 'samples': samples, 'median': medians,
                    'executables': {key: {'path': str(path),
                        'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}
                        for key, path in executables.items()},
                    'expected_sha256': hashlib.sha256(expected).hexdigest()})
    print(name, json.dumps(medians), flush=True)
args.output.write_text(json.dumps({
    'baseline_revision': '1261087 (production code unchanged from 07cf1e8)',
    'warmups': 1, 'repetitions': 5, 'order': 'seeded shuffle per round',
    'runtime_flag': '-report_gc',
    'basis': 'Shared M12Study GC Basis built by pre-change compiler; benchmark units rebuilt',
    'results': results,
}, indent=2) + '\n')
