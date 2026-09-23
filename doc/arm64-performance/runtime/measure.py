#!/usr/bin/env python3
"""Compare 20 published MLKit benchmarks; uses only MLKit compilers."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import time
import threading


def main():
    p = argparse.ArgumentParser(description=__doc__)
    for arg in ('source', 'bench', 'bitstealing', 'arm64', 'x64', 'output'):
        p.add_argument('--' + arg, required=True, type=Path)
    p.add_argument('--repetitions', type=int, default=5)
    p.add_argument('--reuse-binaries', action='store_true',
                   help='resume only with unchanged sources, compilers, and flags')
    args = p.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, SML_LIB=str(args.source.absolute()))
    benchmarks = []
    for name, src in [('calc', 'calc.mlb'), ('DLX', 'dlx.mlb'),
                      ('kbc', 'kbc.sml'), ('lexgen', 'lexgen.sml'),
                      ('logic', 'logic.mlb'), ('nucleic', 'nucleic.mlb'),
                      ('patricia', 'patricia.sml'), ('ray', 'ray.mlb'),
                      ('uf', 'uf.mlb'), ('vliw', 'vliw.sml')]:
        benchmarks.append((name, args.bitstealing.resolve() / 'src' / name.lower(), src))
    for name in ('mlyacc', 'professor', 'barnes-hut', 'fft', 'life',
                 'mandelbrot', 'mpuz', 'msort', 'simple', 'zebra'):
        suffix = '.mlb' if name in ('mlyacc', 'barnes-hut') else '.sml'
        benchmarks.append((name, args.bench.resolve() / 'benchmarks', name + suffix))

    def command(cmd, cwd=None):
        return subprocess.check_output(cmd, cwd=cwd, text=True).strip()

    metadata = {
        'platform': platform.platform(),
        'compiler_revision': command(['git', 'rev-parse', 'HEAD'], args.source),
        'bench_revision': command(['git', 'rev-parse', 'HEAD'], args.bench),
        'bitstealing_revision': command(['git', 'rev-parse', 'HEAD'], args.bitstealing),
        'compiler_sha256': {arch: hashlib.sha256(path.read_bytes()).hexdigest()
                            for arch, path in [('arm64', args.arm64), ('x64', args.x64)]},
        'runtime_sha256': {str(path.relative_to(args.source)): hashlib.sha256(path.read_bytes()).hexdigest()
                           for path in (args.source / 'lib' / 'runtimeSystem.a',
                                        args.source / 'lib' / 'runtimeSystemGC.a',
                                        args.source / 'lib' / 'darwin-arm64' / 'runtimeSystem.a',
                                        args.source / 'lib' / 'darwin-arm64' / 'runtimeSystemGC.a')},
        'repetitions': args.repetitions, 'warmups': 1,
        'metric': 'elapsed seconds, perf_counter, separate process per sample',
    }
    results = []
    for name, cwd, src in benchmarks:
        print('Building', name, flush=True)
        expected_path = cwd / (src + '.out.ok')
        if not expected_path.exists():
            expected_path = cwd / (src.lower() + '.out.ok')
        expected = expected_path.read_bytes()
        configs = []
        for mode in ('gc', 'no_gc'):
            for arch, compiler in [('x64', args.x64), ('arm64', args.arm64)]:
                key = name + '-' + arch + '-' + mode
                exe = out / (key + '.exe')
                cmd = [str(compiler.resolve()), '--mlb-subdir', 'M11' + arch,
                       '-' + mode, '-o', str(exe)]
                if arch == 'x64':
                    cmd += ['-as', 'as -arch x86_64 -q', '-ldexe',
                            'gcc -arch x86_64 -Wl,-stack_size,0x10000000']
                cmd.append(src)
                # Completed binaries can be reused when resuming the same run.
                if not args.reuse_binaries or not exe.exists():
                    with (out / (key + '.compile.log')).open('w') as log:
                        subprocess.run(cmd, cwd=cwd, env=env, stdout=log,
                                       stderr=subprocess.STDOUT, check=True, timeout=600)
                actual_arch = command(['lipo', '-archs', str(exe)])
                assert actual_arch == ('x86_64' if arch == 'x64' else 'arm64'), actual_arch
                configs.append({'name': name, 'arch': arch, 'mode': mode,
                                'source': str(cwd / src), 'compile_command': cmd,
                                'samples': [], 'exe': str(exe)})
        # One untimed warm-up, then alternate configuration order each round.
        for rep in range(args.repetitions + 1):
            for row in (configs if rep % 2 == 0 else list(reversed(configs))):
                key = name + '-' + row['arch'] + '-' + row['mode']
                with (out / (key + '.stdout')).open('wb') as stdout, \
                     (out / (key + '.stderr')).open('wb') as stderr:
                    start = time.perf_counter()
                    completed = subprocess.Popen([row['exe']], cwd=cwd, stdout=stdout,
                                                 stderr=stderr)
                    watchdog = threading.Timer(180, completed.kill)
                    watchdog.start()
                    try:
                        completed.wait()
                        elapsed = time.perf_counter() - start
                    finally:
                        watchdog.cancel()
                if completed.returncode or (out / (key + '.stdout')).read_bytes() != expected:
                    raise RuntimeError(key + ': execution/output check failed')
                if rep:
                    row['samples'].append(elapsed)
        for row in configs:
            row['median'] = statistics.median(row['samples'])
            row['min'] = min(row['samples'])
            row['max'] = max(row['samples'])
            row['output_sha256'] = hashlib.sha256(expected).hexdigest()
            print(name, row['arch'], row['mode'], round(row['median'], 4), flush=True)
            results.append(row)
        (out / 'measurements.json').write_text(json.dumps(
            {'metadata': metadata, 'results': results}, indent=2) + '\n')
    print('All 20 benchmarks passed in all four configurations.', flush=True)


if __name__ == '__main__':
    main()
