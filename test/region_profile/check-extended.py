#!/usr/bin/env python3
"""Assertions for generated periodic, parallel, GC and REPL profile streams."""
import importlib.util
import json
from pathlib import Path
import sys
spec = importlib.util.spec_from_file_location("reader", Path(__file__).resolve().parents[2]/"src/Tools/RegionProfile/rp-read.py")
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)
mode, filename = sys.argv[1:]
records = [json.loads(line) for line in open(filename)]
with open(filename) as stream:
    samples = list(reader.read_samples(stream))
assert samples
assert [s['sample'] for s in samples] == list(range(1,len(samples)+1))
assert all(a['end_time'] <= b['time'] for a,b in zip(samples,samples[1:]))
if mode == 'periodic':
    assert len(samples) >= 2
    assert all(s['reason'] == 'periodic' for s in samples)
elif mode in ('parallel','argobots'):
    assert sum(s['reason'] == 'explicit' for s in samples) == 61
    starts = [r['thread'] for r in records if r['type'] == 'thread_start']
    ends = [r['thread'] for r in records if r['type'] == 'thread_end']
    assert sorted(starts) == list(range(13))
    assert set(range(1,13)) <= set(ends)
    assert any(len({r['thread'] for r in s['regions']}) > 2 for s in samples)
    if mode == 'argobots':
        assert all(r['worker'] in (0,1,-1) for s in samples for r in s['regions'])
        assert any(r['worker'] >= 0 for s in samples for r in s['regions'])
elif mode in ('gc','gengc'):
    reasons = [s['reason'] for s in samples if s['reason'].endswith('_gc')]
    assert len(reasons) >= 2 and reasons == ['before_gc','after_gc']*(len(reasons)//2)
    collections = [s for s in samples if s['reason'].endswith('_gc')]
    assert all(a['gc_kind'] == b['gc_kind'] for a,b in zip(collections[::2],collections[1::2]))
    assert any(r['large_bytes'] >= 80000 for s in samples for r in s['regions'])
    if mode == 'gengc':
        assert any(r['g1_pages'] and r['g1_unused_tail'] for s in samples for r in s['regions'])
else:
    assert mode == 'repl'
    assert sum(s['reason'] == 'explicit' for s in samples) == 2
    assert all(any(r['unit'] == '<global>' for r in s['regions']) for s in samples)
print(mode+': stream, timing, and accounting checks passed')
