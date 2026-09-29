#!/usr/bin/env python3
"""Differential and malformed-input tests for the compiled SML stream reader."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
viewer = os.environ.get('RPVIEW',str(root/'bin/rpview'))
spec = importlib.util.spec_from_file_location('reference',Path(__file__).with_name('reference-reader.py'))
reference = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reference)
with tempfile.TemporaryDirectory(prefix='rp-sml-reader-') as d:
    profile = Path(d)/'profile.rp'
    output = Path(d)/'profile.html'
    def run(text, ok=True):
        profile.write_text(text)
        p = subprocess.run([viewer,str(profile),'--output',str(output)],text=True,capture_output=True,timeout=10,
                           env=dict(os.environ,PATH='/nonexistent'))
        assert (p.returncode==0)==ok,(p.returncode,p.stdout,p.stderr)
        return json.loads(output.read_text().split('const samples=')[1].split(';\n')[0]) if ok else None
    def compare(text):
        actual=run(text)
        expected=list(reference.read_samples(text.splitlines(keepends=True)))
        def exact(v):
            if type(v) is int: return str(v)
            if isinstance(v,list): return [exact(x) for x in v]
            if isinstance(v,dict): return {k:exact(x) for k,x in v.items()}
            return v
        assert [{k:v for k,v in s.items() if k!='marks'} for s in actual]==exact(expected)
    for path in sys.argv[1:]:
        text=Path(path).read_text()
        compare(text)
        compare(text+'{"type":')
        run(text+'invalid\n',False)
    # Begin from an exact, small complete stream, with unusual names/markers.
    rows=[json.loads(l) for l in Path(sys.argv[1]).read_text().splitlines()]
    for r in rows:
        if r['type']=='region':
            r['name']='\"\\\n\0é λ 😀 </script> __DATA__ __META__ __LIVE__'
            r['large_bytes']=2**64-1
    text=''.join(json.dumps(r)+'\n' for r in rows)
    compare(text)
    run(text.replace(str(2**64-1),str(2**64)),False)
    for version in (1,2):
        old=[dict(r,version=version) if r['type']=='header' else r for r in rows if r['type']!='stack']
        compare(''.join(json.dumps(r)+'\n' for r in old))
    for bad in ['{"type":"header","type":"header"}\n',
                '{"type":"header","format":"mlkit-region-profile","version":03,"page_bytes":8192}\n',
                '{"type":"header","format":"\\uD800"}\n','[true,]\n']:
        run(bad,False)
    header=dict(type='header',format='mlkit-region-profile',version=3,page_bytes=8192,
                main_source='/tmp/__DATA__ __META__ </script>.sml',gc_enabled=True)
    summary=dict(type='session_end',gc_collections=2**60+3)
    assert run(json.dumps(header)+'\n'+json.dumps(summary)+'\n')==[]
    metadata=json.loads(output.read_text().split('const profile=')[1].split(';\n')[0])
    assert metadata==dict(main_source=header['main_source'],gc_enabled=True,gc_collections=str(2**60+3),complete=True)
    assert run(json.dumps(header)+'\n')==[]
    metadata=json.loads(output.read_text().split('const profile=')[1].split(';\n')[0])
    assert metadata['gc_collections'] is None and metadata['complete'] is False
    run(json.dumps(dict(header,gc_enabled='yes'))+'\n',False)
    run(json.dumps(header)+'\n'+json.dumps(dict(summary,gc_collections=-1))+'\n',False)
print('SML reader: exact uint64, Unicode/escaping, v1/v2/v3, truncation, and malformed input passed')
