#!/usr/bin/env python3
"""Verify the relocated offline SML binary with no interpreter or assets."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
viewer = Path(os.environ.get('RPVIEW',root/'bin/rpview'))
with tempfile.TemporaryDirectory(prefix='rp-offline-') as directory:
    base = Path(directory)
    binary = base/'rpview'
    shutil.copy2(viewer,binary)
    profile = base/'profile.rp'
    rows = [json.loads(line) for line in Path(sys.argv[1]).read_text().splitlines()]
    for row in rows:
        if row['type']=='region':
            row['name']='</script><script>bad()</script>'
            row['large_bytes']=2**60+1
    source = ''.join(json.dumps(row)+'\n' for row in rows)
    profile.write_text(source)
    env = dict(os.environ,PATH='/nonexistent')
    subprocess.run([str(binary)],cwd=base,env=env,check=True,capture_output=True)
    html = (base/'profile.html').read_text()
    assert html.count('<script>')==1 and '\\u003c/script>' in html
    assert str(2**60+1) in html
    assert 'fetch(' not in html and '<script src=' not in html
    for name in ['profile.rp','alias.rp']:
        if name=='alias.rp': os.link(profile,base/name)
        result=subprocess.run([str(binary),'profile.rp','--output',name],cwd=base,env=env,capture_output=True)
        assert result.returncode!=0
        assert profile.read_text()==source
print('Offline relocated executable, no Python/assets/network, exact counters, escaping, and input preservation passed')
