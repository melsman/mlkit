#!/usr/bin/env python3
"""Check offline escaping, exact counters, and live HTTP/control integration."""
import http.client
import json
from pathlib import Path
import re
import socket
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
viewer = root/'src/Tools/RegionProfile/rp-view.py'
with tempfile.TemporaryDirectory(prefix='rp-view-', dir='/tmp') as directory:
    base = Path(directory)
    profile = base/'test.rp'
    rows = [json.loads(line) for line in Path(sys.argv[1]).read_text().splitlines()]
    for row in rows:
        if row['type'] == 'region':
            row['name'] = '</script><script>bad()</script>'
            row['large_bytes'] = 2**60+1
    profile.write_text(''.join(json.dumps(row)+'\n' for row in rows))
    offline = base/'profile.html'
    subprocess.run([sys.executable,str(viewer),str(profile),'--output',str(offline)],check=True,capture_output=True)
    text = offline.read_text()
    assert text.count('<script>') == 1
    assert '\\u003c/script>' in text and str(2**60+1) in text
    endpoint = str(base/'control.sock')
    with socket.socket(socket.AF_UNIX,socket.SOCK_DGRAM) as receiver:
        receiver.bind(endpoint)
        receiver.settimeout(2)
        server = subprocess.Popen([sys.executable,str(viewer),str(profile),'--serve','--control',endpoint],
                                  stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
        try:
            url = server.stdout.readline().strip()
            port = int(url.split(':')[-1].rstrip('/'))
            client = http.client.HTTPConnection('127.0.0.1',port,timeout=3)
            def request(method,path,body=None,headers=None):
                client.request(method,path,body=body,headers=headers or {})
                response = client.getresponse()
                return response.status,response.read().decode()
            status, page = request('GET','/')
            assert status == 200
            token = re.search(r'token="([0-9a-f]{64})"',page)[1]
            status, body = request('GET','/samples')
            assert status == 200
            assert json.loads(body)[0]['regions'][0]['large_bytes'] == str(2**60+1)
            assert request('GET','/',headers={'Host':'evil.test'})[0] == 403
            assert request('POST','/control','sample')[0] == 403
            assert request('POST','/control','sample',{'X-Profile-Token':token})[0] == 202
            assert receiver.recv(32) == b'sample'
            assert request('POST','/control','unknown',{'X-Profile-Token':token})[0] == 400
            client.close()
        finally:
            server.terminate()
            server.communicate(timeout=5)
print('Offline escaping, exact counters, loopback HTTP, and authenticated controls passed')
