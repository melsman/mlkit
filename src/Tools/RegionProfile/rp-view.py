#!/usr/bin/env python3
"""Generate an offline HTML profile, or serve a live view on loopback.

Counters remain decimal strings in the browser and are summed with BigInt.
Floating point is used only for chart coordinates. No third-party dependencies.
"""
import argparse
import importlib.util
import json
from pathlib import Path
import secrets
import socket
from http.server import BaseHTTPRequestHandler, HTTPServer

spec = importlib.util.spec_from_file_location("rp_read", Path(__file__).with_name("rp-read.py"))
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)


def data(path):
    with open(path, encoding="utf-8") as stream:
        lines = stream.readlines()
    samples = list(reader.read_samples(lines))
    for sample in samples:
        sample["marks"] = []
    if samples:
        import bisect
        times = [s["time"] for s in samples]
        for line in lines:
            if not line.endswith("\n"):
                break
            record = json.loads(line)
            if record["type"] == "mark":
                index = min(bisect.bisect_left(times, record["time"]),len(samples)-1)
                samples[index]["marks"].append(record)
    # JSON numbers would silently lose exact uint64 values in JavaScript.
    def exact(value):
        if type(value) is int:
            return str(value)
        if isinstance(value, dict):
            return {k: exact(v) for k, v in value.items()}
        if isinstance(value, list):
            return [exact(v) for v in value]
        return value
    return exact(samples)


TEMPLATE = r'''<!doctype html><html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width"><title>MLKit region profile</title>
<style>
body{font:16px system-ui,sans-serif;margin:32px auto;max-width:1100px;padding:0 20px;color:#182c39;background:#f7f9fa}
h1{font-size:28px}label,button,select{margin-right:12px}button,select{font:inherit;padding:7px}
svg{background:white;border:1px solid #bdcbd3;width:100%;height:280px;margin:18px 0}
input[type=range]{width:100%}table{border-collapse:collapse;width:100%;background:white}td,th{padding:9px;border-bottom:1px solid #ddd;text-align:right}td:first-child,th:first-child{text-align:left;overflow-wrap:anywhere}p{line-height:1.5}.note{color:#4d626f}#status{min-height:24px}
</style><h1>MLKit region profile</h1>
<p class="note">Allocated region footprint, including page headers and earlier-page slack; finite regions show reserved stack space. This is neither RSS nor reachable data. Thread attribution follows lifetime ownership. Worker identity is the owner's execution stream at sampling time, not the origin of allocations.</p>
<label>Group <select id="group"><option value="aggregate">Aggregate</option><option value="region">Region binding</option><option value="thread">Lifetime owner</option><option value="worker">Execution stream</option></select></label>
<label>Metric <select id="metric"><option value="total">Region footprint</option><option value="page_footprint">Page footprint</option><option value="large_bytes">Large objects</option><option value="finite_bytes">Finite reservations</option><option value="descriptor_bytes">Descriptors (separate)</option></select></label>
<div id="controls" hidden><button data-command="start">Start</button><button data-command="pause">Pause</button><button data-command="sample">Sample</button><button data-command="flush">Flush</button></div>
<p id="status" role="status"></p><svg id="chart" viewBox="0 0 1000 280" role="img" aria-label="Sampled total footprint over time"></svg>
<label>Snapshot <input id="sample" type="range" min="0" max="0" value="0"></label><p id="caption"></p><p id="marks"></p>
<table><thead><tr><th>Group</th><th>Bytes</th><th>Pages</th><th>Unused last-page tails</th></tr></thead><tbody id="rows"></tbody></table>
<script>
let samples=__DATA__; const live=__LIVE__, token=__TOKEN__, canControl=__CONTROL__;
const el=id=>document.getElementById(id), ns='http://www.w3.org/2000/svg';
function bytes(r){return el('metric').value==='total'?['page_footprint','large_bytes','finite_bytes'].reduce((a,k)=>a+BigInt(r[k]),0n):BigInt(r[el('metric').value]);}
function sum(s){return s.regions.reduce((a,r)=>a+bytes(r),0n);}
function key(r){switch(el('group').value){case 'region':return r.unit+' / '+(r.name||'binding')+' #'+r.binding;case 'thread':return r.unit==='<global>'?'Persistent/global': 'Thread '+r.thread;case 'worker':return r.worker===undefined||r.worker==='-1'?'Worker identity unavailable':'Execution stream '+r.worker;default:return 'All regions';}}
function draw(){
 const slider=el('sample');slider.max=Math.max(0,samples.length-1); const s=samples[Number(slider.value)];
 el('rows').replaceChildren();el('chart').replaceChildren();
 if(!s){el('caption').textContent='No completed snapshots yet.';return;}
 const values=samples.map(sum), peak=values.reduce((a,b)=>a>b?a:b,0n), max=peak||1n, first=BigInt(samples[0].time), last=BigInt(samples.at(-1).time), span=last-first||1n;
 const path=document.createElementNS(ns,'polyline');path.setAttribute('fill','none');path.setAttribute('stroke','#007f86');path.setAttribute('stroke-width','2');path.setAttribute('points',samples.map((v,i)=>(20+960*Number(BigInt(v.time)-first)/Number(span))+','+(255-230*Number(values[i])/Number(max))).join(' '));el('chart').append(path);
 const title=document.createElementNS(ns,'text');title.setAttribute('x','20');title.setAttribute('y','20');title.textContent='Sampled maximum: '+peak.toLocaleString()+' bytes';el('chart').append(title);
 const groups=new Map(); for(const r of s.regions){const k=key(r),v=groups.get(k)||[0n,0n,0n];v[0]+=bytes(r);v[1]+=BigInt(r.pages);v[2]+=BigInt(r.unused_tail);groups.set(k,v);}
 for(const [k,v] of [...groups].sort((a,b)=>a[1][0]===b[1][0]?0:a[1][0]>b[1][0]?-1:1)){const tr=document.createElement('tr');for(const text of [k,...v.map(n=>n.toLocaleString())]){const td=document.createElement('td');td.textContent=text;tr.append(td);}el('rows').append(tr);}
 el('marks').textContent=(s.marks||[]).map(m=>'Marker '+(Number(m.time)/1e9).toFixed(6)+' s: '+m.label).join(' · ');
 el('caption').textContent='Snapshot '+s.sample+' · '+(Number(s.time)/1e9).toFixed(6)+' s · '+s.reason+' · '+s.frames+' frames · '+s.pages_visited+' pages traversed · '+BigInt(s.cache_bytes||0).toLocaleString()+' cached page bytes (separate)';
}
for(const id of ['sample','metric','group'])el(id).addEventListener('input',draw);
el('controls').hidden=!canControl;
for(const b of document.querySelectorAll('[data-command]'))b.onclick=async()=>{try{const r=await fetch('/control',{method:'POST',headers:{'X-Profile-Token':token,'Content-Type':'text/plain'},body:b.dataset.command});if(!r.ok)throw Error(await r.text());el('status').textContent='Command queued; applied at the next ML safe point.';}catch(e){el('status').textContent=String(e);}};
async function refresh(){try{const r=await fetch('/samples',{cache:'no-store'});if(!r.ok)throw Error(await r.text());const follow=Number(el('sample').value)>=samples.length-1;samples=await r.json();el('sample').max=Math.max(0,samples.length-1);if(follow)el('sample').value=el('sample').max;draw();}catch(e){el('status').textContent=String(e);}setTimeout(refresh,1000);}
draw();if(live)refresh();
</script></html>'''


def html(samples, live=False, token="", control=False):
    encoded = json.dumps(samples, separators=(",", ":")).replace("<", "\\u003c")
    return (TEMPLATE.replace("__LIVE__", json.dumps(live)).replace("__TOKEN__", json.dumps(token))
            .replace("__CONTROL__", json.dumps(control)).replace("__DATA__", encoded))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", nargs="?", default="profile.rp")
    parser.add_argument("--output", default="profile.html")
    parser.add_argument("--serve", action="store_true")
    parser.add_argument("--port", type=int, default=0)
    parser.add_argument("--control", help="runtime -rp_control Unix datagram socket")
    args = parser.parse_args()
    if not args.serve:
        Path(args.output).write_text(html(data(args.file)), encoding="utf-8")
        print(args.output)
        return
    token = secrets.token_hex(32)
    class Handler(BaseHTTPRequestHandler):
        def log_message(self, format, *args):
            pass  # Do not flood the terminal with the one-second refreshes.
        def reply(self, status, body, content_type="text/plain; charset=utf-8"):
            payload = body.encode("utf-8")
            self.send_response(status)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(payload)))
            self.send_header("Cache-Control", "no-store")
            self.send_header("X-Content-Type-Options", "nosniff")
            self.end_headers()
            self.wfile.write(payload)
        def do_GET(self):
            # Validate Host to prevent DNS rebinding against the loopback server.
            if self.headers.get("Host") != f"127.0.0.1:{self.server.server_port}":
                return self.reply(403, "Invalid host")
            try:
                if self.path == "/":
                    self.reply(200, html([], True, token, bool(args.control)), "text/html; charset=utf-8")
                elif self.path == "/samples":
                    self.reply(200, json.dumps(data(args.file)), "application/json")
                else:
                    self.reply(404, "Not found")
            except (OSError, ValueError, KeyError) as exc:
                self.reply(503, str(exc))
        def do_POST(self):
            if self.path != "/control" or not args.control or self.headers.get("X-Profile-Token") != token:
                return self.reply(403, "Control unavailable")
            try:
                size = int(self.headers.get("Content-Length", "0"))
                if not 0 < size <= 16:
                    return self.reply(400, "Invalid command")
                command = self.rfile.read(size)
                if command not in (b"start", b"pause", b"sample", b"flush"):
                    return self.reply(400, "Invalid command")
                with socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM) as client:
                    client.settimeout(1)
                    client.sendto(command, args.control)
                self.reply(202, "Queued")
            except (OSError, ValueError) as exc:
                self.reply(503, str(exc))
    with HTTPServer(("127.0.0.1", args.port), Handler) as server:
        print(f"http://127.0.0.1:{server.server_port}/", flush=True)
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()
