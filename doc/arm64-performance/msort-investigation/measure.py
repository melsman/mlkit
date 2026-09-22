from pathlib import Path
import subprocess,os,time,json,statistics,random
r=Path('/private/tmp/mlkit-msort');exes={'old-arm64':Path('/private/tmp/mlkit-gc20-current/msort-arm64.exe'),'arm64':r/'arm64.exe','x64':r/'x64.exe'};out={k:[] for k in exes};rng=random.Random(223)
expected=Path('/private/tmp/mlkit-gc20-current/bench/benchmarks/msort.sml.out.ok').read_bytes()
for rep in range(6):
 order=list(exes);rng.shuffle(order)
 for key in order:
  with (r/'actual').open('wb') as stdout,(r/'stderr').open('wb') as stderr:
   t=time.perf_counter();p=subprocess.Popen([str(exes[key]),'-report_gc'],stdout=stdout,stderr=stderr);_,status,u=os.wait4(p.pid,0);dt=time.perf_counter()-t;p.returncode=os.waitstatus_to_exitcode(status)
  assert p.returncode==0 and (r/'actual').read_bytes()==expected
  if rep:out[key].append({'seconds':dt,'cpu_seconds':u.ru_utime+u.ru_stime,'rss_bytes':u.ru_maxrss,'gc':(r/'stderr').read_text().strip()})
(r/'measurements.json').write_text(json.dumps(out,indent=2)+'\n')
for k,v in out.items():print(k,{a:statistics.median(s[a] for s in v) for a in ['seconds','cpu_seconds','rss_bytes']},v[0]['gc'])
