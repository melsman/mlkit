from pathlib import Path
import os,subprocess,time,random,statistics,json,hashlib
r=Path('/private/tmp/mlkit-uf-nogc');variants=['x64','arm64','direct'];samples={a:[] for a in variants};rng=random.Random(223);expected=(r/'arm64/uf.mlb.out.ok').read_bytes()
for rep in range(8):
 order=variants[:];rng.shuffle(order)
 for a in order:
  with (r/'actual').open('wb') as out,(r/'stderr').open('wb') as err:
   t=time.perf_counter();p=subprocess.Popen([str(r/(a+'.exe'))],stdout=out,stderr=err);_,status,u=os.wait4(p.pid,0);elapsed=time.perf_counter()-t;p.returncode=os.waitstatus_to_exitcode(status)
  assert p.returncode==0 and (r/'actual').read_bytes()==expected,a
  if rep:samples[a].append({'seconds':elapsed,'cpu_seconds':u.ru_utime+u.ru_stime,'rss_bytes':u.ru_maxrss})
medians={a:{k:statistics.median(s[k] for s in ss) for k in ss[0]} for a,ss in samples.items()}
(r/'repeat.json').write_text(json.dumps({'samples':samples,'medians':medians,'sha256':{a:hashlib.sha256((r/(a+'.exe')).read_bytes()).hexdigest() for a in variants}},indent=2)+'\n')
print(json.dumps(medians,indent=2))
