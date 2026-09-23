from pathlib import Path
import os,subprocess,time,random,statistics,json,hashlib
r=Path('/private/tmp/mlkit-three-fixed');rng=random.Random(223);results=[]
for mode in ['no_gc','gc']:
 for name in ['simple','mpuz','uf']:
  expected=Path('/private/tmp/mlkit-nogc20-current/paper/src/uf/uf.mlb.out.ok' if name=='uf' else '/private/tmp/mlkit-nogc20-current/bench/benchmarks/'+name+'.sml.out.ok').read_bytes()
  variants=['old','arm64','x64'];samples={v:[] for v in variants}
  for rep in range(8):
   order=variants[:];rng.shuffle(order)
   for variant in order:
    exe=r/f'final-{mode}-{variant}-{name}.exe'
    with (r/'actual').open('wb') as out,(r/'stderr').open('wb') as err:
     start=time.perf_counter();p=subprocess.Popen([str(exe)],stdout=out,stderr=err);_,status,u=os.wait4(p.pid,0);elapsed=time.perf_counter()-start;p.returncode=os.waitstatus_to_exitcode(status)
    assert p.returncode==0 and (r/'actual').read_bytes()==expected,(mode,name,variant,p.returncode)
    if rep:samples[variant].append({'seconds':elapsed,'cpu_seconds':u.ru_utime+u.ru_stime,'rss_bytes':u.ru_maxrss})
  medians={v:{k:statistics.median(row[k] for row in rows) for k in rows[0]} for v,rows in samples.items()}
  results.append({'mode':mode,'benchmark':name,'samples':samples,'medians':medians,'sha256':{v:hashlib.sha256((r/f'final-{mode}-{v}-{name}.exe').read_bytes()).hexdigest() for v in variants}})
  print(mode,name,json.dumps(medians),flush=True)
  (r/'measurements.json').write_text(json.dumps(results,indent=2)+'\n')
