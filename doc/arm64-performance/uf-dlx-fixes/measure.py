from pathlib import Path
import os,subprocess,time,statistics,json,random,re,hashlib
r=Path('/private/tmp/mlkit-outliers-fixed');results=[]
for name in ['uf','dlx']:
 cwd=r/(name+'-arm64');expected=(cwd/(name+'.mlb.out.ok')).read_bytes()
 exes={'old-arm64':Path('/private/tmp/mlkit-outliers')/(name+'-arm64.exe'),'arm64':r/(name+'-arm64.exe'),'x64':r/(name+'-x64.exe')}
 samples={v:[] for v in exes};rng=random.Random(223)
 for rep in range(6):
  order=list(exes);rng.shuffle(order)
  for v in order:
   with (r/'stdout').open('wb') as out,(r/'stderr').open('wb') as err:
    start=time.perf_counter();p=subprocess.Popen([str(exes[v]),'-report_gc'],cwd=cwd,stdout=out,stderr=err);_,status,u=os.wait4(p.pid,0);elapsed=time.perf_counter()-start;p.returncode=os.waitstatus_to_exitcode(status)
   assert p.returncode==0 and (r/'stdout').read_bytes()==expected,(name,v)
   gc=re.search(r'GC\(([\d.]+)ms\): (\d+) collections, (\d+)kb rpages',(r/'stderr').read_text());assert gc
   if rep:samples[v].append({'seconds':elapsed,'cpu_seconds':u.ru_utime+u.ru_stime,'rss_bytes':u.ru_maxrss,'gc_ms':float(gc[1]),'collections':int(gc[2]),'region_pages':int(gc[3])})
 med={v:{k:statistics.median(x[k] for x in ss) for k in ss[0]} for v,ss in samples.items()}
 results.append({'benchmark':name,'samples':samples,'medians':med,'executables':{v:hashlib.sha256(p.read_bytes()).hexdigest() for v,p in exes.items()}});print(name,json.dumps(med),flush=True)
(r/'measurements.json').write_text(json.dumps(results,indent=2)+'\n')
