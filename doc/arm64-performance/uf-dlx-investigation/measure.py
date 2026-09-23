import os,subprocess,time,statistics,json,random,re,hashlib
from pathlib import Path
r=Path('/private/tmp/mlkit-outliers');results=[]
for name,variants in [('uf',['x64','arm64','call','mul','both']),('dlx',['x64','arm64','polls','request'])]:
 cwd=r/(name+'-arm64');expected=(cwd/(name+'.mlb.out.ok')).read_bytes();samples={v:[] for v in variants};rng=random.Random(223)
 for rep in range(6):
  order=variants[:];rng.shuffle(order)
  for v in order:
   exe=r/(name+'-'+v+'.exe')
   with (r/'actual').open('wb') as out,(r/'stderr').open('wb') as err:
    start=time.perf_counter();p=subprocess.Popen([str(exe),'-report_gc'],cwd=cwd,stdout=out,stderr=err);_,status,u=os.wait4(p.pid,0);elapsed=time.perf_counter()-start;p.returncode=os.waitstatus_to_exitcode(status)
   assert p.returncode==0 and (r/'actual').read_bytes()==expected,(name,v)
   gc=re.search(r'GC\(([\d.]+)ms\): (\d+) collections, (\d+)kb rpages',(r/'stderr').read_text());assert gc
   if rep:samples[v].append({'seconds':elapsed,'cpu_seconds':u.ru_utime+u.ru_stime,'rss_bytes':u.ru_maxrss,'gc_ms':float(gc[1]),'collections':int(gc[2]),'region_pages':int(gc[3])})
 med={v:{k:statistics.median(x[k] for x in ss) for k in ss[0]} for v,ss in samples.items()}
 results.append({'benchmark':name,'samples':samples,'medians':med,'executables':{v:hashlib.sha256((r/(name+'-'+v+'.exe')).read_bytes()).hexdigest() for v in variants}});print(name,json.dumps(med),flush=True)
(r/'measurements.json').write_text(json.dumps(results,indent=2)+'\n')
