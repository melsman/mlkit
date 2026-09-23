from pathlib import Path
import subprocess,time,statistics,random,json,hashlib,re,resource
root=Path('/private/tmp/mlkit-m12-mlyacc-study')
results=[]
for name,src in [('mlyacc','mlyacc.mlb')]:
 exe={'x64':Path('/private/tmp/mlkit-m11-final/mlyacc-x64-gc.exe'), **{v:root/('mlyacc-'+v+'.exe') for v in ['baseline','leaf','inline','comparisons','brreturns','brunions','brcomparisons']}}
 expected=(Path('/private/tmp/mlkit-m12-select/bench')/(src+'.out.ok')).read_bytes()
 samples={v:[] for v in exe};rng=random.Random(223)
 for rep in range(6):
  order=list(exe);rng.shuffle(order)
  for variant in order:
   before=resource.getrusage(resource.RUSAGE_CHILDREN);start=time.perf_counter()
   p=subprocess.run([str(exe[variant]),'-report_gc'],cwd=Path('/private/tmp/mlkit-m12-select/bench'),capture_output=True,timeout=60)
   elapsed=time.perf_counter()-start;after=resource.getrusage(resource.RUSAGE_CHILDREN)
   assert p.returncode==0 and p.stdout==expected,(name,variant,p.stderr)
   gc=re.search(r'GC\(([\d.]+)ms\): (\d+) collections',p.stderr.decode());assert gc,p.stderr
   if rep:samples[variant].append({'wall_seconds':elapsed,'cpu_seconds':after.ru_utime+after.ru_stime-before.ru_utime-before.ru_stime,'gc_ms':float(gc[1]),'collections':int(gc[2])})
 med={v:{k:statistics.median(s[k] for s in xs) for k in xs[0]} for v,xs in samples.items()}
 results.append({'benchmark':name,'samples':samples,'medians':med,'executables':{v:{'path':str(p),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for v,p in exe.items()}})
 print(name,json.dumps(med),flush=True)
(root/'measurements.json').write_text(json.dumps({'production_revision':'924178a','gc':True,'warmups':1,'repetitions':5,'basis':'All ARM variants share M12SelectFinal Basis/runtime; benchmark assembly only changes','results':results},indent=2)+'\n')
