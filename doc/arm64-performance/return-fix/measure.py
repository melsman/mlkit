from pathlib import Path
import subprocess,time,statistics,random,json,hashlib,re,resource
root=Path('/private/tmp/mlkit-m12-return-fix')
results=[]
for name,folder,src in [('nucleic','nucleic','nucleic.mlb'),('mlyacc','bench','mlyacc.mlb'),('professor','bench','professor.sml')]:
 exe={v:root/(v+'-'+name+'.exe') for v in ['x64','before','after']}
 expected=(root/'after'/folder/(src+'.out.ok')).read_bytes()
 samples={v:[] for v in exe};rng=random.Random(223)
 for rep in range(6):
  order=list(exe);rng.shuffle(order)
  for variant in order:
   before=resource.getrusage(resource.RUSAGE_CHILDREN);start=time.perf_counter()
   p=subprocess.run([str(exe[variant]),'-report_gc'],cwd=root/'after'/folder,capture_output=True,timeout=60)
   elapsed=time.perf_counter()-start;after=resource.getrusage(resource.RUSAGE_CHILDREN)
   assert p.returncode==0 and p.stdout==expected,(name,variant,p.stderr)
   gc=re.search(r'GC\(([\d.]+)ms\): (\d+) collections',p.stderr.decode());assert gc,p.stderr
   if rep:samples[variant].append({'wall_seconds':elapsed,'cpu_seconds':after.ru_utime+after.ru_stime-before.ru_utime-before.ru_stime,'gc_ms':float(gc[1]),'collections':int(gc[2])})
 med={v:{k:statistics.median(s[k] for s in xs) for k in xs[0]} for v,xs in samples.items()}
 results.append({'benchmark':name,'samples':samples,'medians':med,'executables':{v:{'path':str(p),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for v,p in exe.items()}})
 print(name,json.dumps(med),flush=True)
(root/'measurements.json').write_text(json.dumps({'baseline_revision':'78dfa6c (compiler fbbd01b; subsequent commits documentation/CI only)','gc':True,'warmups':1,'repetitions':5,'basis':'All three variants compile complete Basis and benchmarks from source into fresh M12RetBefore, M12RetAfter, M12RetX64 caches. X64 compiler rebuilt from current source. All compilers built using MLKit -gc.','results':results},indent=2)+'\n')
