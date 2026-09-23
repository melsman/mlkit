from pathlib import Path
import hashlib,json,subprocess,resource,time,statistics,random,re
root=Path('/private/tmp/mlkit-m12-assembly');results=[]
for name,src in [('mlyacc','mlyacc.mlb'),('professor','professor.sml')]:
 expected=(root/'bench'/(src+'.out.ok')).read_bytes()
 variants={'x64':Path('/private/tmp/mlkit-m11-final')/(name+'-x64-gc.exe'),'previous_basis':Path('/private/tmp/mlkit-m12-static')/(name+'-gc.exe'),'fresh_basis':root/(name+'-gc.exe'),'peephole':root/(name+'-peephole.exe'),'schedule':root/(name+'-schedule.exe')}
 if name=='professor':variants['region_fp']=root/(name+'-region_fp.exe')
 samples={k:[] for k in variants}
 rng=random.Random(223)
 for rep in range(6):
  order=list(variants);rng.shuffle(order)
  for v in order:
   before=resource.getrusage(resource.RUSAGE_CHILDREN);start=time.perf_counter()
   p=subprocess.run([str(variants[v]),'-report_gc'],cwd=root/'bench',capture_output=True,timeout=30)
   wall=time.perf_counter()-start;after=resource.getrusage(resource.RUSAGE_CHILDREN)
   assert p.returncode==0 and p.stdout==expected,(name,v,p.stderr.decode())
   m=re.search(r'GC\(([\d.]+)ms\): (\d+) collections',p.stderr.decode());assert m
   cpu=after.ru_utime+after.ru_stime-before.ru_utime-before.ru_stime;gc=float(m[1])/1000
   if rep:samples[v].append({'wall':wall,'cpu':cpu,'gc_cpu':gc,'outside_gc_cpu':cpu-gc,'collections':int(m[2])})
 median={v:{key:statistics.median(r[key] for r in rows) for key in rows[0]} for v,rows in samples.items()}
 row={'benchmark':name,'samples':samples,'median':median,'executables':{v:{'path':str(exe),'sha256':hashlib.sha256(exe.read_bytes()).hexdigest()} for v,exe in variants.items()},'expected_sha256':hashlib.sha256(expected).hexdigest()}
 results.append(row);print(name,json.dumps(median),flush=True)
 (root/'measurements.json').write_text(json.dumps({'revision':'07cf1e8','warmups':1,'repetitions':5,'order':'seeded shuffle per round','runtime_flag':'-report_gc','variants':'fresh Basis rebuilt by current MLKit; isolated assembly experiments affect benchmark units only','results':results},indent=2)+'\n')
