from pathlib import Path
import hashlib,json,os,re,resource,statistics,subprocess,time
root=Path('/private/tmp/mlkit-m12-static'); base=root
programs=[('nucleic',base/'artifact/nucleic','nucleic.mlb'),('mlyacc',base/'bench','mlyacc.mlb'),('professor',base/'bench','professor.sml')]
env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'); results=[]
for name,cwd,source in programs:
 expected=(cwd/(source+'.out.ok')).read_bytes()
 configs=[('x64',Path('/private/tmp/mlkit-m11-final')/(name+'-x64-gc.exe')),('before',Path('/private/tmp/mlkit-m12-layout')/(name+'-gc.exe')),('after',root/(name+'-gc.exe'))]
 samples={k:[] for k,_ in configs}
 for rep in range(4):
  for version,exe in (configs if rep%2==0 else list(reversed(configs))):
   before=resource.getrusage(resource.RUSAGE_CHILDREN);start=time.perf_counter()
   p=subprocess.run([str(exe),'-report_gc'],cwd=cwd,capture_output=True,timeout=60)
   wall=time.perf_counter()-start;after=resource.getrusage(resource.RUSAGE_CHILDREN)
   assert p.returncode==0 and p.stdout==expected,(name,version,p.stderr)
   report=p.stderr.decode();(root/(name+'-'+version+'.report')).write_text(report)
   m=re.search(r'GC\(([\d.]+)ms\): (\d+) collections',report);assert m,report
   gc=float(m[1])/1000
   cpu=after.ru_utime+after.ru_stime-before.ru_utime-before.ru_stime
   if rep:samples[version].append({'wall':wall,'cpu':cpu,'gc_cpu':gc,'outside_gc_cpu':cpu-gc,'collections':int(m[2])})
 row={'benchmark':name,'samples':samples,'median':{v:{key:statistics.median(r[key] for r in rows) for key in rows[0]} for v,rows in samples.items()},'executables':{v:{'path':str(exe),'sha256':hashlib.sha256(exe.read_bytes()).hexdigest()} for v,exe in configs},'expected_sha256':hashlib.sha256(expected).hexdigest()}
 results.append(row)
 print(name,json.dumps(row['median']),flush=True)
 (root/'comparison.json').write_text(json.dumps({'baseline_revision':'95e7c2e (code 91b950f)','x64_binary_revision':'milestone 11 (unchanged X64 backend)','warmups':1,'repetitions':3,'runtime_flag':'-report_gc','basis':'unchanged M12Inline Basis; benchmark units freshly compiled; after includes executable bounds, allocation-aware polling and list tests','results':results},indent=2)+'\n')
