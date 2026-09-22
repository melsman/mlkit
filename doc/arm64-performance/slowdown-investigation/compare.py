from pathlib import Path
import hashlib,json,os,re,resource,statistics,subprocess,time
root=Path('/private/tmp/mlkit-m12-diagnosis'); base=Path('/private/tmp/mlkit-m12-layout')
programs=[('nucleic',base/'artifact/nucleic','nucleic.mlb'),('mlyacc',base/'bench','mlyacc.mlb'),('professor',base/'bench','professor.sml')]
env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'); results=[]
for name,cwd,source in programs:
 expected=(cwd/(source+'.out.ok')).read_bytes()
 for kind in ['fast','count']:
  cmd=[str(base/'mlkit'),'-gc','--mlb-subdir','M12Inline','--no_delete_target_files','-ldexe','gcc -arch arm64 '+str(root/('Arm64GC-'+kind+'.o')),'-o',str(root/(name+'-'+kind+'.exe')),source]
  with (root/(name+'-'+kind+'.compile.log')).open('w') as log:
   subprocess.run(cmd,cwd=cwd,env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
 configs=[('x64',Path('/private/tmp/mlkit-m11-final')/(name+'-x64-gc.exe')),('arm64',base/(name+'-gc.exe')),('arm64_range_reject',root/(name+'-fast.exe'))]
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
 p=subprocess.run([str(root/(name+'-count.exe')),'-report_gc'],cwd=cwd,capture_output=True,timeout=60)
 assert p.returncode==0 and p.stdout==expected,(name,'count')
 (root/(name+'-count.report')).write_bytes(p.stderr)
 row={'benchmark':name,'samples':samples,'median':{v:{key:statistics.median(r[key] for r in rows) for key in rows[0]} for v,rows in samples.items()},'lookup_counters':p.stderr.decode(),'executables':{v:{'path':str(exe),'sha256':hashlib.sha256(exe.read_bytes()).hexdigest()} for v,exe in configs},'expected_sha256':hashlib.sha256(expected).hexdigest()}
 results.append(row)
 print(name,json.dumps(row['median']),p.stderr.decode().strip(),flush=True)
 (root/'comparison.json').write_text(json.dumps({'baseline_revision':'91b950f','x64_binary_revision':'milestone 11 (unchanged X64 backend)','warmups':1,'repetitions':3,'runtime_flag':'-report_gc','basis':'unchanged M12Inline Basis; exact same ML objects for original ARM64 and range-reject experiment','results':results},indent=2)+'\n')
