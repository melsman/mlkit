#!/usr/bin/env python3
import os,sys,json,shutil,hashlib,subprocess,time,threading,signal,statistics,random,platform
from pathlib import Path
ROOT=Path('/private/tmp/mlkit-gc20-current');REPO=Path('/private/tmp/mlkit-m2-source')
SOURCES={'bench':Path('/private/tmp/mlkit-m11-bench'),'paper':Path('/private/tmp/mlkit-m11-published')}
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def git(p):return subprocess.check_output(['git','rev-parse','HEAD'],cwd=p,text=True).strip()
def specs():
 rows=[]
 for n,s in [('calc','calc.mlb'),('DLX','dlx.mlb'),('kbc','kbc.sml'),('lexgen','lexgen.sml'),('logic','logic.mlb'),('nucleic','nucleic.mlb'),('patricia','patricia.sml'),('ray','ray.mlb'),('uf','uf.mlb'),('vliw','vliw.sml')]:
  rows.append((n,ROOT/'paper/src'/n.lower(),s))
 for n in ['mlyacc','professor','barnes-hut','fft','life','mandelbrot','mpuz','msort','simple','zebra']:
  rows.append((n,ROOT/'bench/benchmarks',n+('.mlb' if n in ['mlyacc','barnes-hut'] else '.sml')))
 return sorted(rows,key=lambda row:row[0].lower())
def build():
 assert not (ROOT/'manifest.json').exists()
 for a in ['arm64','x64']:assert not list((REPO/'basis').glob('MLB/*GC20Current'+a)),a
 for key,p in SOURCES.items():shutil.copytree(p,ROOT/key,ignore=shutil.ignore_patterns('.git','MLB'),dirs_exist_ok=False)
 meta={'revision':git(REPO),'source_revisions':{k:git(p) for k,p in SOURCES.items()},'platform':platform.platform(),'date':'2026-09-23','gc':True,'warmups':1,'repetitions':5,'memory_metric':'wait4 rusage.ru_maxrss, Darwin bytes, peak process RSS','timing_metric':'perf_counter elapsed seconds, separate process, includes startup and I/O','compiler_sha256':{a:sha(ROOT/('mlkit-'+a)) for a in ['arm64','x64']},'runtime_sha256':{a:sha(REPO/'lib'/a/'runtimeSystemGC.a') for a in ['darwin-arm64','darwin-x86_64']}}
 rows=[]
 for name,cwd,src in specs():
  expected=cwd/(src+'.out.ok')
  if not expected.exists():expected=cwd/(src.lower()+'.out.ok')
  for arch in ['x64','arm64']:
   exe=ROOT/(name+'-'+arch+'.exe');cmd=[str(ROOT/('mlkit-'+arch)),'-gc','--mlb-subdir','GC20Current'+arch,'-o',str(exe)]
   if arch=='x64':cmd+=['-as','as -arch x86_64 -q','-ldexe','gcc -arch x86_64 -Wl,-stack_size,0x10000000']
   cmd.append(src)
   with (ROOT/(name+'-'+arch+'.compile.log')).open('w') as f:subprocess.run(cmd,cwd=cwd,env=dict(os.environ,SML_LIB=str(REPO)),stdout=f,stderr=subprocess.STDOUT,check=True,timeout=900)
   assert subprocess.check_output(['lipo','-archs',str(exe)],text=True).strip()==('x86_64' if arch=='x64' else 'arm64')
   rows.append({'benchmark':name,'arch':arch,'cwd':str(cwd),'expected':str(expected),'source':str(cwd/src),'command':cmd,'exe':str(exe),'exe_sha256':sha(exe),'expected_sha256':sha(expected)})
   print('Built',name,arch,flush=True)
 (ROOT/'manifest.json').write_text(json.dumps({'metadata':meta,'configurations':rows},indent=2)+'\n')
def measure():
 manifest=json.loads((ROOT/'manifest.json').read_text());results=[];rng=random.Random(223)
 for name,_,_ in specs():
  configs=[r for r in manifest['configurations'] if r['benchmark']==name];samples={r['arch']:[] for r in configs}
  for rep in range(6):
   order=configs[:];rng.shuffle(order)
   for row in order:
    assert sha(Path(row['exe']))==row['exe_sha256']
    key=name+'-'+row['arch'];out=ROOT/(key+'.stdout');err=ROOT/(key+'.stderr')
    with out.open('wb') as stdout,err.open('wb') as stderr:
     start=time.perf_counter();p=subprocess.Popen([row['exe'],'-report_gc'],cwd=row['cwd'],stdout=stdout,stderr=stderr)
     def kill():
      try:os.kill(p.pid,signal.SIGKILL)
      except ProcessLookupError:pass
     watchdog=threading.Timer(180,kill);watchdog.start()
     try:
      pid,status,usage=os.wait4(p.pid,0);elapsed=time.perf_counter()-start;p.returncode=os.waitstatus_to_exitcode(status)
     finally:watchdog.cancel();watchdog.join()
    assert p.returncode==0 and out.read_bytes()==Path(row['expected']).read_bytes(),key
    assert usage.ru_maxrss>0,key
    if rep:samples[row['arch']].append({'seconds':elapsed,'cpu_seconds':usage.ru_utime+usage.ru_stime,'peak_rss_bytes':usage.ru_maxrss})
  med={a:{k:statistics.median(x[k] for x in ss) for k in ss[0]} for a,ss in samples.items()}
  results.append({'benchmark':name,'samples':samples,'medians':med})
  print(name,json.dumps(med),flush=True)
  (ROOT/'measurements.json').write_text(json.dumps({'metadata':manifest['metadata'],'results':results},indent=2)+'\n')
if __name__=='__main__': {'build':build,'measure':measure}[sys.argv[1]]()
