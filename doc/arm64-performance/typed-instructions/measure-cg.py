from pathlib import Path
import os,subprocess,shutil,json,re,statistics,hashlib,time
root=Path('/private/tmp/mlkit-m12-typed')
source=Path('/private/tmp/mlkit-m12-fresh-basis/nucleic')
compilers={'before':Path('/private/tmp/mlkit-m12-select/mlkit'),'after':root/'mlkit'}
rows=[]
for rep in range(3):
 for variant in (['before','after'] if rep%2==0 else ['after','before']):
  cwd=root/(variant+'-'+str(rep+1));shutil.copytree(source,cwd,ignore=shutil.ignore_patterns('MLB'),dirs_exist_ok=True)
  for stale in list(cwd.rglob('*.KITtimings'))+list(cwd.rglob('KITtimings')):stale.unlink()
  exe=cwd/'nucleic.exe'
  start=time.perf_counter()
  with (cwd/'compile.log').open('w') as out:
   subprocess.run([str(compilers[variant]),'-gc','--mlb-subdir','M12SelectFinal','--timings','--log_to_file','--no_delete_target_files','-o',str(exe),'nucleic.mlb'],cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=out,stderr=subprocess.STDOUT,check=True)
  elapsed=time.perf_counter()-start
  files=list(cwd.rglob('*.KITtimings'))
  if not files:files=list(cwd.rglob('KITtimings'))
  assert files,list(cwd.iterdir())
  times=[]
  for p in files:
   times.extend(float(m) for m in re.findall(r'^CG\s+([0-9.]+)',p.read_text(),re.M))
  assert times
  result=subprocess.run([str(exe)],cwd=cwd,capture_output=True,check=True)
  assert result.stdout==(cwd/'nucleic.mlb.out.ok').read_bytes()
  row={'variant':variant,'run':rep+1,'cg_seconds':sum(times),'compile_wall_seconds':elapsed,'entries':times,'timing_files':[str(p.relative_to(root)) for p in files]}
  rows.append(row);print(row,flush=True)
(root/'cg-results.json').write_text(json.dumps({'baseline_revision':'ec0c186','host':'Both compilers are X64-hosted MLKit -gc builds running under Rosetta2','basis_cache':'M12SelectFinal, warmed and shared; all benchmark-local compilation caches absent each run','runs':rows,'median_cg_seconds':{v:statistics.median(row['cg_seconds'] for row in rows if row['variant']==v) for v in compilers},'compilers':{v:{'path':str(p),'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for v,p in compilers.items()}},indent=2)+'\n')
