from pathlib import Path
import json,re,subprocess
r=Path('/private/tmp/mlkit-m12-assembly');links={}
for row in map(json.loads,(r/'links.jsonl').read_text().splitlines()):links[Path(row['args'][1]).name.split('-')[0]]=row
results={}
for name,row in links.items():
 folder=r/(name+'-count');folder.mkdir(exist_ok=True);args=list(row['args']);args[1]=str(r/(name+'-count.exe'));names=[]
 for i,a in enumerate(args):
  if not a.endswith('.o'):continue
  obj=Path(a);obj=obj if obj.is_absolute() else Path(row['cwd'])/obj
  asm=obj.with_suffix('.s')
  if not asm.exists():continue
  if not(str(obj).startswith(str(r/'bench')) or obj.name=='List.sml.o'):continue
  s=asm.read_text()
  pattern=r'^(_F\.(?:union19_|gt3_|eq2_|gtTerm\d+_|eq_term\d+_|findSol55_|count37_|_17_)[^:\n]*):\n'
  def inject(m):
   index=len(names);names.append(m[1]);off=index*8
   return m[0]+f'\tadrp x16, _study_counts@PAGE\n\tadd x16, x16, _study_counts@PAGEOFF\n\tldr x17, [x16, #{off}]\n\tadd x17, x17, #1\n\tstr x17, [x16, #{off}]\n'
  t,n=re.subn(pattern,inject,s,flags=re.M)
  if not n:continue
  out=folder/(str(i)+'-'+asm.name);out.write_text(t);objout=out.with_suffix('.o')
  subprocess.run(['gcc','-arch','arm64','-c',str(out),'-o',str(objout)],check=True)
  args[i]=str(objout)
 c=folder/'counts.c'
 c.write_text('#include <stdio.h>\nunsigned long long study_counts['+str(len(names))+'];\n__attribute__((destructor)) static void report(void) {\n'+''.join('fprintf(stderr,"'+s+' %llu\\n", study_counts['+str(i)+']);\n' for i,s in enumerate(names))+'}\n')
 subprocess.run(['gcc','-arch','arm64',str(c)]+args,cwd=row['cwd'],check=True)
 p=subprocess.run([args[1]],cwd=row['cwd'],capture_output=True,check=True)
 expected=(r/'bench'/('mlyacc.mlb.out.ok' if name=='mlyacc' else 'professor.sml.out.ok')).read_bytes();assert p.stdout==expected
 counts={a:int(b) for a,b in (line.rsplit(' ',1) for line in p.stderr.decode().splitlines())}
 results[name]=dict(sorted(counts.items(),key=lambda v:-v[1])); print(name,results[name],flush=True)
(r/'call-counts.json').write_text(json.dumps(results,indent=2)+'\n')
