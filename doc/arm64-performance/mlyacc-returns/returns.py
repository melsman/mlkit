from pathlib import Path
import re,json,subprocess
root=Path('/private/tmp/mlkit-m12-mlyacc-study')
row=json.loads((root/'links.jsonl').read_text().splitlines()[-1])
stats={}
for variant in ['brreturns','brunions','brcomparisons']:
 outdir=root/('mlyacc-'+variant);outdir.mkdir(exist_ok=True)
 args=list(row['args']);args[1]=str(root/('mlyacc-'+variant+'.exe'));count=0
 for i,a in enumerate(row['args']):
  if not a.endswith('.o'):continue
  p=Path(a);p=p if p.is_absolute() else Path(row['cwd'])/p
  if '/bench/' not in str(p) or not p.with_suffix('.s').exists():continue
  text=p.with_suffix('.s').read_text()
  def change(m):
   global count
   if variant=='brunions' and not m[1].startswith('_F.union19_'):return m[0]
   if variant=='brcomparisons' and not re.match(r'_F\.(?:gt|eq)',m[1]):return m[0]
   body,n=re.subn(r'^\tret *$', '\tbr x30',m[0],flags=re.M);count+=n
   return body
  out=re.sub(r'^(_F\.[^:\n]+):\n(.*?)(?=^\.text\n|\Z)',change,text,flags=re.M|re.S)
  if out==text:continue
  asm=outdir/(str(i)+'-'+p.with_suffix('.s').name);asm.write_text(out)
  subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(asm.with_suffix('.o'))],check=True,capture_output=True)
  args[i]=str(asm.with_suffix('.o'))
 subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True,capture_output=True)
 stats[variant]=count
(root/'returns.json').write_text(json.dumps(stats,indent=2)+'\n')
