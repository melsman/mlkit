from pathlib import Path
import re,json,subprocess,collections
root=Path('/private/tmp/mlkit-m12-next');row=json.loads((root/'links.jsonl').read_text().splitlines()[-1]);args=list(row['args']);args[1]=str(root/'mlyacc-constants.exe');folder=root/'constants';folder.mkdir(exist_ok=True)
counts=collections.Counter()
for i,a in enumerate(row['args']):
 if not a.endswith('.o'):continue
 p=Path(a);p=p if p.is_absolute() else Path(row['cwd'])/p
 if '/bench/' not in str(p) or not p.with_suffix('.s').exists():continue
 text=p.with_suffix('.s').read_text()
 def fold(m):
  reg=m[1];lines=m[0].splitlines();parts=[0]*4
  for line in lines:
   z=re.search(r'#(\d+), lsl #(\d+)',line);parts[int(z[2])//16]=int(z[1])
  candidates=[]
  for fill,op in [(0,'movz'),(65535,'movn')]:
   changed=[k for k,n in enumerate(parts) if n!=fill] or [0]
   k=changed[0];seed=parts[k] if op=='movz' else parts[k]^65535
   out=[f'\t{op} {reg}, #{seed}, lsl #{16*k}\n']
   out += [f'\tmovk {reg}, #{parts[k]}, lsl #{16*k}\n' for k in changed[1:]]
   candidates.append(out)
  best=min(candidates,key=len)
  if len(best)>=len(lines):return m[0]
  counts['sites']+=1;counts['instructions_removed']+=len(lines)-len(best)
  return ''.join(best)
 out=re.sub(r'\tmovz (x\d+), #\d+, lsl #0\n(?:\tmovk \1, #\d+, lsl #(?:16|32|48)\n)*',fold,text)
 if out==text:continue
 asm=folder/(str(i)+'-'+p.with_suffix('.s').name);asm.write_text(out)
 subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(asm.with_suffix('.o'))],capture_output=True,check=True);args[i]=str(asm.with_suffix('.o'))
subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],capture_output=True,check=True)
(root/'constants.json').write_text(json.dumps(counts,indent=2)+'\n');print(dict(counts))
