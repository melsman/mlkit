from pathlib import Path
import re,json,subprocess,collections

root=Path('/private/tmp/mlkit-m12-mlyacc-study')
links={Path(r['args'][1]).name.split('-')[0]:r for r in map(json.loads,(root/'links.jsonl').read_text().splitlines())}

def functions(text):
 return list(re.finditer(r'^(_F\.[^:\n]+):\n(.*?)(?=^\.text\n|\Z)',text,re.M|re.S))

stats={}
for name,row in links.items():
 objects={}
 for i,arg in enumerate(row['args']):
  if not arg.endswith('.o'):continue
  p=Path(arg);p=p if p.is_absolute() else Path(row['cwd'])/p
  if p.with_suffix('.s').exists() and ('/bench/' in str(p) or '/basis/' in str(p)):
   objects[i]=(p,p.with_suffix('.s').read_text())
 leaves={};wrappers={}
 header='\tstp x29, x30, [sp, #0]\n\tadd x29, sp, #0\n'
 footer='\tldp x29, x30, [sp, #0]\n\tadd sp, sp, #16\n\tret \n'
 for _,text in objects.values():
  for m in functions(text):
   symbol,body=m.groups()
   w=re.fullmatch(r'\tb (_F\.[^\n]+)\n',body)
   if w:wrappers[symbol]=w[1]
   if not body.startswith(header) or not body.endswith(footer):continue
   middle=body[len(header):-len(footer)]
   if len(middle.splitlines())>14:continue
   if re.search(r'\b(?:sp|x29|x30)\b',middle):continue
   if not middle or any(not re.match(r'\t(?:mov|movz|movk|fmov|ldr|ldrb|ldrh|ldp|cmp|fcmp|cset|lsl|lsr|asr|add|sub|and|orr|eor|ubfx|sxtw|uxtw) ',line) for line in middle.splitlines()):continue
   leaves[symbol]=middle
 def resolve(symbol):
  seen=set()
  while symbol in wrappers and symbol not in seen:seen.add(symbol);symbol=wrappers[symbol]
  return symbol if symbol in leaves else None
 for variant in ['leaf','inline','comparisons']:
  outdir=root/(name+'-'+variant);outdir.mkdir(exist_ok=True)
  args=list(row['args']);args[1]=str(root/(name+'-'+variant+'.exe'))
  counts=collections.Counter();changed=0
  for i,(obj,text) in objects.items():
   # Keep Basis identical: this first experiment isolates benchmark-unit changes.
   if '/bench/' not in str(obj):continue
   if variant=='leaf':
    def omit(m):
     symbol,body=m.groups()
     if symbol not in leaves:return m[0]
     counts['leaf_headers_removed']+=1
     return symbol+':\n'+leaves[symbol]+'\tadd sp, sp, #16\n\tret \n'
    out=re.sub(r'^(_F\.[^:\n]+):\n(.*?)(?=^\.text\n|\Z)',omit,text,flags=re.M|re.S)
   else:
    def inline(m):
     pc,target=m[1],m[2];callee=resolve(target)
     if callee is None:return m[0]
     if variant=='comparisons' and not re.match(r'_F\.(?:gt|eq)',callee):return m[0]
     counts['calls_inlined']+=1;counts['callee:'+callee]+=1
     # Preserve LR, flags and all ML registers. The callee never observes SP,
     # calls, branches, stores, allocates, or reads/modifies the frame registers.
     # Keep the continuation label/descriptor and branch over its inline data.
     return '\tadr x30, '+pc+'\n'+leaves[callee]+'\tb '+pc+'\n'
    out=re.sub(r'\tsub sp, sp, #16\n\tadr x30, (L\w+)\n\tb (_F\.[^\n]+)\n(?=\.p2align 3\n)',inline,text)
   if out==text:continue
   asm=outdir/(str(i)+'-'+obj.with_suffix('.s').name);asm.write_text(out);newobj=asm.with_suffix('.o')
   subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(newobj)],check=True,capture_output=True)
   args[i]=str(newobj);changed+=1
  subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True,capture_output=True)
  stats[name+'-'+variant]={'changed_units':changed,'counts':dict(counts)}
  print(name,variant,changed,'units',flush=True)
(root/'experiments.json').write_text(json.dumps(stats,indent=2)+'\n')
