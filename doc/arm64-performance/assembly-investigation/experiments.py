from pathlib import Path
import re,json,subprocess,hashlib,collections
root=Path('/private/tmp/mlkit-m12-assembly')
links={}
for row in map(json.loads,(root/'links.jsonl').read_text().splitlines()):
 name=Path(row['args'][1]).name.split('-')[0]; links[name]=row

def micro(text):
 count=collections.Counter()
 def rule(pat,repl,label):
  nonlocal text
  text,n=re.subn(pat,repl,text,flags=re.M);count[label]+=n
 # These rewrites preserve all architectural registers and flags.
 for _ in range(3):
  rule(r'^\tmov (x\d+), (x\d+)\n\tmov \2, \1\n',r'\tmov \1, \2\n','roundtrip_copy')
  rule(r'^\tmov (x\d+), (x\d+)\n\t(ldr|ldrb|ldrh) \1, \[\1, (#-?\d+)\]\n',r'\t\3 \1, [\2, \4]\n','load_base_copy')
  rule(r'^\tmov (x\d+), (x\d+)\n\t(add|sub|and|orr|eor|lsr|lsl|asr) \1, \1, (#-?\d+)\n',r'\t\3 \1, \2, \4\n','alu_source_copy')
  rule(r'^\tmovz (x\d+), #0, lsl #0\n\torr (x\d+), \2, \1\n',r'\tmovz \1, #0, lsl #0\n','zero_or')
 # Pair either address order, retaining the original lower/higher register mapping.
 pat=r'^\t(ldr|str) (x\d+|d\d+), \[(sp|x\d+), #(-?\d+)\]\n\t\1 (x\d+|d\d+), \[\3, #(-?\d+)\]\n'
 def pair(m):
  op,a,base,x,b,y=m.groups();x=int(x);y=int(y)
  if a[0]!=b[0] or abs(x-y)!=8 or min(x,y)%8 or not -512<=min(x,y)<=504:return m[0]
  if op=='ldr' and (a==b or a==base):return m[0]
  if y<x:a,b=b,a
  count['adjacent_pair']+=1
  return '\t'+('ldp' if op=='ldr' else 'stp')+f' {a}, {b}, [{base}, #{min(x,y)}]\n'
 text=re.sub(pat,pair,text,flags=re.M)
 # Thread branches through label-only blocks ending in one direct branch.
 lines=text.splitlines(True); jumps={}
 for i,line in enumerate(lines):
  if re.fullmatch(r'L\w+:\n',line):
   j=i+1
   while j<len(lines) and re.fullmatch(r'L\w+:\n',lines[j]):j+=1
   if j<len(lines):
    m=re.fullmatch(r'\tb (L\w+)\n',lines[j])
    if m:jumps[line.strip()[:-1]]=m[1]
 def thread(m):
  target=m[2];seen=set()
  while target in jumps and target not in seen:seen.add(target);target=jumps[target]
  if target in seen:return m[0]
  if target!=m[2]:count['branch_thread']+=1
  return '\t'+m[1]+' '+target+'\n'
 text=re.sub(r'^\t(b(?:\.[a-z]+)?) (L\w+)\n',thread,text,flags=re.M)
 return text,dict(count)

def fp(text):
 count=collections.Counter()
 def patch(m):
  block=m[0]
  calls=re.findall(r'\tbl (_\w+)',block)
  if len(calls)!=1 or not re.fullmatch(r'_(?:allocate\w*Region|deallocate\w*Region)',calls[0]):return block
  block,n=re.subn(r'^\t(?:stp|ldp) d\d+, d\d+, \[sp, #\d+\]\n','',block,flags=re.M)
  count['FP_save_restore_instructions_removed']+=n;count['region_call_sites']+=1
  return block
 text=re.sub(r'\tsub sp, sp, #304\n.*?\tadd sp, sp, #304\n',patch,text,flags=re.S)
 return text,dict(count)

# Move an independent stack load up by one instruction. No control-flow, flag,
# heap-access or SP-changing instruction may be crossed. All registers retained.
def schedule(text):
 lines=text.splitlines(True);n=0
 for i in range(1,len(lines)):
  m=re.fullmatch(r'\tldr (x\d+), \[sp, #(\d+)\]\n',lines[i])
  if not m:continue
  dst,off=m[1],int(m[2]); prev=lines[i-1]
  p=re.fullmatch(r'\t(mov|add|sub|and|orr|eor|lsl|lsr|asr) (x\d+), (x\d+)(?:, (#-?\d+|x\d+))?\n',prev)
  if not p:continue
  if dst in re.findall(r'\bx\d+\b',prev):continue
  lines[i-1],lines[i]=lines[i],lines[i-1];n+=1
 return ''.join(lines),{'independent_stack_loads_hoisted_one_instruction':n}

stats={}
for name,row in links.items():
 for variant,transform in [('peephole',micro),('schedule',schedule)]+([('region_fp',fp)] if name=='professor' else []):
  folder=root/(name+'-'+variant);folder.mkdir(exist_ok=True);args=list(row['args']);args[1]=str(root/(name+'-'+variant+'.exe'))
  counts=collections.Counter(); changed=0;before=after=0
  for i,a in enumerate(args):
   if not a.endswith('.o'):continue
   obj=Path(a);obj=obj if obj.is_absolute() else Path(row['cwd'])/obj
   # Patch benchmark units only; leave freshly rebuilt Basis/runtime identical.
   if not str(obj).startswith(str(root/'bench')) or obj.name=='base-link_objects.o':continue
   asm=obj.with_suffix('.s');original=asm.read_text();rewritten,c=transform(original)
   if rewritten==original:continue
   dest=folder/(str(i)+'-'+asm.name);dest.write_text(rewritten)
   out=dest.with_suffix('.o')
   subprocess.run(['gcc','-arch','arm64','-c',str(dest),'-o',str(out)],check=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
   args[i]=str(out);counts.update(c);changed+=1
   before+=len(re.findall(r'^\t\w',original,re.M));after+=len(re.findall(r'^\t\w',rewritten,re.M))
  subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  stats[name+'-'+variant]={'counts':dict(counts),'changed_units':changed,'instructions_before_in_changed_units':before,'instructions_after_in_changed_units':after}
  print(name,variant,stats[name+'-'+variant],flush=True)
(root/'experiments.json').write_text(json.dumps(stats,indent=2)+'\n')
