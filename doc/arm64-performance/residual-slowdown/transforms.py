import re,collections
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
