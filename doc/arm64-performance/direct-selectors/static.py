from pathlib import Path
import re,json
results={}
for v,root,cache in [('before',Path('/private/tmp/mlkit-m12-return-fix/after'),'M12RetAfter'),('after',Path('/private/tmp/mlkit-m12-direct/after'),'M12DirectFresh')]:
 results[v]={}
 for name,folder in [('nucleic',root/'nucleic'),('mlyacc',root/'bench/mlyacc'),('professor',root/'bench')]:
  files=[p for p in folder.rglob('*.s') if cache in str(p) and (name!='professor' or p.name=='professor.sml.s')]
  assert files,(v,name)
  s='\n'.join(p.read_text() for p in files)
  results[v][name]={'files':len(files),'instructions':len(re.findall(r'^\t[a-z]',s,re.M)),'scratch_copy_before_cmp':len(re.findall(r'\tmov (x1[67]), x\d+\n\tcmp \1,',s))}
r=Path('/private/tmp/mlkit-m12-direct');(r/'static-counts.json').write_text(json.dumps(results,indent=2)+'\n');print(json.dumps(results))
