from pathlib import Path
import json,re
r=Path('/private/tmp/mlkit-m12-return-fix');results={}
for variant,cache in [('before','M12RetBefore'),('after','M12RetAfter')]:
 files=list((r/variant).rglob('*.s'))+list(Path('/private/tmp/mlkit-m2-source/basis').glob('MLB/*'+cache+'/*.s'))
 counts={'functions':0,'ret':0,'br_x30':0}
 for p in files:
  if cache not in str(p):continue
  for m in re.finditer(r'^_F\.[^:\n]+:\n(.*?)(?=^\.text\n|\Z)',p.read_text(),re.M|re.S):
   counts['functions']+=1
   counts['ret']+=len(re.findall(r'^\tret\s*$',m[1],re.M))
   counts['br_x30']+=len(re.findall(r'^\tbr x30\s*$',m[1],re.M))
 assert counts['functions']>0
 assert (counts['ret']==0 and counts['br_x30']>0) if variant=='after' else (counts['ret']>0 and counts['br_x30']==0)
 results[variant]=counts
(r/'return-check.json').write_text(json.dumps(results,indent=2)+'\n');print(results)
