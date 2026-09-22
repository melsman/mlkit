from pathlib import Path
import re,difflib,json
root=Path('/private/tmp/mlkit-m12-typed');before=root/'before-1';after=root/'after-1';rows=[]
def normalise(text):
 lines=[]
 for line in text.splitlines():
  line=re.sub(r'\[(sp|[xw][0-9]+)\]',r'[\1, #0]',line)
  line=re.sub(r'#0x([0-9a-fA-F]+)',lambda m:'#'+str(int(m[1],16)),line)
  lines.append(re.sub(r'\s+','',line))
 return lines
for old in before.rglob('*.s'):
 rel=old.relative_to(before);new=after/rel
 if not new.exists():continue
 a=normalise(old.read_text());b=normalise(new.read_text());same=a==b
 row={'file':str(rel),'same_after_format_normalisation':same}
 if not same:
  diff=list(difflib.unified_diff(a,b,fromfile='before',tofile='after'))
  (root/(old.name+'.diff')).write_text('\n'.join(diff)+'\n');row['diff_lines']=len(diff)
 rows.append(row)
print(json.dumps(rows,indent=2));(root/'assembly-comparison.json').write_text(json.dumps(rows,indent=2)+'\n')
