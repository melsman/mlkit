from pathlib import Path
import re,json,subprocess
r=Path('/private/tmp/mlkit-m12-residual')
row=next(row for row in map(json.loads,(r/'links.jsonl').read_text().splitlines()) if 'professor' in row['args'][1])
args=list(row['args']);args[1]=str(r/'professor-countloop.exe')
i=next(i for i,a in enumerate(args) if a.endswith('/professor.sml.o'))
obj=Path(row['cwd'])/args[i];text=obj.with_suffix('.s').read_text()
m=re.search(r'^(_F\.count37_[^:\n]+):\n(.*?)(?=^\.text\n)',text,re.M|re.S);assert m
body=m[2]
assert body.startswith('\tstp x29, x30, [sp, #0]\n\tadd x29, sp, #0\n')
assert body.endswith('\tldp x29, x30, [sp, #0]\n\tadd sp, sp, #16\n\tret \n')
assert '\tbl ' not in body and '_time_to_gc' not in body
assert '\tldr x16, [x16, #16]\n\tmov x1, x16\n' in body
assert '\tsubs x2, x2, x17\n' in body and '[x16, #8]' in body
new=m[1]+''':
\tstp x29, x30, [sp, #0]
\tadd x29, sp, #0
L_study_count_loop:
\ttbnz x1, #0, L_study_count_empty
\tcmp x2, #1
\tb.eq L_study_count_found
\tldr x1, [x1, #16]
\tsubs x2, x2, #2
\tb.vc L_study_count_loop
\tadrp x1, _exn_OVERFLOW@GOTPAGE
\tldr x1, [x1, _exn_OVERFLOW@GOTPAGEOFF]
\tmov x0, x28
\tb _raise_exn
L_study_count_found:
\tldr x0, [x1, #8]
\tldp x29, x30, [sp, #0]
\tadd sp, sp, #16
\tret
L_study_count_empty:
\tldr x1, [x0, #8]
\tmov x0, x28
\tb _raise_exn
'''
p=r/'professor-countloop.s';p.write_text(text[:m.start()]+new+text[m.end():]);args[i]=str(p.with_suffix('.o'))
subprocess.run(['gcc','-arch','arm64','-c',str(p),'-o',args[i]],check=True)
subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)
(r/'professor-count-before.s.txt').write_text(m[0]);(r/'professor-count-after.s.txt').write_text(new)
