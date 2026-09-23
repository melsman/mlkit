from pathlib import Path
import subprocess,json,statistics,random
r=Path('/private/tmp/mlkit-msort');rows={a:[] for a in ['arm64','x64']};rng=random.Random(223)
for rep in range(6):
 order=list(rows);rng.shuffle(order)
 for a in order:
  p=subprocess.run([str(r/(a+'-phase.exe')),'-report_gc'],capture_output=True,text=True,check=True)
  assert p.stdout==Path('/private/tmp/mlkit-gc20-current/bench/benchmarks/msort.sml.out.ok').read_text()
  marks=[line.split() for line in p.stderr.splitlines() if line.startswith('PHASE')];assert len(marks)==3
  vals=[]
  for i in range(2):
   x,y=marks[i:i+2];vals.append({'phase':['generate','sort'][i],'seconds':float(y[2])-float(x[2]),'collections':int(y[3])-int(x[3]),'gc_ms':(int(y[4])-int(x[4]))/10,'rss_peak_bytes':int(y[5])})
  if rep: rows[a].append(vals)
(r/'phases.json').write_text(json.dumps(rows,indent=2)+'\n')
for a,runs in rows.items():
 for i in range(2):print(a,runs[0][i]['phase'],{k:statistics.median(run[i][k] for run in runs) for k in ['seconds','collections','gc_ms','rss_peak_bytes']})
