#!/usr/bin/env python3
"""CLI SVG rendering and HTML defaults, with no runtime PATH dependencies."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import xml.etree.ElementTree as ET

root = Path(__file__).resolve().parents[2]
viewer = os.environ.get('RPVIEW', str(root/'bin/rpview'))
ns = {'s':'http://www.w3.org/2000/svg'}
with tempfile.TemporaryDirectory(prefix='rp-svg-') as directory:
    folder = Path(directory)
    profile = folder/'profile.rp'
    rows = [dict(type='header',format='mlkit-region-profile',version=3,page_bytes=8192,
                 main_source='/source/main.sml',gc_enabled=True)]
    for sample,time in [(1,1000000),(2,2000000)]:
        rows.append(dict(type='sample_begin',sample=sample,time=time,reason='explicit'))
        for binding,thread,amount in [(1,1,2**60+1),(2,2,1024),(3,1,2048)]:
            rows.append(dict(type='region',sample=sample,thread=thread,worker=thread-1,cpu=thread+3,
                             unit='unit',source='/source/test.sml',binding=binding,name='',
                             kind='infinite',region_type='pair',pages=1,unused_tail=4096,
                             page_footprint=4096,large_bytes=amount,finite_bytes=0,descriptor_bytes=64))
        rows.append(dict(type='stack',sample=sample,thread=1,worker=0,cpu=4,
                         active_bytes=64,finite_bytes=0,stack_bytes=64))
        rows.append(dict(type='sample_end',sample=sample,time=time+1,frames=1,pages_visited=3))
    rows.append(dict(type='session_end',time=3000000,max_pages=4,gc_collections=17))
    profile.write_text(''.join(json.dumps(r)+'\n' for r in rows))
    output = folder/'graph.svg'
    def run(*args, ok=True):
        result = subprocess.run([viewer,str(profile),'-o',str(output),*args],text=True,
                                capture_output=True,env=dict(os.environ,PATH='/nonexistent'))
        assert (result.returncode==0)==ok, (args,result.stdout,result.stderr)
        return result
    def svg(*args):
        run(*args)
        node = ET.parse(output).getroot()
        assert node.tag.endswith('svg')
        assert not node.findall('.//s:script',ns) and not node.findall('.//s:foreignObject',ns)
        assert 'ML stack band' not in ''.join(node.itertext())
        assert all('NaN' not in str(n.attrib) and 'Infinity' not in str(n.attrib) for n in node.iter())
        return node
    def polygons(node): return node.findall('.//s:polygon',ns)
    def text(node): return ''.join(node.itertext())
    graph = svg()
    assert len(polygons(graph))==4
    assert len({p.get('fill') for p in polygons(graph)})==4
    assert 'Region profile for main.sml (GC enabled)' in text(graph)
    assert 'Garbage collections: 17' in text(graph) and 'Memory (EiB)' in text(graph)
    assert float(graph.get('width'))<1440
    colors = {p.get('data-band'):p.get('fill') for p in polygons(graph)}
    for scope in ['thread:1','worker:0','cpu:4']:
        filtered = svg('--scope',scope)
        assert len(polygons(filtered))==3
        assert all(colors[p.get('data-band')]==p.get('fill') for p in polygons(filtered))
    graph = svg('--regions','1','--show-base','--show-kind','--show-type')
    assert len(polygons(graph))==3 and 'Other (2 regions)' in text(graph)
    assert 'test.sml' in text(graph) and 'infinite' in text(graph) and 'pair' in text(graph)
    for metric in ['pages','page_footprint','large_bytes','finite_bytes','descriptor_bytes']:
        graph = svg('--metric',metric,'--regions','0')
        assert len(polygons(graph))==3
    graph = svg('--metric','pages','--show-peak')
    assert 'Peak page capacity: 32.00 KiB' in text(graph)
    assert graph.find('.//s:line',ns) is not None
    graph = svg('--metric','page_footprint','--show-peak','--scope','thread:1')
    assert graph.find('.//s:line',ns) is None
    caption = 'Custom </script> <&> "caption" __DATA__ __META__ __OPTIONS__'
    graph = svg('--caption',caption)
    assert graph.find('s:title',ns).text==caption
    run('--format','html','--caption',caption,'--regions','2','--show-base','--hide-kind',
        '--show-type','--show-peak','--legend-below','--metric','pages','--group','region','--scope','thread:1')
    html = output.read_text()
    defaults = json.loads(html.split('const defaults=')[1].split(';\n')[0])
    assert defaults=={'caption':caption,'limit':2,'show-base':True,'show-kind':False,'show-type':True,
                      'show-peak':True,'legend-right':False,'metric':'pages','group':'region','scope':'thread:1'}
    for args in [('--regions','-1'),('--metric','bad'),('--scope','core:1'),('--scope','thread:999'),('--format','pdf'),('--caption',)]:
        run(*args,ok=False)
    run('--output',str(profile),ok=False)
    # A single point is drawn with positive width; an empty stream is a clear error.
    profile.write_text(''.join(json.dumps(r)+'\n' for r in rows if r.get('sample',1)==1))
    graph = svg()
    assert 'single snapshot' in text(graph)
    profile.write_text(json.dumps(rows[0])+'\n')
    assert 'no completed snapshots' in run(ok=False).stderr
print('Direct SML SVG: filters, aggregation, colors, units, captions, options, escaping, empty/single profiles passed')
