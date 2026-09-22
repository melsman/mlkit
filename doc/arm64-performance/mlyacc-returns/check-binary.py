from pathlib import Path
import struct,json
root=Path('/private/tmp/mlkit-m12-mlyacc-study')
def text_section(path):
    data=path.read_bytes()
    count=struct.unpack_from('<I',data,16)[0]
    offset=32
    for _ in range(count):
        command,size=struct.unpack_from('<II',data,offset)
        if command==0x19:
            for index in range(struct.unpack_from('<I',data,offset+64)[0]):
                section=offset+72+index*80
                if data[section:section+16].rstrip(b'\0')==b'__text':
                    length=struct.unpack_from('<Q',data,section+40)[0]
                    position=struct.unpack_from('<I',data,section+48)[0]
                    return data[position:position+length]
        offset+=size
    raise ValueError('No __text section')
baseline=text_section(root/'mlyacc-baseline.exe')
results={}
for variant in ['brreturns','brunions','brcomparisons']:
    candidate=text_section(root/('mlyacc-'+variant+'.exe'))
    assert len(baseline)==len(candidate)
    changes=[(baseline[i:i+4].hex(),candidate[i:i+4].hex())
             for i in range(0,len(baseline),4) if baseline[i:i+4]!=candidate[i:i+4]]
    assert set(changes)=={('c0035fd6','c0031fd6')},set(changes)
    results[variant]={'text_bytes':len(baseline),'changed_instructions':len(changes),
                      'only_change':'ret -> br x30; text length, addresses and all other instructions identical'}
(root/'binary-check.json').write_text(json.dumps(results,indent=2)+'\n')
print(json.dumps(results,indent=2))
