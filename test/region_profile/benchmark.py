#!/usr/bin/env python3
"""Compare a plain and an instrumented periodic.sml executable; report medians."""
import json
import os
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile
import time
plain, instrumented = sys.argv[1:]
with tempfile.TemporaryDirectory(prefix='rp-bench-') as directory:
    for label,exe,flags in [
        ('plain',plain,[]),('instrumented-disabled',instrumented,[]),
        ('paused',instrumented,['-rp','-rp_paused']),
        *[(interval,instrumented,['-rp','-rp_interval',interval]) for interval in ['1ms','10ms','100ms']]]:
        elapsed=[]
        report=''
        output=Path(directory)/'profile.rp'
        for _ in range(7):
            args=[exe,*flags]
            if flags: args+=['-rp_file',str(output),'-rp_report']
            start=time.perf_counter()
            result=subprocess.run(args,capture_output=True,text=True,check=True,
                                  env=dict(os.environ,RP_ITERATIONS='1000000000'))
            elapsed.append(time.perf_counter()-start)
            report=result.stderr.strip()
        print(json.dumps(dict(mode=label,median_seconds=statistics.median(elapsed),
                              bytes=output.stat().st_size if flags else 0, report=report)),flush=True)
