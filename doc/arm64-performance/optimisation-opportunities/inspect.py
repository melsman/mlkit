#!/usr/bin/env python3
"""Count syntactic opportunities in retained ARM64 .s files; not an optimiser."""
import hashlib
import json
from pathlib import Path
import re
import sys

root = Path(sys.argv[1])
rows = []
for path in sorted(root.rglob('*.s')):
    if path.name == 'base-link_objects.s':
        continue
    lines = path.read_text().splitlines()
    row = {
        'file': str(path.relative_to(root)),
        'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
        'instructions': sum(bool(re.match(r'^\s+[a-z][a-z0-9.]*\s', s)) for s in lines),
        'expanded_conditional_branches': sum(s.startswith('L_mlkit_branch_skip_')
                                            and s.endswith(':') for s in lines),
        'constant_sequences': 0,
        'removable_zero_movk': 0,
        'unconditional_branch_to_next_label': 0,
    }
    for i, line in enumerate(lines):
        match = re.fullmatch(r'\s*movz\s+(x\d+), #\d+, lsl #0', line)
        if match and i + 3 < len(lines):
            chunks = [re.fullmatch(r'\s*movk\s+' + match[1]
                                  + r', #(\d+), lsl #' + str(k * 16), lines[i + k])
                      for k in range(1, 4)]
            if all(chunks):
                row['constant_sequences'] += 1
                row['removable_zero_movk'] += sum(int(chunk[1]) == 0 for chunk in chunks)
        match = re.fullmatch(r'\s*b\s+(\S+)\s*', line)
        if match and i + 1 < len(lines) and lines[i + 1].strip() == match[1] + ':':
            row['unconditional_branch_to_next_label'] += 1
    rows.append(row)
print(json.dumps(rows, indent=2))
