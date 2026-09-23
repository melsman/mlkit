#!/usr/bin/env python3
"""Stage CI diagnostics without traversing regression-test symlinks."""
import os
from pathlib import Path
import shutil
import sys

root = Path(sys.argv[1])
destination = root / "reports"
suffixes = {".log", ".html", ".res", ".mlbres", ".out", ".err"}
names = {"TESTmessages", "complog.txt"}
for directory in (root / "logs", root / "tmp"):
    for current, dirs, files in os.walk(directory, followlinks=False):
        dirs[:] = [name for name in dirs if not (Path(current) / name).is_symlink()]
        for name in files:
            source = Path(current) / name
            if source.is_symlink() or not source.is_file():
                continue
            if directory.name != "logs" and source.suffix not in suffixes and name not in names:
                continue
            target = destination / source.relative_to(root)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
config = Path("config.log")
if config.is_file():
    destination.mkdir(parents=True, exist_ok=True)
    shutil.copy2(config, destination / config.name)
