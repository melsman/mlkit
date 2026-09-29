#!/usr/bin/env python3
"""Exercise the runtime's local control socket; PROGRAM is an instrumented loop."""
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import time

with tempfile.TemporaryDirectory(prefix="rp-live-", dir="/tmp") as directory:
    base = Path(directory)
    endpoint = base/"control.sock"
    profile = base/"live.rp"
    process = subprocess.Popen([sys.argv[1], "-rp", "-rp_paused", "-rp_interval", "0",
                                "-rp_control", str(endpoint), "-rp_file", str(profile)],
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               env=dict(os.environ, RP_ITERATIONS="1000000000"))
    try:
        for _ in range(200):
            if endpoint.exists():
                break
            if process.poll() is not None:
                raise RuntimeError(process.communicate()[1].decode())
            time.sleep(.01)
        with socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM) as client:
            for command in ("sample", "start", "pause", "sample", "flush"):
                client.sendto(command.encode(), str(endpoint))
                time.sleep(.02)
        out, err = process.communicate(timeout=20)
        assert process.returncode == 0, err.decode()
        records = [json.loads(line) for line in profile.read_text().splitlines()]
        assert [r["reason"] for r in records if r["type"] == "sample_begin"] == ["explicit", "start", "pause", "explicit"]
        assert not endpoint.exists()
        print("Live controls and socket cleanup passed")
    finally:
        if process.poll() is None:
            process.kill()
            process.wait()
