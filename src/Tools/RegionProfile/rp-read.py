#!/usr/bin/env python3
"""Read complete region snapshots; summarize them as CSV or JSON.

The JSON-lines stream has unsigned 64-bit byte/nanosecond counters. Readers
must preserve integer precision. A sample is committed by its sample_end;
truncated final lines and unfinished samples are ignored. Other malformed
records are errors. Repeated (unit,binding,thread) records are recursive
instances and are summed, not overwritten.
"""
import argparse
import csv
import json
import sys


def read_samples(stream):
    header = None
    pending = None
    regions = []
    for number, line in enumerate(stream, 1):
        if not line.endswith("\n"):
            break  # writer has not committed this last record yet
        try:
            record = json.loads(line)
        except (ValueError, TypeError) as exc:
            raise ValueError(f"line {number}: invalid JSON") from exc
        if not isinstance(record, dict):
            raise ValueError(f"line {number}: expected a record")
        kind = record.get("type")
        if header is None:
            if kind != "header" or record.get("format") != "mlkit-region-profile" or record.get("version") not in (1, 2):
                raise ValueError("expected a version 1 or 2 mlkit-region-profile header")
            header = record
        elif kind == "sample_begin":
            if pending is not None:
                raise ValueError("nested samples")
            pending, regions = record, []
        elif kind in ("region", "sample_end"):
            if pending is None or record.get("sample") != pending.get("sample"):
                raise ValueError("record outside its sample")
            if kind == "region":
                for key in ("pages", "unused_tail", "page_footprint", "large_bytes", "finite_bytes", "descriptor_bytes"):
                    v = record.get(key)
                    if type(v) is not int or not 0 <= v < 2**64:
                        raise ValueError(f"invalid uint64 field {key}")
                if record["page_footprint"] != record["pages"]*header["page_bytes"]-record["unused_tail"]:
                    raise ValueError("inconsistent page accounting")
                if "g0_pages" in record:
                    for key in ("g0_pages", "g1_pages", "g0_unused_tail", "g1_unused_tail"):
                        v = record.get(key)
                        if type(v) is not int or not 0 <= v < 2**64:
                            raise ValueError(f"invalid uint64 field {key}")
                    if record["pages"] != record["g0_pages"]+record["g1_pages"] or record["unused_tail"] != record["g0_unused_tail"]+record["g1_unused_tail"]:
                        raise ValueError("inconsistent generation accounting")
                regions.append(record)
            else:
                yield {**pending, "end_time": record["time"], "frames": record["frames"],
                       "pages_visited": record["pages_visited"], "cache_bytes": record.get("cache_bytes",0), "regions": regions}
                pending, regions = None, []
        elif kind not in ("mark", "thread_start", "thread_end", "binding", "session_end", "sample_skipped"):
            raise ValueError(f"unknown record type {kind!r}")
    if header is None:
        raise ValueError("missing profile header")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", nargs="?", default="profile.rp")
    parser.add_argument("--json", action="store_true", help="emit complete samples, including all region instances")
    args = parser.parse_args()
    fields = ["sample", "time_ns", "reason", "frames", "pages_visited", "page_footprint", "large_bytes", "finite_bytes", "descriptor_bytes"]
    writer = csv.DictWriter(sys.stdout, fieldnames=fields)
    if not args.json:
        writer.writeheader()
    try:
        with open(args.file, encoding="utf-8") as stream:
            for sample in read_samples(stream):
                if args.json:
                    print(json.dumps(sample))
                else:
                    row = {k: sample[k] for k in ("sample", "reason", "frames", "pages_visited")}
                    row["time_ns"] = sample["time"]
                    for key in fields[5:]:
                        row[key] = sum(r[key] for r in sample["regions"])
                    writer.writerow(row)
    except (OSError, ValueError, KeyError, TypeError) as exc:
        parser.exit(1, f"rp-read: {exc}\n")


if __name__ == "__main__":
    main()
