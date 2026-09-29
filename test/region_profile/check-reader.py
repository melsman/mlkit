#!/usr/bin/env python3
import importlib.util
import io
import json
import pathlib
import sys

root = pathlib.Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("rp_reader", root / "test/region_profile/reference-reader.py")
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)


def load(path):
    with open(path) as f:
        return list(reader.read_samples(f))


def totals(sample, field):
    return sum(r[field] for r in sample["regions"])


mode, path = sys.argv[1:]
samples = load(path)
if mode == "runtime":
    assert len(samples) == 4
    assert [s['stacks'][0]['active_bytes'] for s in samples] == [1056,512,512,512]
    assert [s['stacks'][0]['stack_bytes'] for s in samples] == [1008,488,488,488]
    assert {r['binding']:r['region_type'] for r in samples[0]['regions']} == {11:'pair',12:'string',13:'bot'}
    first = samples[0]
    assert first["frames"] == 2 and first["pages_visited"] == 2
    assert totals(first, "page_footprint") == 8264
    assert totals(first, "large_bytes") == 4096
    assert totals(first, "finite_bytes") == 48
    assert [s["reason"] for s in samples] == ["explicit", "start", "pause", "explicit"]
    for sample in samples[1:]:
        assert totals(sample, "page_footprint") == 16
        assert totals(sample, "finite_bytes") == 24
        assert totals(sample, "large_bytes") == 0
    text = pathlib.Path(path).read_text()
    lines = text.splitlines(keepends=True)
    # Drop the final end record: unfinished snapshot must not be reported.
    last_end = max(i for i,line in enumerate(lines) if json.loads(line)["type"] == "sample_end")
    assert len(list(reader.read_samples(io.StringIO("".join(lines[:last_end]))))) == 3
    assert len(list(reader.read_samples(io.StringIO(text + '{"type":')))) == 4
    try:
        list(reader.read_samples(io.StringIO(text + "invalid\n")))
    except ValueError:
        pass
    else:
        raise AssertionError("malformed committed records must be rejected")
    # Exercise integer counters beyond IEEE-754's exact range.
    records = [json.loads(line) for line in lines]
    for r in records:
        if r["type"] == "region":
            r["large_bytes"] = 2**60 + 1
    parsed = list(reader.read_samples(io.StringIO("".join(json.dumps(r)+"\n" for r in records))))
    assert parsed[0]["regions"][0]["large_bytes"] == 2**60 + 1
elif mode == "regions":
    assert {r['region_type'] for r in samples[0]['regions'] if r['unit']=='<global>'} == {'top','string','pair','array','ref','triple'}
    assert all(r['region_type']!='unavailable' for s in samples for r in s['regions'])
    assert len(samples) == 9, len(samples)
    assert [totals(s, "finite_bytes") for s in samples[:7]] == [32,16,16,16,0,48,32]
    assert [totals(s, "large_bytes") for s in samples] == [24008,24008,0,0,0,0,0,0,0]
    assert [totals(s, "page_footprint") for s in samples[:5]] == [112,112,112,144,96]
    assert samples[0]["frames"] >= 3 and samples[5]["frames"] >= 4
    local_pages = [r for r in samples[7]["regions"] if r["unit"] != "<global>" and r["kind"] == "infinite"]
    assert len(local_pages) == 1 and local_pages[0]["pages"] == 2, local_pages
    assert local_pages[0]["page_footprint"] == 8192+16+601*8
    # The 12-word result tuple is reserved in the caller before it is filled.
    assert totals(samples[8], "finite_bytes") == 112
    assert samples[8]["frames"] >= 3
elif mode == "api":
    assert [s["reason"] for s in samples] == ["start","explicit","pause","explicit"]
    marks = [json.loads(line) for line in pathlib.Path(path).read_text().splitlines() if '"type":"mark"' in line]
    assert marks[0]["label"] == 'quote" slash\\ newline\n nul\x00tail'
else:
    raise AssertionError(mode)
print(f"{mode}: accounting and stream checks passed")
