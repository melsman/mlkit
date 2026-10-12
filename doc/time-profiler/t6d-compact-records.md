# T6d: compact passive records

[T7](t7-parallel-sampling.md) adds static macOS ARM64 no-GC pthread sampling,
per-thread compact buffers and thread/stream views. Earlier parallel rejection
statements below describe the pre-T7 implementation.

Passive sampling buffers only a 64-bit interrupted PC and 64-bit Mach timestamp.
The record is 16 bytes. Detailed sampling retains state and initiating C-origin
PC in a 32-byte record. Static assertions verify both sizes on the supported ABI.
The layout is selected before handler installation and stays fixed for the
session. The passive handler never reads or stores attribution state/origin.

The default memory budget remains **2 MiB**: 131,072 passive records or 65,536
detailed records. `-tp_buffer N` still means an explicit record count, with the
existing 2..1,048,576 range. Explicit capacities are not silently doubled;
the maximum override therefore consumes 16 MiB passive or 32 MiB detailed.
No buffer is allocated while sampling is disabled.

At 1ms the default passive buffer holds about 131 seconds of nominal delivery;
at 100us about 13 seconds. This is capacity between drains, not a guarantee of
signal delivery. Explicit flush/lifecycle operations and normal shutdown drain
with delivery masked. Overflow retains early records and counts later drops.

The `.rp` wire format and pview reader stay compatible: drains expand passive
records to the existing time-sample fields with state/origin both zero.
Serialized samples still cost 61 bytes. The existing time-session semantics
text describes the selected in-memory layout; its capacity is the actual
record capacity. Full PCs and exact clock ticks are retained without narrowing,
packing, timestamp wraparound or code-range encoding in the handler.

Ordinary builds automatically use compact records. Existing instrumented
single-thread `-rp` builds retain detailed recording by default. Both layouts
use the existing runtime archives. See [T6c](t6c-optional-attribution.md) for
explicit attribution selection, instrumentation costs and matching IR links.

Validation covers ordinary GC/GenGC/no-GC defaults, PC attribution and matching
IR, explicit small-buffer overflow, shutdown/lifecycle draining and stripped
metadata, plus detailed lifecycle/overflow/combined recording and old wire
compatibility. The [T6c measurements](t6c-optional-attribution.md) compare both
layouts at the same default memory budget and retain wall/CPU time, RSS,
capacity, capture and loss observations. No claim of faster delivery follows
from the smaller record alone.
