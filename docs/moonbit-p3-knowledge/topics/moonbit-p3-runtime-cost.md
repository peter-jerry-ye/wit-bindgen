# MoonBit P3 residual runtime cost

Status: investigating

## Question

After byte lowering and operation shape are controlled, why does generated
MoonBit still use more CPU and instructions per request than Rust on the same
WIT and Wasmtime host?

## Current answer

The measured one-operation path leaves roughly 0.43–0.44 million additional
instructions per MoonBit request. This is not Component Model mandatory work.

Sampling profiles identify candidates, not exclusive percentages:

- RC/object metadata;
- allocation and GC;
- coroutine frames and closures;
- scheduler/waitable/subscriber machinery;
- inlined work attributed to large generated wrappers.

Enum/tag decode was below 1% in the optimized path and is not the first target.
Source-map sampling cannot safely split the large wrapper bucket.

## Next evidence

Add low-overhead per-request counters for:

```text
coroutine frames allocated/freed
closure allocations
incref/decref
allocator bytes/high-water
waitable registrations/removals
subscriber map/set operations
callbacks delivered
task-group nodes
```

Promote a candidate into an optimization task only after:

```text
source mechanism → dynamic count → single-variable A/B → CPU/instructions
```

Do not redesign the MoonBit async runtime based only on sampling symbol names.
