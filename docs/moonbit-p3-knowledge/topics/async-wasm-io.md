# `moonbitlang/async` Wasm I/O

Status: separate investigating topic

This topic is not the Component Model or `wit-bindgen`. It concerns
`moonbitlang/async` compiled to core Wasm and run through moonrun's custom
`moonbit_v0` ABI.

## Current answer

The main confirmed large-output issue was I/O granularity:

- a 1 KiB Sender produced about 65 writes for a 64 KiB response;
- changing it to 16 KiB reduced writes to about 5;
- native, V8, and Wasmtime-core CPU/request all fell by roughly 85–88%.

Replacing V8 with Wasmtime while preserving the same guest, AsyncHost, custom
ABI, and syscalls did not improve server throughput. It reduced warmed physical
footprint from roughly 34.3 MiB to 23.2 MiB.

Therefore:

- Sender/direct-body batching precedes engine micro-optimization;
- Reader owned-copy, yield frequency, RC, and compiler lowering still need
  separate attribution;
- async custom-ABI results must not be used to explain WASI P3 binding costs.

This topic is retained here only to prevent future work from mixing the two
server stacks.
