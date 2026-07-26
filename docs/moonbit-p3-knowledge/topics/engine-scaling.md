# Engine supply and horizontal scaling

Status: investigating

## Question

When does adding Store/instance workers, V8 isolates, threads, or processes
increase completed throughput?

## Current model

For a stable CPU-bound interval:

```text
throughput ≈ average runnable cores / CPU seconds per successful request
```

The real ceiling is the minimum of admission, runnable guest capacity, serial
host stages, memory/network bandwidth, and backpressure policy.

Engine count alone is not a performance metric.

## Wasmtime P3

The measured Wasmtime version did not use one instance per request by default:

- a live instance could serve 128 requests over its lifetime;
- up to 16 unfinished guest calls could coexist in one Store event loop;
- those calls were cooperatively multiplexed, not run on 16 cores;
- multi-core guest execution came from multiple Store+instance workers.

Forcing Rust per-instance concurrent reuse to one increased live instance count
without breaking its throughput plateau. More instances were not sufficient.

## V8

Current moonrun executes the guest HTTP server in one isolate. V8 background
threads do not make that isolate's handler multi-core.

A feasible worker is:

```text
one OS thread
  + one isolate
  + one Wasm instance
  + one AsyncHost/event loop
  + one bounded mailbox
```

V8 compiled-module sharing makes this architecture possible, but an in-process
multi-isolate server has not been benchmarked. Multi-process regression cannot
answer that question because it repeats platforms/modules and also reproduced
similar scaling limits in the native control.

This topic is independent of the first `stream<u8>` lowering change.
