# Evidence summary

Status: measured research snapshot

Measurement date: 2026-07-23 through 2026-07-25

This file records the smallest set of measurements needed to understand the
next task. It does not claim that the benchmark machine represents production
hardware or that one workload determines a universal window.

## Benchmark contract

- cleartext HTTP/1.1 keep-alive on same-host loopback;
- no TLS, HTTP/2, compression, filesystem, or outbound network;
- server CPU and retired instructions include the whole server PID;
- the key response workload is one 64 KiB deterministic body;
- body length, status, success rate, and content hash were checked;
- absolute RPS is meaningful only for same-machine A/B comparisons.

## The original 64-element problem

For a 64 KiB upload plus 64 KiB response:

| Implementation | CPU/request | Instructions/request | p99 | Saturated RPS |
|---|---:|---:|---:|---:|
| MoonBit 64 B | 25.14 ms | 109.97M | 385.76 ms | 488 |
| MoonBit 4 KiB | 2.23 ms | 13.30M | 2.23 ms | 11,931 |
| Rust natural chunking | 1.19 ms | 2.40M | 1.46 ms | 10,275 |
| Rust forced to 64 B | 22.90 ms | 52.37M | 397.49 ms | 398 |

The MoonBit 64 B→4 KiB and Rust natural→64 B controls changed in opposite
directions with the same performance shape. This is strong evidence that the
operation window, not the guest language alone, caused the original
order-of-magnitude regression.

Upstream #1664 now uses an approximately 4 KiB byte budget for fixed primitive
streams, so the original universal 64-element problem is no longer the next
task on `main`.

## Scalar lowering versus operation count

Equivalent 64 KiB successful response paths:

| Path | CM operations | Single CPU/request | Saturated CPU/request | Saturated RPS |
|---|---:|---:|---:|---:|
| generated scalar, 4 KiB | 16 | 545.7 µs | 624.7 µs | 12,127 |
| intrinsic bulk copy, 4 KiB | 16 | 327.9 µs | 532.8 µs | 12,399 |
| intrinsic snapshot, 64 KiB | 1 | 100.2 µs | 161.0 µs | 46,371 |
| intrinsic static borrow, 64 KiB | 1 | 99.6 µs | 156.0 µs | 46,456 |

Interpretation:

- scalar→bulk dramatically reduces CPU and instructions;
- keeping 16 canonical operations leaves saturated RPS almost unchanged;
- reducing 16 operations to one changes the host pipeline ceiling;
- snapshot and static borrow are nearly identical for CPU/throughput;
- zero-copy is not required to recover the measured byte-path performance.

The safe snapshot used more concurrent memory. At 64 concurrent 64 KiB
responses, snapshot RSS was about 5.3 MiB above static borrow; 4 MiB of that is
the payload snapshots themselves.

## Generated path versus successful intrinsic

A complete generated MoonBit prototype with bulk byte lowering and a 64 KiB
window retained the generic producer, sink callback, mutex, maps, trailers,
transmission future, and generated lifecycle.

| Policy | Generated CPU/request | Intrinsic snapshot | Gap |
|---|---:|---:|---:|
| single | 102.8 µs | 100.2 µs | +2.6% |
| saturated | 163.2 µs | 161.0 µs | +1.3% |

Instructions/request differed by about 5.4%; saturated RPS differed by about
0.5%. Therefore deleting the generic bridge cannot explain the old regression.

Important limitation: the handwritten intrinsic's active-cancellation branch
did not implement the complete `cancel-write → terminal(progress) →
settlement` lifecycle. It is a successful-path performance lower bound, not a
production replacement.

## Generated MoonBit versus Rust

Same-run one-operation 64 KiB comparison:

| Policy | MoonBit generated | Rust | MoonBit relative |
|---|---:|---:|---:|
| single CPU/request | 102.8 µs | 70.4 µs | +46.0% |
| single instructions/request | 0.911M | 0.467M | +95.2% |
| saturated CPU/request | 163.2 µs | 132.1 µs | +23.5% |
| saturated instructions/request | 0.831M | 0.401M | +107.1% |
| saturated RPS | 46,119 | 41,588 | +10.9% |
| saturated average cores | 7.34 | 5.37 | +36.7% |

MoonBit achieved higher total throughput by consuming more runnable cores, not
by completing each request more efficiently.

## Ownership evidence

- Wrapping canonical storage in a typed owner while passing only its raw
  address was insufficient: the typed owner became dead before terminal
  completion and response bytes were corrupted.
- Explicitly keeping the typed owner live through the terminal write fixed the
  content hash.
- A 45-second 64-concurrency snapshot soak completed 2,089,836 successful
  64 KiB responses. The observed RSS shape rules out a leaked 64 KiB snapshot
  per request, but not a tiny metadata leak.
- Resource cancel-before-lock exact-drop tests passed for the experimental
  ownership-cell path. That test does not prove every active/partial-progress
  path.

## Primary standards

Component Model completion-based stream ownership and progress semantics:

- [Concurrency design](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md)
- [Canonical ABI](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md)
- [WIT resource handles](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/WIT.md#handles)

The standard requires terminal/progress ownership accounting. It does not
require a 64-element window, scalar byte lowering, or a particular MoonBit
object representation.
