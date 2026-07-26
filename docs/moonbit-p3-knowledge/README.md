# MoonBit P3 performance knowledge base

Status: research knowledge snapshot, not an accepted upstream design

Last verified: 2026-07-26

Upstream base: `bytecodealliance/wit-bindgen` `main` at
`7c489011423cd454ec639cfb66f6bedb3e2ede2d`

This directory gives a future contributor enough context to continue the
MoonBit Component Model async performance work without inheriting unrelated
prototype changes.

## Start here

The smallest, safest next task is:

> Specialize outgoing `stream<u8>` lowering from a scalar per-element loop to
> a bulk copy while preserving the current staging snapshot, stream lifecycle,
> cancellation, progress settlement, and 4 KiB window.

The complete task prompt is in
[`start-next-agent.md`](start-next-agent.md).

## Current upstream state

The branch is based directly on Bytecode Alliance `main`, not on the fork's
`moonbit` default branch.

The base already contains:

- `ce2815c`: `perf(moonbit): compact async task state storage` (#1665);
- `7c48901`: `perf(moonbit): tune primitive stream buffering` (#1664).

#1664 changes fixed primitive streams from a universal 64-element policy to an
approximately 4 KiB byte budget. At this base, the outgoing generated callback
still allocates a canonical staging buffer and executes:

```moonbit
for i in 0..<data_len {
  let value = data[i]
  wasm...Lower(value, ptr + i * elem_size)
}
```

The next task intentionally changes only that scalar lowering for `u8`.

## Topics

| Topic | Status | Current answer |
|---|---|---|
| [Byte fast path](topics/wit-bindgen-byte-fast-path.md) | supported | Bulk-lower the existing byte snapshot; do not redesign ownership |
| [Transfer ownership](topics/component-model-ownership.md) | investigating | Resource move, buffer lifetime, and settlement are separate concerns |
| [MoonBit residual runtime cost](topics/moonbit-p3-runtime-cost.md) | investigating | After byte specialization, remaining Rust gap is guest/runtime work |
| [async Wasm I/O](topics/async-wasm-io.md) | investigating | Separate custom-ABI stack; its primary output issue is I/O granularity |
| [Engine scaling](topics/engine-scaling.md) | investigating | Engine count helps only when it creates runnable work |

## Authority rules

This knowledge base separates:

```text
upstream source / standards
  → measured evidence
  → topic memo
  → implementation task
```

- Fixed upstream source and Component Model specifications decide facts.
- [`evidence-summary.md`](evidence-summary.md) records the measurements used by
  the topic memos.
- A topic memo states the current interpretation of one question.
- `start-next-agent.md` is an execution task, not a new design authority.
- No proposal in this directory is an accepted ADR.

If a new result changes a topic's answer, update the evidence first, then the
topic. Do not rewrite the old measurement to match a new implementation.

## Explicit non-decisions

None of the following has been accepted:

- a public `CanonicalOwner` API;
- a general resource `RefCell`;
- zero-copy for mutable byte views;
- eager lowering of resource-bearing payloads;
- an endpoint vtable in the shared runtime;
- V8 multi-isolate performance claims.

The successful handwritten intrinsic is a performance lower-bound experiment.
Its active-cancellation path is not production-complete and must not be copied
as the generated lifecycle implementation.

## Primary source pins

- Component Model:
  [`7972c14`](https://github.com/WebAssembly/component-model/tree/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345)
- Wasmtime benchmark host:
  [`ba71bbb`](https://github.com/bytecodealliance/wasmtime/tree/ba71bbb84b7a79f19506cc67a1c5cf4c855b0eb6)
- wit-bindgen measurement release:
  [`1ae0053`](https://github.com/bytecodealliance/wit-bindgen/tree/1ae00530221542369d0e47ee4a1f4232f09d978d)
- current upstream base:
  [`7c48901`](https://github.com/bytecodealliance/wit-bindgen/tree/7c489011423cd454ec639cfb66f6bedb3e2ede2d)

The benchmark used MoonBit nightly `moon 0.1.20260721`,
`moonc v0.10.4`, wasm-tools 1.244.0, and oha 1.15.0.
