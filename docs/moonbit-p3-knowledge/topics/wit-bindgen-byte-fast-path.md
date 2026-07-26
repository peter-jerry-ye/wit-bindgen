# `wit-bindgen` MoonBit byte fast path

Status: supported research direction, not accepted ADR

## Question

What is the smallest semantics-preserving change that removes unnecessary
per-byte lowering work from generated outgoing `stream<u8>`?

## Current answer

Keep the complete existing generated lifecycle and specialize only the
representation-equal byte copy:

```text
current owned FixedArray snapshot
  → current writer lock
  → current canonical allocation
  → one bulk copy for u8
  → current write/cancel/progress/commit/reject lifecycle
```

Do not merge snapshot and canonical storage in the first patch. Do not change
the approximately 4 KiB window already provided by upstream #1664.

## Why this boundary

- The 4 KiB scalar→bulk comparison showed a large CPU/instruction reduction.
- Keeping the operation count unchanged may leave saturated RPS nearly flat;
  that does not make the optimization invalid.
- The complete generated bridge was already close to the successful
  handwritten path once byte lowering and operation shape were controlled.
- Snapshot and static borrow had nearly equal CPU/throughput, so zero-copy is
  not justified by the measured workload.
- Resource ownership creates much more correctness risk and has no measured
  performance requirement for this patch.

## Included

- outgoing `stream<u8>`;
- transparent aliases only when representation equality is proven;
- one bounds-checked bulk copy into existing canonical storage;
- generator tests and response-content tests.

## Excluded

- input streams;
- strings, records, nested lists, and resources;
- new public APIs;
- eager lowering before lock;
- window changes;
- `CanonicalOwner`, `RefCell`, static borrow, or endpoint vtables;
- changes to cancel/progress/settlement.

## Acceptance

- `u8` selects the bulk path;
- non-byte and resource payloads keep the old path;
- sizes around 0, 64, 4096, and 65536 are exact;
- mutation of the original caller view cannot change staged bytes;
- cancellation and peer drop continue to use existing generated code;
- CPU/instructions improve without requiring a saturated-RPS claim.

The executable task is
[`../start-next-agent.md`](../start-next-agent.md).
