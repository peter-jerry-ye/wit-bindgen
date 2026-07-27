# Component Model transfer ownership

Status: investigating, not an implementation decision

## Question

How are operation buffers and resource-bearing values settled across
suspension, partial progress, peer drop, and cancellation?

## Current model

Three source semantics must remain distinct:

| Semantics | Action at send entry |
|---|---|
| snapshot | copy/lower before later mutation can affect the transfer |
| ownership move | invalidate sender aliases immediately |
| static borrow | require immutable storage that lives through terminal |

The operation state is conceptually:

```text
CallerOwned → Prepared → Active(progress) → Settled(progress)
```

Settlement must occur exactly once:

```text
commit accepted prefix
reject unaccepted suffix
release outer allocation
release typed local owner
```

For resource-bearing values, eager lowering may already have moved handles.
Cancellation before the canonical write starts can therefore still require
reject. Active cancellation must obtain terminal progress rather than assume
zero.

## Current non-decisions

- `CanonicalOwner` is a proof model, not an accepted public runtime API.
- A shared resource cell can invalidate aliases but does not implement
  terminal settlement. Its representation and code-generation seam remain
  under investigation in
  [`resource-cell-and-codegen-seam.md`](resource-cell-and-codegen-seam.md).
- Finalizers must not replace deterministic resource release.
- Mutable `FixedArray` does not imply a valid static borrow.
- Resource optimization is not part of the byte fast path.
- Making a generated resource opaque is not a local edit while lift/lower is
  emitted in other generated packages.

## Required fault matrix before production changes

- success;
- cancel before writer lock;
- active cancel before progress;
- partial progress then cancel;
- peer/local drop;
- HTTP disconnect;
- nested resources;
- OOM/trap boundary;
- exact provider drop count;
- double-settlement assertion.

The first byte specialization should preserve the existing generated lifecycle
and avoid opening this design problem.
