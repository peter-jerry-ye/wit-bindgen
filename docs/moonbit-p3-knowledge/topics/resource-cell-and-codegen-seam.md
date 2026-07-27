# MoonBit resource ownership cell and code-generation seam

Status: design investigation, not an implementation decision

## Question

Can generated MoonBit detect use-after-move, double drop, and escaped borrows
without exposing a combinatorial ownership interface to application code or
breaking lift/lower across generated packages?

## Current source shape

At the pinned upstream base, a WIT resource is generated as a publicly
destructurable MoonBit wrapper:

```moonbit
pub(all) struct Foo(Int)
```

`HandleLower` is emitted at each use site by pattern-matching that wrapper and
passing its `Int` directly to the canonical ABI. `HandleLift` constructs the
wrapper at each use site from the incoming `i32`. Imported `drop`, exported
`new`, exported `drop`, and exported `rep` also pass the wrapped `Int` directly
to resource intrinsics.

This is visible in the pinned generator:

- [`type_resource`](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/crates/moonbit/src/lib.rs#L957-L1057)
  defines `Foo(Int)` and its resource operations;
- [`HandleLower` and `HandleLift`](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/crates/moonbit/src/lib.rs#L1789-L1814)
  destructure and construct the same wrapper at the lowering/lifting use site.

Consequently, making `Foo` opaque is not a local representation change.
Generated code in other MoonBit packages would lose the ability to lift or
lower `Foo`, including when it is nested in a record, variant, list, or fixed
array. Moving that responsibility to the package that defines `Foo` would be a
code-generation architecture change.

## Identities that must remain distinct

```text
application state
      ▲
      │ owner table lookup
 owner rep

MoonBit Resource value: Foo(...)
      │
      │ generated ABI interface
      ▼
 canonical handle ── Component Model resource table
```

An exported-resource owner needs its user-chosen
[Owner representation](../CONTEXT.md#language); in MoonBit that is commonly an
`Int` key into an application-maintained map because the binding does not use a
MoonBit object address as the representation.

That Owner representation is not the Canonical handle stored by today's
`Foo(Int)`. A possible Resource cell introduces a third identity, the Cell
token. Treating all three as "the resource integer" obscures both ownership
and cleanup responsibilities.

## Constraints

1. Keep the ordinary user interface small: one WIT Resource value plus its WIT
   methods and the necessary owner operations such as `new`, `rep`, and
   deterministic `drop`.
2. Preserve cross-package lift/lower for nested resource-bearing types.
3. Preserve the owner-managed `rep -> application state` model.
4. An incoming borrow must not become owned. It may only remain valid for its
   call or asynchronous Borrow lease.
5. MoonBit Resource values are copyable aliases today, so move and drop safety
   cannot rely solely on static affine typing.
6. Resource movement, operation-buffer ownership, and terminal Settlement are
   related but separate state machines.
7. A finalizer may be a backstop but must not replace deterministic canonical
   resource release.
8. Any additional check on a hot resource path needs a benchmark against
   direct handwritten resource intrinsics.

## Alternatives

### A. Keep the raw Canonical handle

Keep `Foo(Int)` with `Int` equal to the Canonical handle.

This is the current, cheapest representation. Copied aliases cannot observe
that another alias has moved or dropped the handle, so generated code cannot
reliably reject use-after-move or double drop.

### B. Keep `Foo(Int)`, reinterpret `Int` as a Cell token

Preserve the publicly destructurable representation needed by current
cross-package generated code, but store a generation-checked Cell token rather
than the Canonical handle:

```text
Foo(cell_token)
       │
       ▼
Resource cell
  canonical_handle
  state: owned | borrowed(lease) | taken | dropped | expired
  resource type/instance domain
```

All generated resource leaves would call a shared runtime:

```text
lift-own(handle)           -> cell_token
lift-borrow(handle, lease) -> cell_token
lower-own(cell_token)      -> handle
lower-borrow(cell_token)   -> handle
rep(cell_token)            -> handle -> resource.rep
drop(cell_token)           -> handle -> resource.drop
```

Benefits:

- the source-level `Foo(Int)` shape and cross-package construction remain;
- copied aliases share a move/drop state;
- fabricated or stale tokens can fail closed instead of becoming arbitrary
  Canonical handles;
- the user-facing Resource interface does not grow a public `RefCell` product
  of owned/borrowed/guard types.

Costs and risks:

- at least one guest-runtime lookup on resource operations;
- cell allocation, generation reuse, and cleanup need exact accounting;
- every generated resource path must stop passing the wrapper's `.0` directly
  to an intrinsic;
- a single shared table needs resource type and component-instance domains,
  while per-resource tables require all generated packages to find the same
  table;
- language visibility still permits application code to inspect or fabricate
  an `Int`, though a checked resolver can trap on invalid values.

This is the least structural prototype, not necessarily the final
representation.

### C. Store a shared Resource cell object directly

`Foo` could wrap a shared runtime object rather than a table token. Aliases
would naturally share state and common operations might avoid a table lookup.
However, the runtime type and enough constructors/accessors would need to be
visible to every generated lift/lower site. It also changes the current
single-`Int` representation and still does not create a generated-code-only
visibility class.

This alternative should be compared experimentally with B rather than assumed
to be cheaper or cleaner.

### D. Make `Foo` opaque and move lift/lower to its defining package

The defining package would expose a narrow Generated ABI interface:

```text
lift-own
lift-borrow
lower-own
lower-borrow
```

Other generated packages would call those operations instead of
pattern-matching `Foo`. Application documentation could hide the interface,
although MoonBit would still treat it as public unless the language or package
layout offers a stronger generated-code visibility mechanism.

This gives the cleanest seam: the module that owns the Resource representation
also owns its lift/lower invariants. It requires changing how the generator
shares type-specific lowering across packages, so it must not be described as
a local `opaque` edit.

## Candidate dynamic state model

The Resource cell model is useful independently of its physical
representation:

```text
lift own   → Owned(handle)
lift borrow→ Borrowed(handle, live lease)

Owned   -- lower-own --> Taken
Owned   -- drop ------> Dropped
Owned   -- lower-borrow/read-rep --> Owned

Borrowed -- lower-borrow/read-rep while lease live --> Borrowed
Borrowed -- lease expires --------------------------> Expired

Taken/Dropped/Expired -- any consuming operation --> trap
```

Open semantic details include:

- the exact ownership commit point if canonical lowering traps after the
  dynamic state transition;
- whether outgoing borrow needs a count, a single operation lease, or a
  different guard;
- how an async incoming borrow's lease is represented and invalidated;
- whether a finalizer on a copied alias becomes a no-op after explicit
  settlement;
- how a canonical exported-resource destructor, whose input represents the
  owner's `rep`, interacts with Resource-value cells;
- whether Cell token reuse can be both compact and stale-alias safe.

These must be resolved against the
[Canonical ABI](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md),
[WIT handle semantics](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/WIT.md#handles),
and
[async concurrency design](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md),
not inferred from a Rust `RefCell`.

## Required tests before selecting a representation

- copy an owned Resource value, consume one alias, then use/drop the other;
- copy and explicitly drop through two aliases;
- escape an incoming borrow past its synchronous or async lease;
- lower a borrow while an owner operation is active, then cancel it;
- own and borrow nested under records, variants, lists, options, results, and
  fixed arrays;
- equal numeric Canonical handles belonging to different resource types;
- recycle a cell slot, then use a stale Cell token;
- lift in one generated package and lower in another;
- exported-resource `new`, `rep`, method dispatch, and destructor with an
  application Owner table;
- trap/OOM during preparation and lowering;
- success, cancellation, peer drop, partial progress, and exactly-once
  Settlement for resource-bearing streams/futures;
- exact provider drop count and an assertion against double settlement.

## Performance questions

Measure at least:

- direct handwritten intrinsic;
- current raw-handle generated code;
- Cell token/table lookup;
- direct shared-cell object, if implementable;
- nested resource arrays to expose per-element overhead;
- success and cancellation paths;
- throughput, tail latency, CPU per request, peak/live cell count, and RSS.

A Resource cell is justified only if its correctness value is clear and its
cost is either small or can be removed from proven single-owner paths.

## Recommended sequence

1. Add failing conformance tests for aliasing, borrow expiry, cross-package
   nesting, and owner destruction without changing representation.
2. Audit every MoonBit `HandleLift`, `HandleLower`, resource intrinsic, and
   async resource-bearing endpoint for direct raw-handle assumptions.
3. Prototype alternative B behind generated/runtime-private names and measure
   it against direct intrinsics.
4. Prototype C only if table lookup or lifecycle cost is material.
5. Decide whether the correctness and locality gains justify alternative D's
   type-owned ABI adapter refactor.
6. Only then decide the public/generated visibility and naming. Do not expose a
   general-purpose `RefCell` merely because the internal state machine is
   RefCell-like.

## Review questions

- Is a generation-checked Cell token a valid transitional representation, or
  does it conflict with assumptions elsewhere in MoonBit FFI generation?
- Should the Generated ABI interface be a deliberately public but undocumented
  convention, or should lift/lower move to the resource's defining package?
- Where does the Component Model define the ownership commit point needed to
  recover or invalidate a cell after a lowering trap?
- What is the minimal correct lease for incoming and outgoing async borrows?
- Can the generator prove enough single-owner cases to bypass dynamic cells
  without splitting the user interface into many ownership variants?

Related fork issues:

- [#1: visibility from another package](https://github.com/peter-jerry-ye/wit-bindgen/issues/1)
- [#12: builtin `Future` and `Stream`](https://github.com/peter-jerry-ye/wit-bindgen/issues/12)
- [#13: CM Task and MoonBit Task](https://github.com/peter-jerry-ye/wit-bindgen/issues/13)
