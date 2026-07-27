# Component Model resource transfer and borrow semantics

Status: pinned semantic research for
[issue #17](https://github.com/peter-jerry-ye/wit-bindgen/issues/17), not an
adapter design decision

## Scope and source pin

This memo answers one question: what rules at Component Model commit
[`7972c14a6c4825fbdc7b7f9f287ae003c9ec8345`](https://github.com/WebAssembly/component-model/tree/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345)
govern `own<T>` and `borrow<T>` lifting, lowering, transfer, failure,
destruction, and synchronous/asynchronous call lifetime?

Normative requirements below come only from that pinned Component Model source.
The final sections separately describe the current MoonBit binding at
wit-bindgen commit
[`7c489011423cd454ec639cfb66f6bedb3e2ede2d`](https://github.com/bytecodealliance/wit-bindgen/tree/7c489011423cd454ec639cfb66f6bedb3e2ede2d)
and policy that the Component Model leaves to a MoonBit adapter.

The Component Model files used here are design/specification sources containing
executable semantic pseudocode. In particular, the Canonical ABI document says
that explicit `trap()` and `trap_if()` calls are the traps in its algorithms,
while `assert()` marks conditions that the specification expects never to fail
([Canonical ABI, lines 94–97](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L94-L97)).

## Short answer

`own<T>` is a unique capability. Lifting it removes the source component's
canonical handle; lowering it creates a new owning handle in the destination.
`borrow<T>` is a call-scoped loan. Lifting it leaves the source handle in place
and lends it to the subtask; lowering it creates a non-owning destination handle
associated with the receiving task. Dropping that borrowed handle releases the
callee-side borrow but never destroys the resource.

There is no separately specified, atomic “transfer commit” operation and no
rollback protocol. Transfer is defined by ordered handle-table mutations during
lifting and lowering. For an asynchronous call, however, there is an important
observable boundary:

- while the subtask is `STARTING`, arguments have not been lifted and an
  `own<T>` argument has not transferred;
- when the subtask starts, argument lifting occurs and `own<T>` arguments
  transfer;
- cancellation before start preserves the caller's ownership;
- cancellation after start does not return transferred `own<T>` arguments.

A MoonBit binding therefore needs a pending-call state if it wants copied
aliases to observe this distinction. A simple eager `Owned -> Taken` transition
before an async call is incorrect unless it can restore ownership on
`CANCELLED_BEFORE_STARTED`.

## Normative semantic objects

WIT defines an owned handle as unique ownership transferred between components.
Dropping the owner destroys the resource. A borrowed handle is a temporary loan
from caller to callee for the duration of the call
([WIT handles, lines 1929–1945](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/WIT.md#L1929-L1945)).
Using a resource name directly denotes an owning handle; `borrow<resource>`
denotes a handle whose drop does not invoke the destructor
([WIT resources, lines 1803–1809](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/WIT.md#L1803-L1809)).

At runtime, each component instance has an encapsulated handle table. Canonical
handles are `i32` table indices; a table access traps on an out-of-bounds or
vacant slot, and removal vacates the slot for reuse
([Canonical ABI table state, lines 1280–1338](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L1280-L1338)).
A resource-handle entry records:

- resource-type identity;
- the resource owner's opaque `rep`;
- whether the handle is owning;
- the task that owns a received borrow;
- the number of live loans made from this handle.

These are semantic fields, not a mandated implementation layout; an optimized
runtime may use another representation that preserves the same observable
behavior
([resource state, lines 1345–1396](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L1345-L1396),
[runtime-state freedom, lines 1269–1277](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L1269-L1277)).

The `rep` and canonical handle are distinct:

```text
resource implementation's opaque rep
                │
                ▼
       ResourceHandle entry
                ▲
                │ i32 canonical handle indexes the table
          component code
```

Only the component instance defining a resource may use `canon resource.rep`,
which returns the stored `rep`
([`canon resource.rep`, lines 4059–4082](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4059-L4082)).

## Exact lift and lower rules

### `own<T>`

`lift_own` removes the source entry, then traps unless the entry is a
`ResourceHandle` of exactly the expected runtime resource type, with no
outstanding lends, and marked owning. On success it produces the entry's `rep`.
The source canonical handle is now vacant
([`lift_own`, lines 2613–2632](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L2613-L2632)).

`lower_own` creates a new owning destination `ResourceHandle` containing that
resource type and `rep`, then returns its new destination table index
([`lower_own`, lines 3072–3079](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3072-L3079)).

Thus a successful cross-component transfer has two semantic table mutations:

```text
source handles.remove(source_i) -> rep
destination handles.add(Owned(resource_type, rep)) -> destination_i
```

Neither the WIT layer nor the Canonical ABI defines an additional commit token,
two-phase transaction, or recovery callback around these mutations.

### `borrow<T>`

`lift_borrow` reads rather than removes the source entry, checks the entry kind
and exact runtime resource type, increments the source handle's lend accounting
through the current subtask, and produces the same opaque `rep`
([`lift_borrow`, lines 2634–2652](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L2634-L2652)).

`lower_borrow` normally creates a non-owning destination handle tied to the
receiving `Task` and increments that task's `num_borrows`. There is a specified
optimization when lowering back into the resource's implementing instance:
the `rep` may be returned directly instead of allocating an intermediate borrow
handle
([`lower_borrow`, lines 3081–3093](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3081-L3093)).

Lending a handle that is itself borrowed is allowed. The additional lend keeps
that source borrow alive and transitively prevents its current task from
returning while the nested call still holds the loan
([transitive borrow explanation, lines 2646–2652](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L2646-L2652)).

## Call phases and the transfer boundary

The Canonical ABI supplies a `Task` or `Subtask` as the ambient borrow scope for
value lowering or lifting
([lifting/lowering context, lines 1199–1223](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L1199-L1223)).

For a lifted export, argument lowering happens only after the task passes
backpressure and calls `Task.start`. The specification explicitly says that
cancellation while waiting for backpressure aborts before lowering the
arguments, so owned handles are not transferred
([`canon_lift` start, lines 3628–3653](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3628-L3653)).

For a lowered call, its `on_start` callback changes the subtask from `STARTING`
to `STARTED` and lifts its arguments. `on_resolve` distinguishes cancellation
before start, cancellation after start, and successful return. Successful
return lowers the result before recording `RETURNED`
([`canon_lower` callbacks, lines 3885–3910](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3885-L3910)).

This yields the only transfer boundary that the caller can observe:

| Subtask observation | Were argument values read? | `own<T>` argument ownership |
|---|---:|---|
| `STARTING` | No | Still in caller's canonical handle table |
| `STARTED` | Yes | Transferred during argument lift |
| `CANCELLED_BEFORE_STARTED` | No | Never transferred |
| `CANCELLED_BEFORE_RETURNED` | Yes | Already transferred; not returned by cancellation |
| `RETURNED` | Yes | Arguments transferred; any owned results transfer in the opposite direction |

The first two distinctions are also explained by the async ABI's memory
contract: `STARTING` means argument and result storage must remain reserved,
while `STARTED` means arguments have been read and argument storage may be
reused
([async subtask status, lines 3945–3953](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3945-L3953)).
The cancellation built-in explicitly defines `CANCELLED_BEFORE_STARTED` as no
argument receipt and therefore no own-handle transfer
([`subtask.cancel`, lines 4433–4450](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4433-L4450)).

For nested values containing multiple owned handles, lift/lower recursively
executes these operations in value traversal order. The pinned algorithm does
not state that the group transfers atomically.

## Borrow lifetime: synchronous and asynchronous

There are two complementary counters:

- `num_lends` belongs to the caller's source handle. It prevents that handle
  from being dropped or transferred while a peer has a loan.
- `num_borrows` belongs to the receiving task. It prevents that task from
  returning or confirming cancellation while received borrowed handles remain.

The concurrency design states both rules explicitly and requires an async
borrowed handle to remember the particular task whose `num_borrows` it
incremented
([Concurrency borrows, lines 753–772](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md#L753-L772)).

Dropping a received borrowed handle decrements its task's `num_borrows` and does
not invoke the resource destructor. Dropping an owning handle is handled
separately
([`canon resource.drop`, lines 4020–4041](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4020-L4041)).
`Task.return_` and `Task.cancel` both trap while `num_borrows` is nonzero
([task resolution guards, lines 961–990](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L961-L990)).

The caller's `num_lends` counters are decremented only when terminal resolution
is *delivered* to the caller, not merely when the callee internally reaches a
terminal state. Resolution delivery covers `RETURNED`,
`CANCELLED_BEFORE_STARTED`, and `CANCELLED_BEFORE_RETURNED`
([subtask lender accounting, lines 1614–1644](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L1614-L1644)).

Consequently:

- in a synchronous call, resolution is delivered before the lowered call
  returns, and lender counts are released there
  ([sync resolution delivery, lines 3922–3943](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3922-L3943));
- in an asynchronous call, lender counts stay live until the terminal subtask
  event is delivered. A subtask cannot be dropped before that accounting has
  happened
  ([async resolution delivery, lines 3945–3982](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3945-L3982)).

This is stricter and more precise than “valid until the callee function happens
to stop using it.” The callee must explicitly drop every received borrow before
return/cancel; the caller remains prevented from dropping or transferring the
lender until it receives terminal resolution.

Borrow-containing values are not currently allowed as `stream` or `future`
elements. The Canonical ABI asserts that such endpoint element types contain no
borrow and says additional bookkeeping would be needed to ensure they cannot
outlive the originating call
([stream restriction, lines 4598–4608](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4598-L4608),
[future restriction, lines 4716–4719](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4716-L4719)).

## Destruction

`canon resource.new` creates an owning canonical handle containing the
implementation-chosen `rep`
([`canon resource.new`, lines 3985–4007](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3985-L4007)).

Dropping an owning handle:

1. checks that the intrinsic may be called;
2. removes the canonical handle;
3. validates its resource kind, runtime resource-type identity, and absence of
   outstanding lends;
4. invokes the defining resource type's destructor, if present, with exactly
   the `rep` previously supplied to `resource.new`.

The destructor is a normal non-async cross-component call. It may not block,
although it may spawn a cooperative thread. Reentrancy checks can trap the
destructor call
([`canon resource.drop`, lines 4010–4056](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L4010-L4056)).
The concurrency design lists an async effect for resource destructors only as a
future TODO, so an adapter must not currently treat destructor completion as an
arbitrary async operation
([Concurrency TODO, lines 1496–1512](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md#L1496-L1512)).

Dropping a borrowed handle never invokes the destructor. If an owning handle was
transferred to a peer, the peer's eventual owning drop invokes the same
resource-type destructor with the original `rep`; the former owner has no live
canonical handle after the successful lift.

## Success, trap, cancellation, peer drop, and destructor table

The table describes Component Model state, not proposed MoonBit classes.

| Event | Source handle | Destination/peer handle | Lend/borrow accounting | Required outcome |
|---|---|---|---|---|
| Successful `own<T>` transfer | Removed during `lift_own` | New owning handle added during `lower_own` | Source must have `num_lends == 0` | Unique ownership moves to peer |
| Successful `borrow<T>` loan | Remains in source table | Non-owning handle, normally tied to receiving task | Source `num_lends += 1`; task `num_borrows += 1` | Temporary loan; no destruction |
| Peer drops received borrow | Source remains | Borrow handle removed | Task `num_borrows -= 1`; source lend remains until resolution delivery | No destructor |
| Peer returns/cancels with a live borrow | Source remains | At least one borrow still live | `num_borrows > 0` | Trap |
| Resolution delivered | Source remains for a loan | Callee has already dropped its borrows | Each recorded source `num_lends -= 1` | Caller may again drop or transfer lender |
| Caller drops or transfers a lent source | Lift/drop removes the indexed entry before the guard in the pinned pseudocode | None | `num_lends != 0` | Trap; no successful operation |
| Async cancellation before start | Unchanged; arguments were not lifted | None | No new lends/borrows from this call | `CANCELLED_BEFORE_STARTED`; own arguments not transferred |
| Async cancellation after start | Owned arguments already removed | Callee received owned arguments; received borrows must be dropped | Borrow counts must settle before `task.cancel` | `CANCELLED_BEFORE_RETURNED`; no results returned |
| Cancellation races with normal return | Depends on whether start occurred | Callee may still return normally | Borrow counts must settle for either terminal path | Caller may receive `RETURNED` despite requesting cancellation |
| Peer drops transferred own | Former source remains vacant | Peer's own handle removed | Drop traps if peer handle has outstanding lends | Destructor called once through that successful owning drop |
| Own drop with no destructor declared | Owning handle removed | N/A | `num_lends == 0` | No-op destructor body |
| Own drop with destructor | Owning handle removed before invocation | N/A | `num_lends == 0` | Synchronous destructor called with original `rep` |
| Destructor traps | Handle was already removed | N/A | N/A | Component Model trap; no rollback or retry is specified |
| Trap during nested lift/lower | Some earlier ordered table mutations may already have happened | Some later mutations may not have happened | No settlement protocol is run | Trap tears down execution/store; no recoverable partial-transfer result |

Cancellation is cooperative. After start, a callee may continue arbitrarily
long and may choose to return normally; it must drop all borrows before either
`task.return` or `task.cancel`. Cancellation before start runs no callee code
([Concurrency cancellation, lines 774–833](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md#L774-L833)).

The final trap row follows from the ordered `remove`/`add` algorithms, the lack
of rollback in those algorithms, and the Canonical ABI statement that a trap
tears down the whole store
([non-async trap behavior, lines 3783–3790](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/CanonicalABI.md#L3783-L3790)).
It is therefore wrong to model a trap like a recoverable MoonBit `Result` that
returns control to the caller with ownership restored. Explicit error returns
and cancellation are recoverable and do require precise ownership state;
canonical traps do not.

## What the pinned standard does not choose for MoonBit

The rules above constrain observable behavior but do **not** require any of the
following adapter choices:

1. a Rust-like `RefCell`, a `ResourceCell`, a generation-checked table token, or
   a directly shared heap object;
2. the MoonBit source-level states or public methods used to represent
   `STARTING`, owned, borrowed, moved, dropped, or expired values;
3. whether copied MoonBit aliases share a dynamic cell, are rejected by a
   separate static analysis, or remain unchecked;
4. whether generated code makes a resource wrapper opaque or continues to
   expose a destructurable single-field representation;
5. how a generator shares type-specific lift/lower operations across MoonBit
   packages;
6. whether a stale or fabricated guest token traps before entering a canonical
   intrinsic;
7. whether explicit `drop` and a language GC finalizer coexist, and how a
   finalizer observes an already-moved or already-dropped alias;
8. the data structure used by a resource implementation to map owner `rep`
   values to application state;
9. whether an adapter adds stronger eager checks than the canonical table's
   traps, provided valid programs retain the specified behavior;
10. how a binding reports a destructor trap to MoonBit. The canonical behavior
    is a trap, not a typed recoverable destructor error.

The standard's semantic table may be optimized; it is not evidence that a
second guest-side table is required. A ResourceCell proposal must justify its
additional lookup and lifecycle independently.

## Current MoonBit adapter seam

At wit-bindgen commit `7c489011`, MoonBit resources are generated as publicly
destructurable `Foo(Int)` values. Imported-resource drop and
locally-defined/exported-resource `new`, `drop`, and `rep` pass this `Int`
directly to canonical resource intrinsics
([MoonBit `type_resource`, lines 957–1057](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/crates/moonbit/src/lib.rs#L957-L1057)).
Every generated handle lift constructs the wrapper from an incoming `Int`, and
every generated handle lower destructures it back to an `Int`
([MoonBit handle lift/lower, lines 1789–1814](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/crates/moonbit/src/lib.rs#L1789-L1814)).

For a resource implemented by MoonBit, the exported canonical destructor
receives the owner `rep`, then the generated shim presents it to user code as
the resource wrapper passed to `Foo::dtor`
([MoonBit destructor shim, lines 1061–1090](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/crates/moonbit/src/lib.rs#L1061-L1090)).
The runtime test demonstrates the intended owner pattern: allocate an integer
`rep`, store application state in `Map[Int, State]`, call `Foo::new(rep)`, and
remove `self.0` from that owner table in `Foo::dtor`
([MoonBit resource-payload test, lines 17–65](https://github.com/bytecodealliance/wit-bindgen/blob/7c489011423cd454ec639cfb66f6bedb3e2ede2d/tests/runtime/moonbit/resource-payloads/leaf.mbt#L17-L65)).

Thus today's `Foo(Int)` means two different things at two owner-facing seams:

- an ordinary resource value contains a canonical handle table index;
- the `Foo` value passed to the destructor shim contains the owner `rep`.

The pinned Component Model permits this source-language presentation, but it
does not require it. Any ResourceCell/code-generation change must preserve the
distinction even if both happen to be represented by MoonBit `Int`.

## Consequences for a ResourceCell prototype

These are derived adapter constraints, not standard-mandated class names:

1. **Sync own lowering:** the source cell may become permanently taken when the
   canonical call starts lifting arguments. If the call returns normally, it
   must never become owned again.
2. **Async own lowering:** before the subtask reaches `STARTED`, the cell needs a
   pending/reserved state. `CANCELLED_BEFORE_STARTED` restores the original
   ownership; `STARTED`, `RETURNED`, and `CANCELLED_BEFORE_RETURNED` commit the
   move.
3. **Borrow lowering:** the lending source must remain non-droppable and
   non-transferable from start until terminal resolution is delivered. An async
   “lease” should therefore correspond to the subtask-resolution boundary, not
   merely a lexical callback frame.
4. **Incoming borrow:** every alias must be unable to outlive successful
   `task.return`/`task.cancel`; received-borrow drop must never call the resource
   destructor.
5. **Trap:** no ownership rollback is necessary after a canonical trap because
   control does not return to a live store. Rollback is necessary for the
   explicit `CANCELLED_BEFORE_STARTED` state.
6. **Destructor:** the owner `rep` must reach the implementation destructor
   exactly once for each successfully dropped owning canonical handle, without
   being confused with either a canonical handle or a guest cell token.
7. **Nested values:** a cell implementation must tolerate ordered, non-atomic
   movement of multiple handles. It must not promise transactional all-or-none
   transfer that the Canonical ABI does not provide.

## Remaining ambiguities and deliberately open adapter policy

1. **No guest-side start callback:** the standard exposes `STARTING`,
   `STARTED`, and terminal progress through the async lowered ABI, but does not
   prescribe how a source-language binding attaches ownership transitions to
   those statuses.
2. **Mutation before guards:** `lift_own` and `resource.drop` remove a table
   entry before several trapping checks. Since a trap tears down the store, the
   pinned semantics need no rollback, but they do not specify whether an
   adapter may prevalidate and trap before changing its own shadow state.
3. **Failure between source removal and destination insertion:** the algorithms
   do not define a recovery path for table exhaustion, allocation failure, or
   another trap after an earlier owned handle has been lifted. The whole-store
   trap makes this unobservable to surviving component code.
4. **Component-tree teardown:** the concurrency design discusses automatic
   dropping of all handles during a future “blast zone” feature, not as a
   current general rule
   ([Concurrency cancellation, lines 828–833](https://github.com/WebAssembly/component-model/blob/7972c14a6c4825fbdc7b7f9f287ae003c9ec8345/design/mvp/Concurrency.md#L828-L833)).
   A MoonBit GC finalizer policy cannot be derived from that future possibility.
5. **Destructor failure:** the handle is removed before the synchronous
   destructor call and reentrancy may trap. The standard specifies neither a
   retry nor typed cleanup recovery after that trap.
6. **Public representation:** the standard says nothing about MoonBit package
   visibility. Keeping `Foo(Int)`, reinterpreting the `Int` as a cell token, or
   moving lift/lower to a defining-package ABI adapter are generator-architecture
   decisions.

These open points should remain explicit in any implementation issue. They are
not missing normative own/borrow rules; they are the language-adapter seam.
