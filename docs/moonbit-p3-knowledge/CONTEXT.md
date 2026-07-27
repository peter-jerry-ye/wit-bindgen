# MoonBit Component Model resources

This glossary distinguishes the identities and lifetimes involved when
MoonBit values represent Component Model resources. It describes the problem
domain, not a selected implementation.

## Language

**Resource value**:
A nominal MoonBit value such as `Foo` that gives application and generated
code access to one Component Model resource capability.
_Avoid_: Resource object, rep

**Owner representation (`owner rep`)**:
An integer chosen by a guest that implements an exported resource, used to
find the guest's application state when the host calls the resource again.
_Avoid_: Canonical handle, cell token, object address

**Canonical handle**:
The canonical ABI `i32` that transfers an `own<T>` or temporarily identifies a
`borrow<T>` across a component call.
_Avoid_: Owner rep, resource object

**Cell token**:
A proposed guest-runtime identifier carried inside a Resource value instead
of carrying the Canonical handle directly. It identifies a generation-checked
Resource cell and is not an accepted design.
_Avoid_: Owner rep, canonical handle

**Resource cell**:
Proposed shared dynamic state through which aliases of one Resource value
observe ownership transfer, borrow expiry, and deterministic drop.
_Avoid_: User-facing `RefCell`, owner table

**Owner table**:
Application-maintained storage mapping Owner representations to the state of
exported Resource instances.
_Avoid_: Resource cell table, canonical resource table

**Borrow lease**:
Evidence that an incoming borrowed Resource value is still within the dynamic
call or asynchronous scope in which its Canonical handle is valid.
_Avoid_: Ownership, reference count

**Pending transfer**:
A proposed Resource-cell state for an async `own<T>` argument while its
subtask is still `STARTING`: cancellation before start restores ownership,
while `STARTED` commits the move.
_Avoid_: Taken, borrowed

**Generated ABI interface**:
The code-generator-only operations that turn Resource values into Canonical
handles and lift Canonical handles back into Resource values. MoonBit does not
currently enforce that only generated code calls this interface.
_Avoid_: User interface, endpoint vtable

**Settlement**:
The exactly-once terminal accounting that commits accepted values, rejects
unaccepted values, and releases operation-owned storage after success,
cancellation, peer drop, or failure.
_Avoid_: Drop, finalization
