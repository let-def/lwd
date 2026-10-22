# Stress-testing findings for `lwd`

This documents what came out of a stress-testing pass over `lwd`, `lwd_table`,
`lwd_seq` and `lwd_utils`. The corresponding testsuite lives in
`test/` (`dune build test/run_tests.exe && ./_build/default/test/run_tests.exe`,
or `dune test`); every finding below has a test pinning it down, referenced by
name and file.

**Update 1:** this pass was first run against `master` (818ce8a), then re-run
against a branch that turned out to already contain fixes for several of the
bugs found the first time around. This revision reflects that branch:
findings that no longer reproduce are marked **FIXED**. The core library was
**not modified** as part of either pass.

**Update 2:** the last open bug (an exception during evaluation confusing the
unsafe-mutation diagnostic) was fixed in `lib/lwd/lwd.ml` (`sample`). See
"Still-open bugs".

**Update 3:** `map2`/`pair`/`app` now evaluate their operands left to right
(previously listed under "Notable, apparently-intentional behaviors"). See
"Still-open bugs".

**Update 4:** released nodes are now reset to `Eval_none` rather than
`Eval_progress`, so a re-acquired graph behaves like a fresh one. See
"Still-open bugs".


## Fixed since the initial pass (previously reported against `master`)

### `Lwd_table.next`/`prev` used to crash on a row that was bound and then removed

Fixed by commit `3b17bcf` ("Lwd_table: fix `{prev,next}` on deleted rows"),
which adds a `| Node {parent = Leaf; _} -> None` case ahead of the
tree-walking logic in both functions. `lwd_table.mli` documents `next`/`prev`
as returning `None` "if the input row is unbound"; this used to only hold for
a row that was *never* bound, and crashed with `Assert_failure` for a row
that used to be bound and became unbound via `remove` or `clear` (root cause:
`remove`/`clear` sever a node by pointing its `.parent` straight at `Leaf`,
which the fallback tree-walk in the old `next`/`prev` treated as an
impossible case).

Pinned (as a fix-confirmation, not a bug pin) by: `test/test_lwd_table.ml`,
*"FIXED: next/prev on a row that WAS bound and became unbound via
remove/clear now correctly return None ..."*.

### `Lwd_table.row` structural-equality hazard now has a sanctioned escape hatch

`'a Lwd_table.row` is still a genuinely cyclic mutable value (a tree node
with a `parent` back-pointer), so comparing two rows with structural
equality/`compare` is still unsafe in principle — see the still-relevant
writeup under finding #9 below. What's new: `lwd_table.mli` now exposes
`equal_row : 'a row -> 'a row -> bool` (`= (==)`), a documented, sanctioned
way to compare rows that sidesteps the hazard entirely. Pinned by:
`test/test_lwd_table.ml`, *"equal_row is the safe (physical-equality-based)
way to compare rows ..."*.

### The invalidation-safety diagnostic no longer misfires on `fix`'s own internal self-mutation

On `master`, three of the scenarios upstream's own new `lib/lwd/lwd_test.ml`
exercises (`fix_stabilization_loop`, `fix_with_dynamic_bind`,
`fix_dirty_bit_clearance`) failed with "Unexpected unsafe action": every
mutation of `fix`'s witness variable from within its own body — the entire
point of `fix` — was flagged as an unsafe re-entrant mutation. Commits
`2b1884b` ("Fix too optimistic invalidation logic") and `10e2ff7`
("Simplify and correct fix logic") replaced the old `Strong`/`Fragile`
sensitivity-propagation scheme with a much simpler model: invalidating a
`Fix` node that's mid-evaluation just quietly resets it to `Eval_none`
(`Operator {desc = Fix _; value = Eval_progress; _} -> t.value <- Eval_none`)
without flagging anything or propagating further. Verified: all 12 tests in
`lib/lwd/lwd_test.ml` pass on `fixes`; running that same file against
`master` reproduces the 3 failures above.

### `fix` is now correctly "re-armed" after it converges

On `master`, once a `fix` computation stabilized, a later, independent
mutation of the witness variable could fail to reach the observing root at
all — `is_damaged` stayed `false` and `on_invalidate` was never called, even
though a later explicit re-sample *would* still have recomputed correctly.
This is a real hazard for reactive-UI-style consumers that rely on
`on_invalidate` to know when to schedule a re-render: the notification is
what silently goes missing, not the underlying data. Root cause and fix:
commits `7390d23`/`6a1f474` ("fix was not re-armed after convergence"),
together with `sub_sample`'s `Fix` case now unconditionally caching
(`t.value <- Eval_some result`) after its internal convergence loop, and
separately self-invalidating (`do_invalidate self`) if `doc` is *still*
damaged once the witness has stabilized.

Verified directly: reproduced the missed-invalidation symptom against
`master` (`is_damaged` stays `true` from the very first sample onward, and
`on_invalidate` is not called for the second logical change) with a
standalone script mirroring this exact scenario; it does not reproduce on
`fixes`. Pinned by: `test/test_lwd.ml`, *"fix: is correctly \"re-armed\"
after convergence ..."*, adapted from upstream's own `test_fix_dirty_bit_clearance`.

### Reference removal used to stay deferred inside the release queue

Commit `85c90f7` ("Separate removing reference from releasing graph"):
previously, both *removing a reference* (decrementing a node's trace) and
*actually releasing it* (tearing down children, calling `Prim.release`) were
deferred together into the release queue. If a `bind`'s inner graph stayed
referenced-but-queued longer than expected, this could cause spurious
invalidations. Now `remove_reference` runs synchronously at the point of
disconnection, and only the deep release is deferred; a new `TA` trace state
("like `T0` but the node is still acquired — it is in a release queue
somewhere") tracks that distinction, fixed alongside in `95c580a` ("fix
assertions in nodes acquired while in release queue"). This was originally
caught by upstream's own `test_fix_with_bind`.

A stress test bouncing a `bind` back and forth between three branches many
times without ever flushing the release queue in between (`test/test_lwd.ml`,
*"bind: switching back and forth between branches without flushing the
release queue ..."*) passes on `fixes`; note this exact sequence was checked
and does *not* happen to reproduce a failure on pre-fix code either — it's
kept as a general stress test of the same area, not a proven regression test
for this specific historical bug.

## Still-open bugs

None.

### FIXED: an exception during evaluation left the root's diagnostic state confused

If a computation raised while being evaluated, `sample`/`sub_sample` never
got to run the code that caches the result, so both the failing node *and the
observing root itself* were left at `Eval_progress`. The root being stuck this
way is what mattered: the very next time anything invalidated it — even a
completely unrelated, perfectly ordinary `Lwd.set` — `invalidate_node`'s
`Root` case saw `Eval_progress` and unconditionally set
`status.unsafe <- true`, exactly as if a real re-entrant sample were in
progress. Recomputation itself was correct; only the diagnostic was a false
positive.

**Fix applied:** `sample` now resets the root to `Eval_none` when
`sub_sample` raises, then re-raises. This costs one exception handler per
`sample` call, not per node.

The inner operators on the path of the exception are deliberately left at
`Eval_progress`: for an operator that is not being evaluated, that state
behaves like `Eval_none`. `sub_sample` recomputes it, and `invalidate_node` resets
and propagates it. The two cases that stop propagation, `Fix` and the `Join`
carve-out, are harmless there, because a stuck node never has an ancestor
holding a cached value derived from it: every ancestor on the exception's
path is stuck too, and any other ancestor was already invalidated when the
node was. Adding a handler per node in `sub_sample` would put it on the hot
path for no observable benefit.

The one consequence: `Eval_progress` on an operator does not mean "currently
being evaluated". Cycle detection based on that state (see "No cycle
detection" below) would need a distinct state.

Pinned by: `test/test_lwd.ml`:
- *"FIXED: after an exception escapes evaluation, the root is reset to Eval_none ..."*
- the *"exceptions: ..."* tests that follow it, which check that inner nodes
  left at `Eval_progress` are harmless: deep chains, repeated failures,
  stuck nodes shared between roots or with more than 4 parents (`Tn`
  traces), `fix` and `bind` (selector or branch raising), prims whose
  `acquire` raises, and releasing a root right after a failed sample. Each
  checks values, `on_invalidate` calls, prim acquire/release balance and the
  unsafe-mutation logger.

### FIXED: `map2`/`pair`/`app` evaluated their right operand before their left one

`sub_sample`'s cases for these three combinators were written as direct
nested applications, e.g. `f (aux self x) (aux self y)` for `Map2`. OCaml's
evaluation order for function arguments is unspecified by the language, and
both the bytecode and native backends evaluate `f a b` as "compute `b`, then
`a`, then apply" — so for `Lwd.map2 lhs rhs ~f`, `rhs` was sampled before
`lhs`, `Lwd.pair a b` sampled `b` before `a`, and `Lwd.app fdoc xdoc`
sampled `xdoc` before `fdoc`. Since this fell out of an unspecified language
behavior, it was not guaranteed to be stable across OCaml
versions/optimization settings either.

**Fix applied:** operands are now bound with `let` in source order, so they
are evaluated left to right; `lwd.mli` documents this for `map2` and `app`
(and `pair`, defined as a `map2`). This matches the order in which
`sub_acquire` and `sub_release` already visit them.

Pinned by: `test/test_lwd.ml`, *"FIXED: map2/pair/app evaluate their
operands left to right ..."*, which also checks a nested combination.

### FIXED: a released and re-acquired graph did not behave like a fresh one

`sub_release` left released nodes at `Eval_progress` instead of `Eval_none`
(since `e299f31`, without a stated reason). The two states only differ in
`invalidate_node`: `Eval_none` stops an invalidation, `Eval_progress` resets
the node and propagates to its parents. A released node has no parents, but
once a root acquires the graph again, the parts not evaluated yet are
acquired but not computed, and user code running meanwhile could observe the
difference. For instance, in a `map2` whose left operand sets a var read by
the right one, the mutation was not flagged on a fresh graph but was flagged
as unsafe after a release and re-observe.

**Fix applied:** `sub_release` resets released nodes to `Eval_none`. This
also keeps `Eval_progress` meaningful (see "No cycle detection" below). As
a side effect, a node released in the middle of its own evaluation no
longer caches the result when that evaluation finishes.

Pinned by: `test/test_lwd.ml`, *"a released and re-acquired graph behaves
like a fresh one ..."*.

## Notable, apparently-intentional behaviors (not bugs, but easy to trip over)

All of the following were re-verified against `fixes` and still hold exactly
as originally described against `master` — the invalidation-logic rewrite
changed *how* unsafe mutations are detected (see above) but not these
user-visible outcomes.

### Cross-sibling mutation in `map2`/`pair`/`app` is flagged unsafe, unless the target was never evaluated

A comment above `invalidate_node` in `lwd.ml` floats the idea of treating
`map2`/`pair`/`app` operands as independent enough that one could mutate a
variable the other one reads, since they're "morally" evaluated in parallel,
but explicitly says this was never implemented. Still true post-rewrite:
the right operand mutating a dependency of the left one, which was already
read, is flagged `` `Mutation `` exactly like any other re-entrant mutation,
because the invalidation propagates up to the currently-`Eval_progress` root.

The left operand mutating a dependency of the right one depends on whether
that dependency holds a cached value. If it was never evaluated (typically on
the first sample), `invalidate_node` stops at its `Eval_none` state: nothing
is flagged, and the right operand, which runs next, reads the new value. On
later samples the dependency is cached, so the same mutation is flagged. The
diagnostic is conservative here: the value read is not stale either way.
Before operands were evaluated left to right, the same applied with the roles
swapped.

Pinned by: `test/test_lwd.ml`, *"map2: the right operand mutating a
dependency of the left one ..."* and *"map2: the left operand mutating a
dependency of the right one is only flagged ..."*.

### The "safe inner invalidation during a Join's outer evaluation" carve-out is real, and narrow

`invalidate_node` has a special case: while the *selector* half of a `bind`
(the "outer" document that decides which branch to run) is still being
evaluated, invalidating the *previously selected* branch is explicitly treated
as safe and not flagged, because that branch is about to be replaced/dropped
anyway. This is a real, working code path, not just aspirational — confirmed
with a scenario that mutates a variable the *old* branch depends on while the
selector closure itself is still running.

The carve-out is narrow, though: mutating a variable that the *newly
selected* branch depends on, during that branch's own (separate) evaluation
pass, is **not** covered by it and is flagged `` `Mutation `` like anything
else.

Pinned by: `test/test_lwd.ml`, *"bind: mutating a var only the OLD
(about-to-be-replaced) branch depends on ... IS considered safe"* and
*"bind: mutating a var read by the freshly-selected branch ..."*.

### The "unsafe mutation" diagnostic is a per-`set`-call flag, not per-observer

`do_invalidate` threads a single `status` record through the *entire*
invalidation fan-out of one `Lwd.set` call. If that one `set` reaches several
independent observers of the same variable, and even just one of them happens
to be behaving re-entrantly (e.g. it's the classic "read a var, then set it
again" pattern), the whole call is logged as unsafe — including for
completely unrelated, perfectly well-behaved observers that were invalidated
by the very same `set`. The unrelated observers still recompute correctly;
only the diagnostic is coarse-grained. There is no way, from the logger
callback alone, to tell which observer actually misbehaved.

Pinned by: `test/test_lwd.ml`, *"unsafe status is a per-`set`-call flag, not
per-observer ..."*.

### No cycle detection: a self-referential document overflows the stack

The `eval` type's `Eval_progress` constructor is commented as "used to detect
cycles", but no code path actually inspects that state to *reject* a cycle —
`sub_sample`'s cache-lookup match treats `Eval_progress` exactly like
`Eval_none` (both fall into the same `| _ ->` branch that starts a fresh
evaluation). A document that, through `bind`/`join`, ends up depending on
itself (constructible via a `let rec`/`Lazy.t` indirection) recurses without
bound and overflows the stack, rather than raising a clean, diagnosable
error. `Lwd.dump_trace` exists for debugging trace shapes, but nothing calls
it automatically on this path.

Pinned by: `test/test_lwd.ml`, *"no cycle detection: a self-referential
document overflows the stack ..."* — guarded by a wall-clock alarm so the
testsuite itself can't hang if the behavior ever changes to "loops forever"
instead of a stack overflow.

### `fix` has no built-in iteration cap

`Lwd.fix` retries its body for as long as the `wrt` witness keeps changing
(now via a tail-recursive internal `loop`, still with no maximum iteration
count). A `fix` whose body keeps invalidating its own witness spins
indefinitely (confirmed at several hundred thousand iterations per second;
bounded in the test only by an external wall-clock alarm). This matches the
documented semantics ("tries to stabilize... by repeating evaluation until
the witness stabilizes"), but it's worth calling out explicitly since it
means a convergence bug in application code turns into a hang, not a bounded
error.

Pinned by: `test/test_lwd.ml`, *"fix: a non-convergent computation is not
bounded ..."*, contrasted with the convergent case in *"fix: converges in the
expected number of rounds ..."*.

### `Lwd_table.row` is a cyclic mutable value: never compare it with `(=)`/`compare`

`'a Lwd_table.row` (= `'a Lwd_table.t`, the internal `tree` type) is a
balanced-tree node that holds a mutable `parent` back-pointer alongside its
`left`/`right` children — a live, bound row is a genuinely cyclic OCaml value.
Comparing two such values with structural equality/comparison (`(=)`,
`compare`, `Hashtbl.hash`-based structures, `[@@deriving eq]`, etc.) walks
that cycle. OCaml's generic `compare`/`(=)` has no cycle detection; in
practice this doesn't stack-overflow (the runtime uses an explicit,
heap-allocated worklist rather than the C stack specifically so it can handle
deep-but-acyclic structures) — instead the worklist grows forever and the
process dies with `Out_of_memory`.

This is easy to reach by accident: `Lwd_table.next r1 = Some r2` looks like an
entirely ordinary, innocuous comparison. It only crashes when both sides are
`Some _` (the discriminant-only fast path for `None` vs `Some _` never
touches the row's contents); comparing against `None`, or checking
`is_bound`/`get`, is unaffected. As of this branch there's a documented,
sanctioned fix: always compare rows with the new `Lwd_table.equal_row`
(physical equality) — see "Fixed since the initial pass" above.

This was hit directly while writing the testsuite itself (see
`test/test_lwd_table.ml`'s `row_opt_eq` helper and its doc comment); it is
*not* included as an automated regression test, since deliberately inducing
an `Out_of_memory` condition inside a shared test process is inherently
unreliable across machines/heap sizes and risks destabilizing whatever runs
after it. Minimal repro, run standalone:
```ocaml
let t = Lwd_table.make () in
let r1 = Lwd_table.append t ~set:1 in
let r2 = Lwd_table.append t ~set:2 in
let _r3 = Lwd_table.append t ~set:3 in
Lwd_table.unset r2;
ignore (Lwd_table.next r1 = Some r2) (* Out_of_memory *)
```

## Things that were suspected to be bugs, but weren't

While iterating on the testsuite itself, two things looked at first like
library bugs and turned out to be bugs in the test code instead — worth
recording so the reasoning isn't lost:

- An early property-test failure for `Lwd_table` (order mismatches between a
  naive list model and the real table after `before`/`after` operations)
  turned out to be an off-by-one in the test's own shadow model: the `Before`
  action spliced the `live`-rows list using `after`-style index boundaries
  while splicing the plain-value model list using `before`-style boundaries.
  Once aligned, 300 random `prepend/append/before/after/remove/set/clear`
  scenarios against `Lwd_table` all pass — see *"lwd_table: order and
  incremental reduction agree with a naive model ..."*.
- An early version of the "map2/pair/app evaluation order" test above used
  `Lwd.map ~f (Lwd.return ())` to build its instrumented operands. `map`
  over a `Pure` value is folded into a new `Pure` value *eagerly*, at graph
  construction time (`Pure vx -> Pure (f vx)` in `lwd.ml`), bypassing
  `sub_sample` entirely — so the test was unknowingly measuring OCaml's
  argument-evaluation order for the *test's own* function calls, not `lwd`'s
  operand evaluation order at all. It happened to agree with the
  answer at the time by coincidence (both were instances of the same
  right-to-left convention). Rewritten to use `Lwd.var`/`Lwd.get` as the base document,
  which forces a genuine, lazily-evaluated `Operator` node.
