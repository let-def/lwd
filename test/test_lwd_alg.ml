(* Stress tests for [Lwd_alg]: incremental transformations over custom
   algebraic data types, generalizing [Lwd_seq]'s reuse-by-physical-identity
   scheme to arbitrary user-defined recursive types.

   The test data type is a simple binary tree of ints:
     type tree = Leaf of int | Node of tree Lwd_alg.t * tree Lwd_alg.t
   Every subtree is wrapped in [Lwd_alg.t] (via [make]), since that's how the
   library peeks into recursive structure (through the user-supplied
   [foldable]/[fold_tree] below). *)

open Framework

type tree =
  | Leaf of int
  | Node of tree Lwd_alg.t * tree Lwd_alg.t

let fold_tree : tree Lwd_alg.foldable =
 fun kind acc t ->
  match t with
  | Leaf _ -> acc
  | Node (l, r) -> Lwd_alg.add kind (Lwd_alg.add kind acc l) r

let leaf n = Lwd_alg.make fold_tree (Leaf n)
let node l r = Lwd_alg.make fold_tree (Node (l, r))

let rec naive_sum t =
  match Lwd_alg.peek t with
  | Leaf n -> n
  | Node (l, r) -> naive_sum l + naive_sum r

(* A fresh, independent counting sum-map: every test gets its own so call
   counts across tests can't interfere with each other (map.slot/defined/func
   are mutable fields on a single shared record, see the "two independent
   streams" test below for what happens when a map IS deliberately shared). *)
let make_counting_sum_map () =
  let calls = ref 0 in
  let m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
  Lwd_alg.define m (fun tape t ->
      incr calls;
      match t with
      | Leaf n -> n
      | Node (l, r) -> Lwd_alg.apply tape m l + Lwd_alg.apply tape m r);
  (m, calls)

let () =
  test "basic: transform computes the same result as a naive recursive fold" (fun () ->
      let m, _ = make_counting_sum_map () in
      let f = Lwd_alg.transform m in
      let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 in
      let t = node (node l1 l2) l3 in
      assert (f t = naive_sum t);
      assert (f t = 6))

let () =
  test "peek/make: peek returns exactly the value passed to make" (fun () ->
      let t = leaf 42 in
      match Lwd_alg.peek t with
      | Leaf 42 -> ()
      | _ -> failwith "peek returned something else")

let () =
  test "calling the SAME unchanged (physically identical) top-level value again is fully \
        cached: the map function is not re-invoked at all"
    (fun () ->
       let m, calls = make_counting_sum_map () in
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 in
       let t = node (node l1 l2) l3 in
       assert (f t = 6);
       let first_round_calls = !calls in
       assert (first_round_calls > 0);
       calls := 0;
       assert (f t = 6);
       assert (!calls = 0);
       assert (f t = 6);
       assert (!calls = 0))

let () =
  test "BUG: changing a single leaf forces recomputation of the ENTIRE tree, including \
        completely unrelated, physically-unchanged sibling subtrees -- not just the path \
        from the changed leaf to the root. This contradicts the documented contract of \
        `transform`/`Resumption` (\"recomputes just the delta\"), and the module's own \
        stated goal of generalizing Lwd_seq (whose reducer DOES only recompute the \
        changed element under an equivalent rebuilt-spine/reused-leaves pattern -- see \
        test_lwd_seq.ml's incrementality test). See FINDINGS.md for the root-cause \
        analysis (populate_cache's reuse lookup is gated by the enclosing trace's own \
        root identity, which is unavoidably fresh on every genuine change because a \
        parent's cached [value] field is immutable)."
    (fun () ->
       let m, calls = make_counting_sum_map () in
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 in
       (* deep, left-leaning chain: l1/l2/l3 are as far as possible from the
          single leaf that's about to change *)
       let t1 = node (node l1 l2) l3 in
       assert (f t1 = 6);
       calls := 0;
       let l3' = leaf 30 in
       let t2 = node (node l1 l2) l3' in
       assert (f t2 = 33);
       (* if incrementality worked as documented, only l3' (and the 2
          ancestor Node wrappers on its path to the root) would be
          recomputed: 3 calls. Instead, everything is recomputed: 5 calls
          (2 Node + Leaf 1 + Leaf 2 + Leaf 30), including l1 and l2 which
          are completely unrelated to the change and were passed in as the
          exact same physical Lwd_alg.t values as before. *)
       assert (!calls = 5))

let () =
  test "the same full-recompute happens through Resumption.of_map/.next, not just \
        `transform` (rules out a transform-specific glue bug)"
    (fun () ->
       let m, calls = make_counting_sum_map () in
       let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 and l4 = leaf 4 in
       let t1 = node (node (node l1 l2) l3) l4 in
       let r1 = Lwd_alg.Resumption.of_map m t1 in
       assert (r1.result = 10);
       calls := 0;
       let l4' = leaf 40 in
       let t2 = node (node (node l1 l2) l3) l4' in
       let r2 = r1.next t2 in
       assert (r2.result = 46);
       (* documented expectation: only l4' + the single ancestor Node on its
          path to the root should be recomputed (2 calls). Observed: every
          node is recomputed (7 calls: 3 Node + 4 Leaf). *)
       assert (!calls = 7))

let () =
  test "reusing an intermediate (non-leaf) wrapper unchanged does not help either: the \
        cache lookup is gated by the OUTERMOST recorded trace's root identity, not by \
        how deep the actual physical sharing goes"
    (fun () ->
       let m, calls = make_counting_sum_map () in
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 in
       let inner = node l1 l2 in
       let l3 = leaf 3 in
       let t1 = node inner l3 in
       assert (f t1 = 6);
       calls := 0;
       let l3' = leaf 30 in
       (* `inner` is reused as the exact same physical value; only the outer
          Node and l3 are new *)
       let t2 = node inner l3' in
       assert (f t2 = 33);
       (* if reuse were keyed off physical identity independently of
          ancestry, this would be 2 calls (outer Node + l3'). Observed: 5
          (outer Node + inner Node + l1 + l2 + l3'), i.e. `inner` being
          untouched bought nothing. *)
       assert (!calls = 5))

let () =
  test "finalize is never invoked in the full-recompute scenario above: dropped leaves \
        are silently abandoned rather than explicitly finalized, since populate_cache \
        never got a chance to register them as \"reused\" in the first place. This \
        matters if `finalize` is used for resource cleanup (e.g. releasing something \
        associated with a transformed value): those resources would never be released \
        via this hook when a whole subtree is (unnecessarily) recomputed."
    (fun () ->
       let finalized = ref [] in
       let m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
       Lwd_alg.define m
         ~finalize:(fun t out ->
             match t with
             | Leaf n -> finalized := (n, out) :: !finalized
             | Node _ -> ())
         (fun tape t ->
            match t with
            | Leaf n -> n
            | Node (l, r) -> Lwd_alg.apply tape m l + Lwd_alg.apply tape m r);
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 in
       assert (f (node (node l1 l2) l3) = 6);
       let l2' = leaf 20 in
       assert (f (node (node l1 l2') l3) = 24);
       assert (!finalized = []))

let () =
  test "undefined: calling apply/transform on a map that was never `define`d raises Failure"
    (fun () ->
       let m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
       let t = leaf 1 in
       match Lwd_alg.transform m t with
       | _ -> failwith "expected Failure"
       | exception Failure _ -> ())

let () =
  test "define: calling define twice on the same map raises Invalid_argument" (fun () ->
      let m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
      Lwd_alg.define m (fun _ t -> match t with Leaf n -> n | Node _ -> 0);
      match Lwd_alg.define m (fun _ t -> match t with Leaf n -> n | Node _ -> 0) with
      | () -> failwith "expected Invalid_argument"
      | exception Invalid_argument _ -> ())

let () =
  test "a map created directly with `map` (not `undefined`/`define`) works immediately, \
        without needing define" (fun () ->
      let m = Lwd_alg.map (fun _ t -> match t with Leaf n -> n * 2 | Node _ -> 0) in
      assert (Lwd_alg.transform m (leaf 21) = 42))

let () =
  test "an exception raised by map.func during a delta transform propagates to the \
        caller, and the transformer is still usable afterwards (cleanup ran)"
    (fun () ->
       let boom = ref false in
       let m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
       Lwd_alg.define m (fun tape t ->
           match t with
           | Leaf n -> if !boom && n = 99 then failwith "boom" else n
           | Node (l, r) -> Lwd_alg.apply tape m l + Lwd_alg.apply tape m r);
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 in
       assert (f (node l1 l2) = 3);
       boom := true;
       let l1' = leaf 99 in
       (try
          ignore (f (node l1' l2));
          failwith "expected exception"
        with Failure msg -> assert (msg = "boom"));
       boom := false;
       (* the transformer must still be usable for a subsequent, non-raising input *)
       let l1'' = leaf 5 in
       assert (f (node l1'' l2) = 7))

let () =
  test "two independent transform streams sharing the SAME underlying map stay correct \
        as long as calls are not literally interleaved/reentrant (map.slot is a single \
        transient mutable field, consumed synchronously within one apply call)"
    (fun () ->
       let m, _ = make_counting_sum_map () in
       let fa = Lwd_alg.transform m and fb = Lwd_alg.transform m in
       let a1 = leaf 1 and a2 = leaf 2 in
       let b1 = leaf 100 and b2 = leaf 200 in
       assert (fa (node a1 a2) = 3);
       assert (fb (node b1 b2) = 300);
       let a2' = leaf 20 in
       assert (fa (node a1 a2') = 21);
       let b2' = leaf 2000 in
       assert (fb (node b1 b2') = 2100))

let init = ()
