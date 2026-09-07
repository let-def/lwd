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

(* The following four tests originally pinned a real bug (see FINDINGS.md,
   "Lwd_alg: incremental reuse only ever fires when the top-level input is
   entirely unchanged"): `populate_cache`'s reuse lookup was gated on the
   enclosing trace's own root identity, which is unavoidably fresh on every
   genuine edit because [Lwd_alg.t]'s [value] field is immutable, so any
   change anywhere defeated caching for the WHOLE tree. That's now fixed in
   lwd_alg.ml (`populate_cache`/`clear_cache` walk through the traces of
   dropped values, down to the first shared value, instead of stopping at
   the first non-reused parent), and
   these tests now confirm the fix: changing one leaf only recomputes that
   leaf and its ancestors on the path to the root. *)

let () =
  test "FIXED: changing a single leaf only recomputes that leaf and its \
        ancestors on the path to the root -- physically-unchanged sibling \
        subtrees are correctly reused, not recomputed"
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
       (* only l3' and its 2 ancestor Node wrappers are recomputed; l1 and l2
          (unrelated, physically reused) are not touched at all *)
       assert (!calls = 3))

let () =
  test "FIXED: the same delta-only recompute happens through Resumption.of_map/.next, \
        not just `transform`"
    (fun () ->
       let m, calls = make_counting_sum_map () in
       let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 and l4 = leaf 4 in
       (* `mid` is bound once and reused physically below -- writing the
          equivalent expression out twice would allocate two distinct
          Lwd_alg.t wrappers with the same logical shape, which the library
          has no way to tell apart from a genuine change (it only ever
          compares by physical identity, never structurally) *)
       let mid = node (node l1 l2) l3 in
       let t1 = node mid l4 in
       let r1 = Lwd_alg.Resumption.of_map m t1 in
       assert (r1.result = 10);
       calls := 0;
       let l4' = leaf 40 in
       let t2 = node mid l4' in
       let r2 = r1.next t2 in
       assert (r2.result = 46);
       (* only l4' + the single ancestor Node on its path to the root
          (the outermost one) are recomputed *)
       assert (!calls = 2))

let () =
  test "FIXED: reusing an intermediate (non-leaf) wrapper unchanged now correctly \
        avoids recomputing it and everything beneath it"
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
       (* only the outer Node + l3' are recomputed; `inner` (and l1, l2
          beneath it) are reused wholesale, without even being visited *)
       assert (!calls = 2))

let () =
  test "FIXED: finalize now fires for a leaf that is genuinely dropped (replaced by a \
        new one), even though it's nested under an ancestor that also had to be \
        rebuilt"
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
       (* l2 (value 2, output 2) was dropped in favor of l2'; l1 and l3 are
          still live (reused) and must not be finalized *)
       assert (!finalized = [ (2, 2) ]))

let () =
  test "FIXED: dropping a whole subtree (not just a single leaf) finalizes every leaf \
        underneath it, not just the ones directly touched"
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
       (* replace the whole (l1, l2) subtree with a fresh one; only l3 survives *)
       let l4 = leaf 4 and l5 = leaf 5 in
       assert (f (node (node l4 l5) l3) = 12);
       assert (List.sort compare !finalized = [ (1, 1); (2, 2) ]))

let () =
  test "two different maps applied to the SAME tree keep independent caches (a node's \
        cache list holds one entry per map, matched by identity, not overwritten)"
    (fun () ->
       let sum_m, sum_calls = make_counting_sum_map () in
       let prod_calls = ref 0 in
       let prod_m : (tree, int) Lwd_alg.map = Lwd_alg.undefined () in
       Lwd_alg.define prod_m (fun tape t ->
           incr prod_calls;
           match t with
           | Leaf n -> n
           | Node (l, r) -> Lwd_alg.apply tape prod_m l * Lwd_alg.apply tape prod_m r);
       let f_sum = Lwd_alg.transform sum_m in
       let f_prod = Lwd_alg.transform prod_m in
       let l1 = leaf 2 and l2 = leaf 3 and l3 = leaf 4 in
       let mid = node l1 l2 in
       let t1 = node mid l3 in
       assert (f_sum t1 = 9);
       assert (f_prod t1 = 24);
       sum_calls := 0;
       prod_calls := 0;
       let l3' = leaf 40 in
       let t2 = node mid l3' in
       assert (f_sum t2 = 45);
       assert (f_prod t2 = 240);
       (* both maps independently only recompute the changed path (2 calls
          each: the outer Node + l3'), regardless of the other map's cache *)
       assert (!sum_calls = 2);
       assert (!prod_calls = 2))

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

(* --- traversal cost: an edit must cost O(delta), not O(size of the tree) ---

   Counting map.func invocations only shows what was recomputed, not what was
   traversed to find out. Walking the whole old trace tree on every edit
   recomputes the right things while costing O(n) per edit, as does marking
   below the frontier of shared values. The fold calls (marking) and the
   allocated words (populating the caches allocates one cell per visited
   trace) give a deterministic account of that cost. *)

let fold_calls = ref 0

let counting_fold_tree : tree Lwd_alg.foldable =
 fun kind acc t -> incr fold_calls; fold_tree kind acc t

let cleaf n = Lwd_alg.make counting_fold_tree (Leaf n)
let cnode l r = Lwd_alg.make counting_fold_tree (Node (l, r))

(* A complete tree of the given depth, with a function rebuilding its
   leftmost spine around a fresh leaf. *)
let rec complete depth =
  if depth = 0 then cleaf 0 else cnode (complete (depth - 1)) (complete (depth - 1))

let rec edit_leftmost v t =
  match Lwd_alg.peek t with
  | Leaf _ -> cleaf v
  | Node (l, r) -> cnode (edit_leftmost v l) r

(* Average (fold calls, minor words) per single-leaf edit *)
let edit_cost depth =
  let m, calls = make_counting_sum_map () in
  let f = Lwd_alg.transform m in
  let t = ref (complete depth) in
  ignore (f !t);
  let rounds = 64 in
  fold_calls := 0;
  calls := 0;
  let words = Gc.minor_words () in
  for i = 1 to rounds do
    t := edit_leftmost i !t;
    assert (f !t = i)
  done;
  let words = Gc.minor_words () -. words in
  assert (!calls = rounds * (depth + 1));
  (!fold_calls / rounds, int_of_float words / rounds)

let () =
  test "a single-leaf edit visits O(depth) nodes, not O(size of the tree)"
    (fun () ->
       let small_folds, small_words = edit_cost 6 in
       let large_folds, large_words = edit_cost 16 in
       (* 1024x more leaves, 17/7 times deeper: the cost must follow the
          depth. A walk over the whole tree would multiply it by ~1000. *)
       if large_folds > 4 * small_folds || large_words > 4 * small_words then
         failwith
           (Printf.sprintf
              "edit cost does not scale with depth: \
               depth 6: %d folds, %d words; depth 16: %d folds, %d words"
              small_folds small_words large_folds large_words))

let () =
  test "transforming the same value again does not traverse it" (fun () ->
      let m, calls = make_counting_sum_map () in
      let f = Lwd_alg.transform m in
      let t = complete 10 in
      ignore (f t);
      fold_calls := 0;
      calls := 0;
      ignore (f t);
      assert (!calls = 0);
      (* only the root is visited, to clear its mark *)
      assert (!fold_calls <= 1))

let make_finalizing_sum_map () =
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
  (m, finalized)

let () =
  test "finalize is not called on the sub-computations of a reused computation"
    (fun () ->
       let m, finalized = make_finalizing_sum_map () in
       let f = Lwd_alg.transform m in
       let x = node (leaf 1) (leaf 2) in
       assert (f x = 3);
       (* the old root is now nested in the new one: reused, with l1 and l2 *)
       assert (f (node x (leaf 3)) = 6);
       assert (!finalized = []);
       (* and nested deeper, under a node which is dropped afterward *)
       assert (f (node (node x (leaf 4)) (leaf 5)) = 12);
       assert (List.sort compare !finalized = [ (3, 3) ]))

let () =
  test "an exception during a delta transform does not finalize the old computations, \
        which are still the ones the next transform starts from"
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
            | Leaf 99 -> failwith "boom"
            | Leaf n -> n
            | Node (l, r) -> Lwd_alg.apply tape m l + Lwd_alg.apply tape m r);
       let f = Lwd_alg.transform m in
       let l1 = leaf 1 and l2 = leaf 2 in
       assert (f (node l1 l2) = 3);
       (try ignore (f (node l2 (leaf 99))); failwith "expected exception"
        with Failure msg -> assert (msg = "boom"));
       assert (!finalized = []);
       assert (f (node (leaf 5) l2) = 7);
       assert (!finalized = [ (1, 1) ]))

(* --- randomized stress test: single-leaf edits on random tree shapes ---

   Rather than hand-picked shapes, generate a random tree, then repeatedly
   replace ONE random leaf (rebuilding only its ancestors, exactly like a
   real incremental caller would), checking on every round that:
   - the result matches a naive from-scratch sum
   - the number of map.func invocations is EXACTLY the depth of the edited
     leaf (its own recompute + one per ancestor up to the root) -- not more
     (that would mean unrelated work was redone) and not less (that would
     mean something was wrongly reused). *)

type htree =
  | HLeaf of { mutable cur : tree Lwd_alg.t; mutable value : int }
  | HNode of { l : htree; r : htree; mutable cur : tree Lwd_alg.t }

let cur_of = function HLeaf r -> r.cur | HNode r -> r.cur

let fresh_value = let c = ref 0 in fun () -> incr c; !c

let rec build_htree = function
  | `Leaf ->
    let v = fresh_value () in
    HLeaf { cur = leaf v; value = v }
  | `Node (a, b) ->
    let ha = build_htree a and hb = build_htree b in
    HNode { l = ha; r = hb; cur = node (cur_of ha) (cur_of hb) }

let rec count_leaves = function
  | HLeaf _ -> 1
  | HNode { l; r; _ } -> count_leaves l + count_leaves r

let rec to_list = function
  | HLeaf r -> [ r.value ]
  | HNode { l; r; _ } -> to_list l @ to_list r

(* Replaces the [idx]-th leaf (in-order) with a fresh value, mutating [.cur]
   along the path to the root. Returns the depth of the replaced leaf
   (number of nodes recomputed: itself + every ancestor). *)
let rec replace_leaf idx = function
  | HLeaf r ->
    let v = fresh_value () in
    r.cur <- leaf v;
    r.value <- v;
    1
  | HNode { l; r; _ } as self ->
    let nl = count_leaves l in
    let depth =
      if idx < nl then replace_leaf idx l else replace_leaf (idx - nl) r
    in
    (match self with
     | HNode t -> t.cur <- node (cur_of l) (cur_of r)
     | HLeaf _ -> assert false);
    depth + 1

let gen_shape =
  QCheck.Gen.(
    sized_size (0 -- 6) (fix (fun self n ->
        if n = 0 then return `Leaf
        else map2 (fun a b -> `Node (a, b)) (self (n / 2)) (self (n / 2)))))

let qcheck_random_edits =
  QCheck.Test.make ~count:200 ~name:"random single-leaf edits recompute exactly the path to the root"
    (QCheck.make (QCheck.Gen.pair gen_shape (QCheck.Gen.list_size QCheck.Gen.(1 -- 20) QCheck.Gen.nat)))
    (fun (shape, edits) ->
       let m, calls = make_counting_sum_map () in
       let f = Lwd_alg.transform m in
       let ht = build_htree shape in
       let n = count_leaves ht in
       assert (f (cur_of ht) = List.fold_left ( + ) 0 (to_list ht));
       List.iter
         (fun idx ->
            let idx = idx mod n in
            calls := 0;
            let expected_depth = replace_leaf idx ht in
            let expected_sum = List.fold_left ( + ) 0 (to_list ht) in
            let actual_sum = f (cur_of ht) in
            if actual_sum <> expected_sum then
              failwith
                (Printf.sprintf "sum mismatch: expected %d, got %d" expected_sum actual_sum);
            if !calls <> expected_depth then
              failwith
                (Printf.sprintf "recompute count mismatch at leaf %d: expected %d (tree depth), got %d"
                   idx expected_depth !calls))
         edits;
       true)

let () =
  test "randomized: single-leaf edits on random tree shapes always recompute exactly \
        the edited leaf's path to the root, never more, never less (property test, \
        200 random shapes x up to 20 edits each)"
    (fun () -> qcheck_test "lwd_alg random single-leaf edits" qcheck_random_edits)

let init = ()
