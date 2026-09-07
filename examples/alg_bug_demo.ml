(* Minimal repro for the Lwd_alg incrementality bug, now FIXED (see
   FINDINGS.md and lib/lwd/lwd_alg.ml's populate_cache/clear_cache).

   Per lwd_alg.mli's documented contract, after changing a single leaf, only
   that leaf and its ancestors on the path to the root should be recomputed.
   That used to not hold (every node in the tree was recomputed, including
   ones completely unrelated to the change); this now demonstrates the fix. *)

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

(* A "sum" transformation that prints every leaf it actually visits. *)
let sum_map : (tree, int) Lwd_alg.map = Lwd_alg.undefined ()

let () =
  Lwd_alg.define sum_map (fun tape t ->
      match t with
      | Leaf n ->
        Printf.printf "  recomputing Leaf %d\n%!" n;
        n
      | Node (l, r) ->
        Printf.printf "  recomputing Node\n%!";
        Lwd_alg.apply tape sum_map l + Lwd_alg.apply tape sum_map r)

let () =
  let f = Lwd_alg.transform sum_map in
  let l1 = leaf 1 and l2 = leaf 2 and l3 = leaf 3 in
  let l12 = node l1 l2 in
  let t1 = node l12 l3 in

  print_endline "== round 1: initial computation (everything is new, this part is expected) ==";
  Printf.printf "sum = %d\n\n%!" (f t1);

  (* Change ONLY l3, but this time reuse l12 = node l1 l2 wholesale, as the
     exact same physical Lwd_alg.t value -- not just its leaves. *)
  let l3' = leaf 30 in
  let t2 = node l12 l3' in

  print_endline "== round 2: only l3 changed, l12 = (node l1 l2) reused wholesale ==";
  print_endline "expected (and, since the fix, actual): only \"recomputing Leaf 30\" and";
  print_endline "the outer Node -- l12's own Node computation, and l1/l2, are not touched:";
  Printf.printf "sum = %d\n%!" (f t2)
