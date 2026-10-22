(* Stress tests for [Lwd_seq]: the balanced sequence type, its reactive
   fold/map/filter/sort_uniq combinators, and the low-level [Reducer]
   that makes them incremental.

   Two different things are worth checking, and they need different
   techniques:
   - CORRECTNESS: does the reactive result always match a naive,
     from-scratch computation on the equivalent list? This is checked with
     a model-based random test, regenerating a fresh [Lwd_seq.t] from a
     mutated list at every step (see [random_list_walk]).
   - INCREMENTALITY: the module promises O(1) bookkeeping per change,
     relying on *physical* sharing of unchanged sub-sequences between
     successive versions. [of_list] always allocates fresh leaves, so it
     can never exercise this path -- a dedicated test builds two versions
     of a sequence that share all but one leaf, and checks the map
     function is only re-invoked for the changed leaf (see
     [test "incrementality: ..."]). *)

open Framework

(* --- round-trip conversions --- *)

let pp_int_list l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

let () =
  test "of_list/to_list roundtrip (various lengths)" (fun () ->
      List.iter
        (fun l ->
           check_eq (Printf.sprintf "roundtrip %s" (pp_int_list l)) pp_int_list l
             (Lwd_seq.to_list (Lwd_seq.of_list l)))
        [ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3; 4; 5; 6; 7 ]; List.init 500 Fun.id ])

let () =
  test "of_array/to_array roundtrip" (fun () ->
      let a = [| 1; 2; 3; 4; 5 |] in
      check_eq "roundtrip" (fun a -> pp_int_list (Array.to_list a)) a
        (Lwd_seq.to_array (Lwd_seq.of_array a)))

let () =
  test "transform_list is a reactive-free concat-map" (fun () ->
      let l = [ 1; 2; 3 ] in
      let seq = Lwd_seq.transform_list l (fun x -> Lwd_seq.of_list [ x; x * 10 ]) in
      check_eq "concat-map" pp_int_list [ 1; 10; 2; 20; 3; 30 ] (Lwd_seq.to_list seq))

let () =
  test "empty sequence: view is Empty, to_list is []" (fun () ->
      assert (Lwd_seq.view Lwd_seq.empty = Empty);
      assert (Lwd_seq.to_list Lwd_seq.empty = []))

(* --- reactive combinators vs. a naive model, across random edits --- *)

type list_action =
  | LInsert of int * int (* index, value *)
  | LRemove of int
  | LReplace of int * int
  | LClear

let gen_value = QCheck.Gen.(1 -- 200)

let gen_action =
  QCheck.Gen.(
    oneof_weighted
      [ (3, map2 (fun i v -> LInsert (i, v)) nat gen_value);
        (2, map (fun i -> LRemove i) nat);
        (2, map2 (fun i v -> LReplace (i, v)) nat gen_value);
        (1, return LClear) ])

let gen_scenario = QCheck.Gen.list_size QCheck.Gen.(1 -- 50) gen_action

let apply_action l = function
  | LInsert (i, v) ->
    if l = [] then [ v ]
    else
      let i = i mod (List.length l + 1) in
      let before = List.filteri (fun j _ -> j < i) l in
      let after = List.filteri (fun j _ -> j >= i) l in
      before @ (v :: after)
  | LRemove i -> if l = [] then l else List.filteri (fun j _ -> j <> i mod List.length l) l
  | LReplace (i, v) ->
    if l = [] then l else List.mapi (fun j x -> if j = i mod List.length l then v else x) l
  | LClear -> []

let string_of_action = function
  | LInsert (i, v) -> Printf.sprintf "Insert(%d,%d)" i v
  | LRemove i -> Printf.sprintf "Remove %d" i
  | LReplace (i, v) -> Printf.sprintf "Replace(%d,%d)" i v
  | LClear -> "Clear"

(* Runs [actions] against both a plain list model and a reactive pipeline
   built from [make_pipeline], comparing [Lwd_seq.to_list] of the pipeline's
   output to [expect] applied to the model list, at every step. *)
let run_reactive_scenario ~name ~make_pipeline ~expect actions =
  let model = ref [] in
  let v = Lwd.var (Lwd_seq.of_list !model) in
  let observed = make_pipeline (Lwd.get v) in
  let o = Lwd.observe observed in
  List.iteri
    (fun step action ->
       model := apply_action !model action;
       Lwd.set v (Lwd_seq.of_list !model);
       let expected = expect !model in
       let actual = Lwd_seq.to_list (Lwd.quick_sample o) in
       check_eq
         (Printf.sprintf "%s: step %d after %s" name step (string_of_action action))
         pp_int_list expected actual)
    actions;
  Lwd.quick_release o

let qcheck_map =
  QCheck.Test.make ~count:200 ~name:"Lwd_seq.map matches List.map under random edits"
    (QCheck.make gen_scenario ~print:(fun a -> String.concat ";" (List.map string_of_action a)))
    (fun actions ->
       run_reactive_scenario ~name:"map"
         ~make_pipeline:(Lwd_seq.map (fun x -> x * 2))
         ~expect:(List.map (fun x -> x * 2))
         actions;
       true)

let qcheck_filter =
  QCheck.Test.make ~count:200 ~name:"Lwd_seq.filter matches List.filter under random edits"
    (QCheck.make gen_scenario ~print:(fun a -> String.concat ";" (List.map string_of_action a)))
    (fun actions ->
       run_reactive_scenario ~name:"filter"
         ~make_pipeline:(Lwd_seq.filter (fun x -> x mod 2 = 0))
         ~expect:(List.filter (fun x -> x mod 2 = 0))
         actions;
       true)

let qcheck_filter_map =
  QCheck.Test.make ~count:200 ~name:"Lwd_seq.filter_map matches List.filter_map under random edits"
    (QCheck.make gen_scenario ~print:(fun a -> String.concat ";" (List.map string_of_action a)))
    (fun actions ->
       run_reactive_scenario ~name:"filter_map"
         ~make_pipeline:(Lwd_seq.filter_map (fun x -> if x mod 3 = 0 then Some (x / 3) else None))
         ~expect:(List.filter_map (fun x -> if x mod 3 = 0 then Some (x / 3) else None))
         actions;
       true)

let qcheck_fold_monoid =
  QCheck.Test.make ~count:200 ~name:"Lwd_seq.fold_monoid (sum) matches naive sum under random edits"
    (QCheck.make gen_scenario ~print:(fun a -> String.concat ";" (List.map string_of_action a)))
    (fun actions ->
       let model = ref [] in
       let v = Lwd.var (Lwd_seq.of_list !model) in
       let observed = Lwd_seq.fold_monoid (fun x -> x) (0, ( + )) (Lwd.get v) in
       let o = Lwd.observe observed in
       List.iter
         (fun action ->
            model := apply_action !model action;
            Lwd.set v (Lwd_seq.of_list !model);
            let expected = List.fold_left ( + ) 0 !model in
            let actual = Lwd.quick_sample o in
            if expected <> actual then failwith (Printf.sprintf "expected %d got %d" expected actual))
         actions;
       Lwd.quick_release o;
       true)

let () =
  test "lwd_seq: map/filter/filter_map/fold_monoid agree with naive list operations \
        under random insert/remove/replace/clear sequences (property tests, 200 cases each)"
    (fun () ->
       qcheck_test ~seed:1 "seq map" qcheck_map;
       qcheck_test ~seed:2 "seq filter" qcheck_filter;
       qcheck_test ~seed:3 "seq filter_map" qcheck_filter_map;
       qcheck_test ~seed:4 "seq fold_monoid" qcheck_fold_monoid)

(* --- incrementality: physically-shared sub-sequences should not be
       re-processed --- *)

let () =
  test "incrementality: changing one element out of many only re-invokes the mapper \
        for that element (physically shared leaves are reused, not recomputed)"
    (fun () ->
       let n = 50 in
       let leaves = Array.init n (fun i -> Lwd_seq.element i) in
       let build leaves =
         Array.fold_left (fun acc l -> Lwd_seq.concat acc l) Lwd_seq.empty leaves
       in
       let v = Lwd.var (build leaves) in
       let calls = Hashtbl.create 16 in
       let observed =
         Lwd_seq.map
           (fun x ->
              Hashtbl.replace calls x (1 + try Hashtbl.find calls x with Not_found -> 0);
              x * 2)
           (Lwd.get v)
       in
       let o = Lwd.observe observed in
       let r1 = Lwd_seq.to_list (Lwd.quick_sample o) in
       check_eq "round 1 result" pp_int_list (List.init n (fun i -> i * 2)) r1;
       check_eq "round 1: every element mapped exactly once" string_of_int n (Hashtbl.length calls);
       Hashtbl.reset calls;
       (* Round 2: rebuild the tree, reusing every leaf EXCEPT index 10,
          which is replaced by a brand new leaf with a different value. *)
       leaves.(10) <- Lwd_seq.element 999;
       Lwd.set v (build leaves);
       let r2 = Lwd_seq.to_list (Lwd.quick_sample o) in
       let expected2 = List.init n (fun i -> (if i = 10 then 999 else i) * 2) in
       check_eq "round 2 result" pp_int_list expected2 r2;
       check_eq "round 2: only the changed element is re-mapped" string_of_int 1
         (Hashtbl.fold (fun _ n acc -> acc + n) calls 0);
       assert (Hashtbl.mem calls 999);
       Lwd.quick_release o)

(* --- Balanced --- *)

let () =
  test "Balanced.concat preserves left-to-right order regardless of rebalancing" (fun () ->
      let open Lwd_seq.Balanced in
      let s = List.fold_left (fun acc i -> concat acc (element i)) empty (List.init 200 Fun.id) in
      check_eq "balanced order" pp_int_list (List.init 200 Fun.id) (Lwd_seq.to_list (s :> int Lwd_seq.t)))

let () =
  test "Balanced.concat stays within +-1 rank of a left/right leaning insertion, per its own \
        internal invariant (no exception raised while building a large, skewed sequence)"
    (fun () ->
       let open Lwd_seq.Balanced in
       (* left-leaning: concat one new element at a time on the right *)
       let s1 = List.fold_left (fun acc i -> concat acc (element i)) empty (List.init 2000 Fun.id) in
       (* right-leaning: concat one new element at a time on the left *)
       let s2 =
         List.fold_left (fun acc i -> concat (element i) acc) empty (List.init 2000 (fun i -> 1999 - i))
       in
       check_eq "left-leaning order" pp_int_list (List.init 2000 Fun.id) (Lwd_seq.to_list (s1 :> int Lwd_seq.t));
       check_eq "right-leaning order" pp_int_list (List.init 2000 Fun.id) (Lwd_seq.to_list (s2 :> int Lwd_seq.t)))

(* --- sort_uniq --- *)

let () =
  test "sort_uniq matches List.sort_uniq under random edits" (fun () ->
      let model = ref [] in
      let v = Lwd.var (Lwd_seq.of_list !model) in
      let observed = Lwd_seq.sort_uniq compare (Lwd.get v) in
      let o = Lwd.observe observed in
      let gen = QCheck.Gen.list_size QCheck.Gen.(1 -- 40) gen_action in
      let actions = QCheck.Gen.generate1 ~rand:(Random.State.make [| 7 |]) gen in
      List.iter
        (fun action ->
           model := apply_action !model action;
           Lwd.set v (Lwd_seq.of_list !model);
           let expected = List.sort_uniq compare !model in
           let actual = Lwd_seq.to_list (Lwd.quick_sample o) in
           check_eq
             (Printf.sprintf "sort_uniq after %s" (string_of_action action))
             pp_int_list expected actual)
        actions;
      Lwd.quick_release o)

let () =
  test "sort_uniq collapses cmp-equal elements to a single representative, keeping the \
        FIRST-inserted value rather than the most recent one"
    (fun () ->
       (* cmp only looks at the value modulo 10, so e.g. 3 and 13 are "equal" *)
       let cmp a b = compare (a mod 10) (b mod 10) in
       let v = Lwd.var (Lwd_seq.of_list [ 3; 13; 23 ]) in
       let observed = Lwd_seq.sort_uniq cmp (Lwd.get v) in
       let o = Lwd.observe observed in
       let result = Lwd_seq.to_list (Lwd.quick_sample o) in
       check_eq "collapses to one representative (the first one, 3)" pp_int_list [ 3 ] result;
       Lwd.quick_release o)

let init = ()
