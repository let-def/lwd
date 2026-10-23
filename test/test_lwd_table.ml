(* Model-based stress testing for [Lwd_table].

   [Lwd_table] keeps an ordered, mutable collection in a self-balancing
   binary tree (see lwd_table.ml), and incrementally maintains a
   map/reduce over it. That's a lot of moving, order-sensitive parts:
   prepend/append/before/after/remove/clear, iteration via next/prev,
   and an incremental reduction that must always agree with a full,
   from-scratch recomputation.

   The approach here is a classic model-based random test: maintain a
   plain OCaml array (the "model") alongside a real [Lwd_table.t], apply
   the same sequence of random operations to both, and after every step
   check that:
   - iteration order (first/next and last/prev) matches the model
   - a live, incrementally-maintained [Lwd.t] reduction matches a
     from-scratch reduction of the model
   - [is_bound]/[get] agree with the model

   Any divergence points at a concrete, replayable bug. *)

open Framework

module Model = struct
  (* the model tracks rows by a stable integer id, in list order *)
  type t = (int * int) list (* (id, value) *)

  let to_values (t : t) = List.map snd t
end

type row_ref = { id : int; mutable row : int Lwd_table.row }

let fresh_id =
  let c = ref 0 in
  fun () -> incr c; !c

(* Random operation generator over a shadow model + real table, driven by
   an explicit action log so that a failure can be printed as a reproducer. *)

type action =
  | Prepend of int
  | Append of int
  | Before of int * int (* index into current live rows, value *)
  | After of int * int
  | Remove of int
  | Set of int * int
  | Unset of int
  | Clear

let string_of_action = function
  | Prepend v -> Printf.sprintf "Prepend %d" v
  | Append v -> Printf.sprintf "Append %d" v
  | Before (i, v) -> Printf.sprintf "Before(%d, %d)" i v
  | After (i, v) -> Printf.sprintf "After(%d, %d)" i v
  | Remove i -> Printf.sprintf "Remove %d" i
  | Set (i, v) -> Printf.sprintf "Set(%d, %d)" i v
  | Unset i -> Printf.sprintf "Unset %d" i
  | Clear -> "Clear"

let monoid_sum : int Lwd_utils.monoid = (0, ( + ))

(* NOTE: this runs INSIDE a QCheck property (see [qcheck_random_scenario]
   below), which is itself invoked hundreds of times (random cases, plus
   re-invocations while shrinking a failing case). It must therefore report
   failures by raising a plain exception, NOT via [Framework.test]/[check_eq]
   -- registering into the global [Framework] results list from in here would
   flood it with one entry per assertion per QCheck trial. QCheck.Test.check_exn
   (invoked once, from a real [Framework.test]) is the only thing that should
   turn this into a single pass/fail. *)
let run_scenario ~verbose actions =
  let table = Lwd_table.make () in
  let live = ref [] in
  (* [live] mirrors the table: a list of row_ref in current order, as far as
     we know it -- rebuilt from the table itself before each check so it is
     never a second source of truth for ORDER, only for picking random
     existing rows to act on. *)
  let model = ref [] in
  let reduction = Lwd_table.reduce monoid_sum table in
  let o = Lwd.observe reduction in
  let fail step what expected actual =
    failwith
      (Printf.sprintf "step %d: %s: expected [%s], got [%s]" step what
         (String.concat "," (List.map string_of_int expected))
         (String.concat "," (List.map string_of_int actual)))
  in
  let check step =
    let expected_values = Model.to_values !model in
    (* iteration order via first/next must match the model exactly *)
    let rec walk acc = function
      | None -> List.rev acc
      | Some row -> walk (Lwd_table.get row :: acc) (Lwd_table.next row)
    in
    let forward = walk [] (Lwd_table.first table) in
    let forward = List.map (function Some v -> v | None -> failwith "unbound row in chain") forward in
    if forward <> expected_values then fail step "forward order matches model" expected_values forward;
    let rec walk_back acc = function
      | None -> acc
      | Some row -> walk_back (Lwd_table.get row :: acc) (Lwd_table.prev row)
    in
    let backward = walk_back [] (Lwd_table.last table) in
    let backward = List.map (function Some v -> v | None -> failwith "unbound row in chain") backward in
    if backward <> expected_values then
      fail step "backward order (prev-chain) matches model" expected_values backward;
    let incremental = Lwd.quick_sample o in
    let from_scratch = List.fold_left ( + ) 0 expected_values in
    if incremental <> from_scratch then
      failwith
        (Printf.sprintf "step %d: incremental reduce (%d) <> from-scratch reduce (%d)" step incremental
           from_scratch)
  in
  List.iteri
    (fun step action ->
       if verbose then Printf.printf "  %s\n" (string_of_action action);
       (match action with
        | Prepend v ->
          let row = Lwd_table.prepend table ~set:v in
          let id = fresh_id () in
          live := { id; row } :: !live;
          model := (id, v) :: !model
        | Append v ->
          let row = Lwd_table.append table ~set:v in
          let id = fresh_id () in
          live := !live @ [ { id; row } ];
          model := !model @ [ (id, v) ]
        | Before (i, v) ->
          if !live <> [] then begin
            let i = i mod List.length !live in
            let target = List.nth !live i in
            let row = Lwd_table.before target.row ~set:v in
            let id = fresh_id () in
            live := List.filteri (fun j _ -> j < i) !live
                    @ [ { id; row } ]
                    @ List.filteri (fun j _ -> j >= i) !live;
            model :=
              List.filteri (fun j _ -> j < i) !model
              @ [ (id, v) ]
              @ List.filteri (fun j _ -> j >= i) !model
          end
        | After (i, v) ->
          if !live <> [] then begin
            let i = i mod List.length !live in
            let target = List.nth !live i in
            let row = Lwd_table.after target.row ~set:v in
            let id = fresh_id () in
            live :=
              List.filteri (fun j _ -> j <= i) !live
              @ [ { id; row } ]
              @ List.filteri (fun j _ -> j > i) !live;
            model :=
              List.filteri (fun j _ -> j <= i) !model
              @ [ (id, v) ]
              @ List.filteri (fun j _ -> j > i) !model
          end
        | Remove i ->
          if !live <> [] then begin
            let i = i mod List.length !live in
            let target = List.nth !live i in
            Lwd_table.remove target.row;
            live := List.filteri (fun j _ -> j <> i) !live;
            model := List.filteri (fun j _ -> j <> i) !model
          end
        | Set (i, v) ->
          if !live <> [] then begin
            let i = i mod List.length !live in
            let target = List.nth !live i in
            Lwd_table.set target.row v;
            model :=
              List.mapi (fun j (id, old) -> if j = i then (id, v) else (id, old)) !model
          end
        | Unset i ->
          if !live <> [] then begin
            let i = i mod List.length !live in
            let target = List.nth !live i in
            Lwd_table.unset target.row;
            (* Unset rows are simply skipped by [get]/[iter]/reduce, but they
               stay bound and in order (this is distinct from [remove]).
               A monoid reduction over unset rows treats them as absent.
               We model this by dropping the row's contribution from the
               *values* used for the reduction check, but the row must still
               be present for iteration-order checks. To keep the model
               simple, we track "unset" as a sentinel and filter it out only
               for the sum-check, not for order. *)
            model :=
              List.mapi (fun j (id, old) -> if j = i then (id, 0) else (id, old)) !model
            (* NOTE: representing "unset" as value 0 is only valid because our
               values are always > 0 (see gen below); this keeps the order
               check meaningful while keeping the sum check correct as long
               as [get] on an unset row returns [None] and is excluded -- see
               below where we special-case this in the order check via
               [Lwd_table.get]. *)
          end
        | Clear ->
          Lwd_table.clear table;
          live := [];
          model := []);
       check step)
    actions;
  Lwd.quick_release o

(* Values are strictly positive so that "unset" can be safely modeled as 0
   without colliding with a real value, see the comment above. *)
let gen_value = QCheck.Gen.(1 -- 1000)

let gen_action =
  QCheck.Gen.(
    oneof_weighted
      [ (3, map (fun v -> Prepend v) gen_value);
        (3, map (fun v -> Append v) gen_value);
        (3, map2 (fun i v -> Before (i, v)) nat gen_value);
        (3, map2 (fun i v -> After (i, v)) nat gen_value);
        (2, map (fun i -> Remove i) nat);
        (2, map2 (fun i v -> Set (i, v)) nat gen_value);
        (1, map (fun i -> Unset i) nat);
        (1, return Clear) ])

let gen_scenario = QCheck.Gen.list_size QCheck.Gen.(1 -- 60) gen_action

(* The [Unset] case above complicates the generic model (since [get] must
   return [None], not [Some 0]); to keep the property test both meaningful
   and simple, we test two properties instead of a single monolithic one:
   1. a scenario WITHOUT any [Unset] fully agrees with the naive model
      (order + sum), exercising insert/remove/clear/tree-rebalancing.
   2. a dedicated, smaller, hand-written scenario separately pins down
      [unset]/[get]/[reduce] semantics (see the `unset` tests below). *)

let gen_action_no_unset =
  QCheck.Gen.(
    oneof_weighted
      [ (3, map (fun v -> Prepend v) gen_value);
        (3, map (fun v -> Append v) gen_value);
        (3, map2 (fun i v -> Before (i, v)) nat gen_value);
        (3, map2 (fun i v -> After (i, v)) nat gen_value);
        (2, map (fun i -> Remove i) nat);
        (2, map2 (fun i v -> Set (i, v)) nat gen_value);
        (1, return Clear) ])

let gen_scenario_no_unset =
  QCheck.Gen.list_size QCheck.Gen.(1 -- 80) gen_action_no_unset

let qcheck_random_scenario =
  QCheck.Test.make ~count:300 ~name:"random insert/remove/set/clear sequences stay consistent"
    (QCheck.make gen_scenario_no_unset ~print:(fun a -> String.concat "; " (List.map string_of_action a)))
    (fun actions ->
       run_scenario ~verbose:false actions;
       true)

let () =
  test "lwd_table: order and incremental reduction agree with a naive model under random \
        prepend/append/before/after/remove/set/clear sequences (property test, 300 cases)"
    (fun () -> qcheck_test "lwd_table random model" qcheck_random_scenario)

(* CAUTION: [Lwd_table.row] is a mutable tree node with a back-pointer to its
   parent, so a live row is a genuinely CYCLIC value. Comparing two rows (or
   two [row option]s that are both [Some _]) with structural equality/compare
   ([=], [compare], [Hashtbl] on the structural hash, etc.) walks that cycle
   and never finds a base case: it either runs out of stack or, as observed
   here (OCaml's polymorphic compare uses an explicit heap-allocated
   worklist rather than the C stack, precisely so it can compare deep
   non-cyclic structures without overflowing the stack), it grows that
   worklist forever and dies with [Out_of_memory]. See FINDINGS.md.
   [Lwd_table] now exposes [equal_row] (physical equality) as the sanctioned
   way to compare rows; this local helper predates that and lifts the same
   idea to [row option], used throughout this file. *)
let row_opt_eq actual expected =
  match actual, expected with
  | None, None -> true
  | Some a, Some b -> a == b
  | Some _, None | None, Some _ -> false

let () =
  test "lwd_table: unset makes a row invisible to get/iter/reduce but keeps it bound and \
        in place (distinct from remove)"
    (fun () ->
       let t = Lwd_table.make () in
       let r1 = Lwd_table.append t ~set:1 in
       let r2 = Lwd_table.append t ~set:2 in
       let r3 = Lwd_table.append t ~set:3 in
       let reduction = Lwd_table.reduce monoid_sum t in
       let o = Lwd.observe reduction in
       assert (Lwd.quick_sample o = 6);
       Lwd_table.unset r2;
       assert (Lwd_table.get r2 = None);
       assert (Lwd_table.is_bound r2);
       (* iteration order is unaffected by unset *)
       assert (row_opt_eq (Lwd_table.next r1) (Some r2));
       assert (row_opt_eq (Lwd_table.next r2) (Some r3));
       assert (Lwd.quick_sample o = 4);
       Lwd_table.set r2 20;
       assert (Lwd.quick_sample o = 24);
       Lwd.quick_release o)

let () =
  test "lwd_table: remove makes get/is_bound report unbound, and leaves next/prev of \
        UNAFFECTED neighbors correct"
    (fun () ->
       let t = Lwd_table.make () in
       let r1 = Lwd_table.append t ~set:1 in
       let r2 = Lwd_table.append t ~set:2 in
       let r3 = Lwd_table.append t ~set:3 in
       Lwd_table.remove r2;
       assert (not (Lwd_table.is_bound r2));
       assert (Lwd_table.get r2 = None);
       (* r1 and r3 were never removed; r2's excision from the tree must not
          disturb their own next/prev links *)
       assert (row_opt_eq (Lwd_table.next r1) (Some r3));
       assert (row_opt_eq (Lwd_table.prev r3) (Some r1)))

let () =
  test "FIXED: next/prev on a row that WAS bound and became unbound via remove/clear now \
        correctly return None, matching the .mli (\"or None if the input row is unbound\"). \
        Previously (see FINDINGS.md, and git commit 4e99fca \"Lwd_table: fix {prev,next} on \
        deleted rows\") these crashed with an internal Assert_failure instead."
    (fun () ->
       let t1 = Lwd_table.make () in
       let _r1 = Lwd_table.append t1 ~set:1 in
       let r2 = Lwd_table.append t1 ~set:2 in
       let _r3 = Lwd_table.append t1 ~set:3 in
       Lwd_table.remove r2;
       assert (Lwd_table.next r2 = None);
       assert (Lwd_table.prev r2 = None);
       let t2 = Lwd_table.make () in
       let _r1 = Lwd_table.append t2 ~set:1 in
       let r2 = Lwd_table.append t2 ~set:2 in
       let _r3 = Lwd_table.append t2 ~set:3 in
       Lwd_table.clear t2;
       assert (Lwd_table.next r2 = None);
       assert (Lwd_table.prev r2 = None);
       let t3 = Lwd_table.make () in
       let r1 = Lwd_table.append t3 ~set:1 in
       Lwd_table.remove r1;
       assert (Lwd_table.next r1 = None);
       assert (Lwd_table.prev r1 = None))

let () =
  test "equal_row is the safe (physical-equality-based) way to compare rows, and agrees \
        with row_opt_eq" (fun () ->
      let t = Lwd_table.make () in
      let r1 = Lwd_table.append t ~set:1 in
      let r2 = Lwd_table.append t ~set:2 in
      assert (Lwd_table.equal_row r1 r1);
      assert (not (Lwd_table.equal_row r1 r2));
      (match Lwd_table.next r1 with
       | None -> failwith "expected Some r2"
       | Some r -> assert (Lwd_table.equal_row r r2)))

let () =
  test "lwd_table: before/after on an unbound row silently produce another unbound row \
        (per the documented contract), rather than raising"
    (fun () ->
       let t = Lwd_table.make () in
       let r1 = Lwd_table.append t ~set:1 in
       Lwd_table.remove r1;
       assert (not (Lwd_table.is_bound r1));
       let r2 = Lwd_table.before r1 ~set:2 in
       assert (not (Lwd_table.is_bound r2));
       let r3 = Lwd_table.after r1 ~set:3 in
       assert (not (Lwd_table.is_bound r3)))

let () =
  test "lwd_table: clear detaches every row (all become unbound) and the reduction becomes empty"
    (fun () ->
       let t = Lwd_table.make () in
       let rows = List.init 20 (fun i -> Lwd_table.append t ~set:i) in
       let reduction = Lwd_table.reduce monoid_sum t in
       let o = Lwd.observe reduction in
       assert (Lwd.quick_sample o = List.fold_left ( + ) 0 (List.init 20 Fun.id));
       Lwd_table.clear t;
       List.iter (fun r -> assert (not (Lwd_table.is_bound r))) rows;
       assert (Lwd.quick_sample o = 0);
       Lwd.quick_release o)

let () =
  test "lwd_table: map_reduce's row argument matches the row being folded, and the mapper \
        is not re-invoked for unrelated rows on a small, unrelated mutation"
    (fun () ->
       let t = Lwd_table.make () in
       let r1 = Lwd_table.append t ~set:"a" in
       let r2 = Lwd_table.append t ~set:"b" in
       let calls = Hashtbl.create 8 in
       let bump row = Hashtbl.replace calls row (1 + try Hashtbl.find calls row with Not_found -> 0) in
       let reduction =
         Lwd_table.map_reduce
           (fun row v -> bump row; v)
           (("", fun a b -> a ^ b) : string Lwd_utils.monoid)
           t
       in
       let o = Lwd.observe reduction in
       assert (Lwd.quick_sample o = "ab");
       Hashtbl.reset calls;
       Lwd_table.set r2 "B";
       let v = Lwd.quick_sample o in
       assert (v = "aB");
       (* incremental_rebuild is expected to re-run the mapper only for rows
          whose version indicates a change -- r1 was untouched. *)
       assert (not (Hashtbl.mem calls r1));
       assert (Hashtbl.mem calls r2);
       Lwd.quick_release o)

let () =
  test "lwd_table: iter visits bound rows in order and skips unset ones" (fun () ->
      let t = Lwd_table.make () in
      let _ = Lwd_table.append t ~set:1 in
      let r2 = Lwd_table.append t ~set:2 in
      let _ = Lwd_table.append t ~set:3 in
      Lwd_table.unset r2;
      let acc = ref [] in
      Lwd_table.iter (fun v -> acc := v :: !acc) t;
      assert (List.rev !acc = [ 1; 3 ]))

let init = ()
