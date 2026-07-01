open Lwd

(* ================= TESTS ================= *)

let err_handler err =
  Lwd.default_unsafe_action_logger err;
  failwith "Unexpected unsafe action"

let () =
  Lwd.unsafe_action_logger := err_handler

let test_basic_reactivity () =
  let v = var 0 in
  let computed = ref 0 in
  let d = map (get v) ~f:(fun x -> incr computed; x * 2) in
  let rq = make_release_queue () in
  let root = observe d in
  let () = sample rq root |> fun r -> assert (r = 0) in
  assert (!computed = 1);
  (* Idempotent sampling: should not recompute *)
  let () = sample rq root |> fun r -> assert (r = 0) in
  assert (!computed = 1);
  (* Invalidation triggers recomputation *)
  set v 5;
  let () = sample rq root |> fun r -> assert (r = 10) in
  assert (!computed = 2);
  release rq root

let test_bind_dynamic_selection () =
  let idx = var 0 in
  let vals = [| var 10; var 20; var 30 |] in
  let d = bind (get idx) ~f:(fun i -> get vals.(i)) in
  let rq = make_release_queue () in
  let root = observe d in
  let () = sample rq root |> fun r -> assert (r = 10) in
  set idx 1;
  let () = sample rq root |> fun r -> assert (r = 20) in
  set vals.(1) 99;
  let () = sample rq root |> fun r -> assert (r = 99) in
  set idx 2;
  let () = sample rq root |> fun r -> assert (r = 30) in
  release rq root

let test_join_flattening () =
  let v = var (var 10) in
  let d = map (get v) ~f:(fun inner -> get inner) in
  let joined = join d in
  let rq = make_release_queue () in
  let root = observe joined in
  let () = sample rq root |> fun r -> assert (r = 10) in
  set v (var 20);
  let () = sample rq root |> fun r -> assert (r = 20) in
  release rq root

let test_set_inside_map () =
  let v1 = var 0 in
  let v2 = var 0 in
  let calls = ref 0 in
  let d = map (get v1) ~f:(fun x ->
    incr calls;
    set v2 (x * 2);
    x * 2) in
  let rq = make_release_queue () in
  let root = observe d in
  let () = sample rq root |> fun r -> assert (r = 0) in
  assert (!calls = 1);
  assert (peek v2 = 0);
  set v1 5;
  let () = sample rq root |> fun r -> assert (r = 10) in
  assert (!calls = 2);
  assert (peek v2 = 10);
  release rq root

let test_set_inside_bind () =
  let v1 = var 0 in
  let v2 = var 0 in
  let calls = ref 0 in
  let d = bind (get v1) ~f:(fun x ->
    incr calls;
    set v2 (x * 2);
    get v2) in
  let rq = make_release_queue () in
  let root = observe d in
  let () = sample rq root |> fun r -> assert (r = 0) in
  assert (!calls = 1);
  set v1 5;
  let () = sample rq root |> fun r -> assert (r = 10) in
  assert (!calls = 2);
  release rq root

let test_fix_stabilization () =
  let limit = 5 in
  let v = var 0 in
  let d = map (get v) ~f:(fun x ->
    if x < limit then (set v (x + 1); x + 1) else x) in
  let stabilized = fix d ~wrt:(get v) in
  let rq = make_release_queue () in
  let root = observe stabilized in
  let () = sample rq root |> fun r -> assert (r = limit) in
  assert (peek v = limit);
  release rq root

let test_fix_with_bind () =
  let mode = var false in
  let v = var 0 in
  let d =
    bind (get mode) ~f:begin function
      | true ->
        fix ~wrt:(get v) @@
        map (get v) ~f:(fun x -> if x < 3 then (set v (x+1); x+1) else x)
      | false ->
        get v
    end in
  let rq = make_release_queue () in
  let root = observe d in
  let () = sample rq root |> fun r -> assert (r = 0) in
  set mode true;
  let () = sample rq root |> fun r -> assert (r = 3) in
  set v 0;
  set mode false;
  let () = sample rq root |> fun r -> assert (r = 0) in
  release rq root

let test_sharing_memoization () =
  let v = var 1 in
  let calls = ref 0 in
  let d = map (get v) ~f:(fun x -> incr calls; x * 2) in
  let d2 = map2 d d ~f:(fun a b -> a + b) in
  let rq = make_release_queue () in
  let root = observe d2 in
  let () = sample rq root |> fun r -> assert (r = 4) in
  assert (!calls = 1); (* d computed once, shared across both args *)
  set v 2;
  let () = sample rq root |> fun r -> assert (r = 8) in
  assert (!calls = 2);
  release rq root

let test_app_vs_map2 () =
  let v1 = var 2 in
  let v2 = var 3 in
  let d1 = map2 (get v1) (get v2) ~f:(fun a b -> a + b) in
  let d2 = app (map (get v1) ~f:(fun a -> fun b -> a + b)) (get v2) in
  let rq = make_release_queue () in
  let root1 = observe d1 in
  let root2 = observe d2 in
  let () = sample rq root1 |> fun r -> assert (r = 5) in
  let () = sample rq root2 |> fun r -> assert (r = 5) in
  set v1 10;
  let () = sample rq root1 |> fun r -> assert (r = 13) in
  let () = sample rq root2 |> fun r -> assert (r = 13) in
  release rq root1;
  release rq root2

let test_multiple_roots () =
  let v = var 1 in
  let d1 = map (get v) ~f:(fun x -> x * 2) in
  let d2 = map (get v) ~f:(fun x -> x * 3) in
  let rq = make_release_queue () in
  let root1 = observe d1 in
  let root2 = observe d2 in
  let () = sample rq root1 |> fun r -> assert (r = 2) in
  let () = sample rq root2 |> fun r -> assert (r = 3) in
  set v 5;
  assert (is_damaged root1);
  assert (is_damaged root2);
  let () = sample rq root1 |> fun r -> assert (r = 10) in
  let () = sample rq root2 |> fun r -> assert (r = 15) in
  release rq root1;
  release rq root2

let test_quick_sample () =
  let v = var 0 in
  let d = map (get v) ~f:(fun x -> x * 2) in
  let root = observe d in
  let () = quick_sample root |> fun r -> assert (r = 0) in
  set v 7;
  let () = quick_sample root |> fun r -> assert (r = 14) in
  quick_release root

let test_fix_dirty_bit_clearance () =
  let state = var 0 in
  let steps = ref 0 in
  let invalidated = ref false in

  (* Inner document mutates `state` until it reaches 2 *)
  let d = map (get state) ~f:(fun st -> incr steps; if st < 2 then set state (st + 1); st) in
  let stabilized = fix d ~wrt:(get state) in
  let rq = make_release_queue () in
  let root = observe ~on_invalidate:(fun _ -> invalidated := true) stabilized in

  (* === Cycle 1 === *)
  let () = sample rq root |> fun r -> assert (r = 2) in
  assert (!steps = 3);          (* 0→1→2: 3 evaluations *)
  assert (not (is_damaged root));
  assert (not !invalidated);

  (* === Cycle 2 === *)
  set state 0;
  (* BUG CHECK: If `fix` didn't clear its dirty bit after cycle 1,
     invalidation propagation will see it as already dirty and stop.
     The root will remain undamaged, breaking reactivity. *)
  assert (is_damaged root);
  assert !invalidated;

  let () = sample rq root |> fun r -> assert (r = 2) in
  assert (!steps = 6);          (* 3 more evaluations *)
  assert (not (is_damaged root));
  release rq root

(** Simple test runner *)
let check name test =
  try
    test ();
    Printf.printf "[OK]   %s\n" name
  with exn ->
    Printf.printf "[FAIL] %s: %s\n" name (Printexc.to_string exn)

let run_all () =
  check "basic_reactivity_and_caching" test_basic_reactivity;
  check "bind_dynamic_graph_selection" test_bind_dynamic_selection;
  check "join_flattening_dynamic_documents" test_join_flattening;
  check "set_inside_map_unstable" test_set_inside_map;
  check "set_inside_bind_unstable" test_set_inside_bind;
  check "fix_stabilization_loop" test_fix_stabilization;
  check "fix_with_dynamic_bind" test_fix_with_bind;
  check "sharing_and_memoization" test_sharing_memoization;
  check "applicative_vs_map2_consistency" test_app_vs_map2;
  check "multiple_roots_independent_invalidations" test_multiple_roots;
  check "quick_sample_flushes_queue" test_quick_sample;
  check "fix_dirty_bit_clearance" test_fix_dirty_bit_clearance

let () = run_all ()
