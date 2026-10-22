(* Stress tests and invariant checks for the core [Lwd] module.

   These are organized as:
   - "expected behavior" tests, which pin down documented/intended semantics
     (sharing, lifecycle, evaluation order, etc.)
   - "known limitation" tests, which pin down surprising or under-documented
     behaviors discovered while stress-testing the library. They are not
     regressions to fix here (the core library is intentionally left
     untouched) -- they exist so that if the behavior ever changes, the
     change is visible, and so the behavior is written down somewhere.
     See FINDINGS.md for prose write-ups of the most notable ones.
*)

open Framework

let capture_unsafe f =
  let logged = ref [] in
  let saved = !Lwd.unsafe_action_logger in
  Lwd.unsafe_action_logger := (fun a -> logged := a :: !logged);
  Fun.protect ~finally:(fun () -> Lwd.unsafe_action_logger := saved) f;
  List.rev !logged

let () =
  test "basic: map over a var reflects updates" (fun () ->
      let x = Lwd.var 0 in
      let y = Lwd.map ~f:succ (Lwd.get x) in
      let o = Lwd.observe y in
      assert (Lwd.quick_sample o = 1);
      Lwd.set x 10;
      assert (Lwd.quick_sample o = 11);
      Lwd.quick_release o)

let () =
  test "is_pure: return/pure are pure, map over pure stays pure, map over var is not"
    (fun () ->
       assert (Lwd.is_pure (Lwd.return 1) = Some 1);
       assert (Lwd.is_pure (Lwd.pure 1) = Some 1);
       assert (Lwd.is_pure (Lwd.map ~f:succ (Lwd.pure 1)) = Some 2);
       let x = Lwd.var 0 in
       assert (Lwd.is_pure (Lwd.get x) = None);
       assert (Lwd.is_pure (Lwd.map ~f:succ (Lwd.get x)) = None))

let () =
  test "peek/update/may_update" (fun () ->
      let x = Lwd.var 10 in
      assert (Lwd.peek x = 10);
      Lwd.update (fun v -> v * 2) x;
      assert (Lwd.peek x = 20);
      Lwd.may_update (fun v -> if v > 100 then Some 0 else None) x;
      assert (Lwd.peek x = 20);
      Lwd.may_update (fun v -> Some (v + 1)) x;
      assert (Lwd.peek x = 21))

(* --- Sharing --- *)

let () =
  test "diamond: a var used twice in the same expression is read once per round"
    (fun () ->
       let x = Lwd.var 0 in
       let reads = ref 0 in
       let gx = Lwd.map ~f:(fun v -> incr reads; v) (Lwd.get x) in
       let y = Lwd.map2 gx gx ~f:(+) in
       let o = Lwd.observe y in
       assert (Lwd.quick_sample o = 0);
       assert (!reads = 1);
       Lwd.set x 5;
       assert (Lwd.quick_sample o = 10);
       assert (!reads = 2);
       Lwd.quick_release o)

let () =
  test "sharing: two roots observing the same subgraph share its cached value"
    (fun () ->
       let x = Lwd.var 0 in
       let reads = ref 0 in
       let gx = Lwd.map ~f:(fun v -> incr reads; v * 2) (Lwd.get x) in
       let o1 = Lwd.observe gx and o2 = Lwd.observe gx in
       assert (Lwd.quick_sample o1 = 0);
       assert (Lwd.quick_sample o2 = 0);
       assert (!reads = 1);
       Lwd.set x 10;
       assert (Lwd.quick_sample o1 = 20);
       assert (Lwd.quick_sample o2 = 20);
       assert (!reads = 2);
       Lwd.quick_release o1;
       Lwd.quick_release o2)

let () =
  test "trace fan-out: >4 observers of one node exercise the Tn representation"
    (fun () ->
       let x = Lwd.var 0 in
       let gx = Lwd.map ~f:succ (Lwd.get x) in
       let roots = List.init 10 (fun _ -> Lwd.observe gx) in
       List.iter (fun o -> assert (Lwd.quick_sample o = 1)) roots;
       Lwd.set x 100;
       List.iter (fun o -> assert (Lwd.quick_sample o = 101)) roots;
       List.iter Lwd.quick_release roots)

let () =
  test "trace fan-out: releasing some observers (Tn -> T4 downgrade) keeps the rest correct"
    (fun () ->
       let x = Lwd.var 0 in
       let gx = Lwd.map ~f:succ (Lwd.get x) in
       let roots = Array.of_list (List.init 10 (fun _ -> Lwd.observe gx)) in
       Array.iter (fun o -> ignore (Lwd.quick_sample o)) roots;
       (* release enough observers to force Tn's internal compaction/downgrade
          paths (see [sub_release] in lwd.ml), in a non-trivial order *)
       List.iter (fun i -> Lwd.quick_release roots.(i)) [ 3; 7; 0; 5; 8 ];
       Lwd.set x 200;
       Array.iteri
         (fun i o ->
            if not (List.mem i [ 3; 7; 0; 5; 8 ]) then
              assert (Lwd.quick_sample o = 201))
         roots;
       Array.iteri
         (fun i o -> if not (List.mem i [ 3; 7; 0; 5; 8 ]) then Lwd.quick_release o)
         roots)

let () =
  test "set has no equality short-circuit: re-setting the same value still recomputes"
    (fun () ->
       (* This pins current behavior: Lwd.set always invalidates, even if the
          new value is structurally/physically equal to the old one. There is
          no dedup based on equality. See FINDINGS.md. *)
       let x = Lwd.var 0 in
       let reads = ref 0 in
       let d = Lwd.map ~f:(fun v -> incr reads; v) (Lwd.get x) in
       let o = Lwd.observe d in
       ignore (Lwd.quick_sample o);
       assert (!reads = 1);
       Lwd.set x 0;
       ignore (Lwd.quick_sample o);
       assert (!reads = 2);
       Lwd.quick_release o)

(* --- Evaluation order --- *)

let () =
  test "FIXED: map2/pair/app evaluate their operands left to right, as documented. \
        They used to evaluate the right one first, an accident of OCaml's unspecified \
        argument evaluation order. See FINDINGS.md."
    (fun () ->
       (* [mk]'s base document must NOT be pure ([Lwd.return]/[Lwd.pure]):
          [Lwd.map] over a pure value is folded into a new pure value
          EAGERLY, at graph-construction time (see the [Pure vx -> Pure (f
          vx)] branch of [map] in lwd.ml), bypassing [sub_sample] entirely.
          An earlier version of this test used [Lwd.return] here and
          "passed" for the wrong reason: it was measuring OCaml's
          function-argument evaluation order at the call to [mk], not
          sub_sample's operand evaluation order at all. Using a
          [Lwd.var] keeps this a genuine [Operator] node whose [f] only
          runs later, during actual sampling. *)
       let trigger = Lwd.var () in
       let order = ref [] in
       let mk name v = Lwd.map ~f:(fun () -> order := name :: !order; v) (Lwd.get trigger) in
       let check_one label d =
         order := [];
         let o = Lwd.observe d in
         let v = Lwd.quick_sample o in
         Lwd.quick_release o;
         (v, List.rev !order, label)
       in
       let v, ord, label = check_one "map2" (Lwd.map2 (mk "L" 1) (mk "R" 2) ~f:( + )) in
       assert (v = 3 && ord = [ "L"; "R" ] && label = "map2");
       let v, ord, label = check_one "pair" (Lwd.pair (mk "L" 1) (mk "R" 2)) in
       assert (v = (1, 2) && ord = [ "L"; "R" ] && label = "pair");
       let v, ord, label =
         check_one "app" (Lwd.app (Lwd.map ~f:( + ) (mk "L" 1)) (mk "R" 2))
       in
       assert (v = 3 && ord = [ "L"; "R" ] && label = "app");
       let v, ord, label =
         check_one "nested"
           (Lwd.map2 ~f:( + )
              (Lwd.map2 (mk "A" 1) (mk "B" 2) ~f:( + ))
              (Lwd.app (Lwd.map ~f:( + ) (mk "C" 3)) (mk "D" 4)))
       in
       assert (v = 10 && ord = [ "A"; "B"; "C"; "D" ] && label = "nested"))

(* --- Lifecycle --- *)

let () =
  test "release before ever sampling does not crash" (fun () ->
      let x = Lwd.var 0 in
      let o = Lwd.observe (Lwd.get x) in
      Lwd.quick_release o)

let () =
  test "double release of the same root is idempotent" (fun () ->
      let x = Lwd.var 0 in
      let o = Lwd.observe (Lwd.get x) in
      ignore (Lwd.quick_sample o);
      Lwd.quick_release o;
      Lwd.quick_release o)

let () =
  test "a var can be set after its only observer is released, and re-observing recomputes"
    (fun () ->
       let x = Lwd.var 0 in
       let o = Lwd.observe (Lwd.map ~f:succ (Lwd.get x)) in
       assert (Lwd.quick_sample o = 1);
       Lwd.quick_release o;
       Lwd.set x 99;
       assert (Lwd.quick_sample o = 100);
       Lwd.quick_release o)

let () =
  test "is_damaged reflects cache state across sample/release cycles" (fun () ->
      let x = Lwd.var 0 in
      let o = Lwd.observe (Lwd.get x) in
      assert (Lwd.is_damaged o);
      ignore (Lwd.quick_sample o);
      assert (not (Lwd.is_damaged o));
      Lwd.set x 1;
      assert (Lwd.is_damaged o);
      Lwd.quick_release o)

(* --- Exceptions --- *)

let () =
  test "an exception raised while evaluating propagates, and the graph stays usable"
    (fun () ->
       let x = Lwd.var 0 in
       let d = Lwd.map ~f:(fun v -> if v = 0 then failwith "boom" else v * 2) (Lwd.get x) in
       let o = Lwd.observe d in
       (try
          ignore (Lwd.quick_sample o);
          failwith "expected exception"
        with Failure msg -> assert (msg = "boom"));
       ignore (capture_unsafe (fun () -> Lwd.set x 5));
       assert (Lwd.quick_sample o = 10);
       Lwd.quick_release o)

let () =
  test "FIXED: after an exception escapes evaluation, the root is reset to \
        Eval_none, so the NEXT (otherwise perfectly ordinary and unrelated) `set` \
        that invalidates it is no longer spuriously flagged `Mutation`. \
        See FINDINGS.md."
    (fun () ->
       let x = Lwd.var 0 in
       let d = Lwd.map ~f:(fun v -> if v = 0 then failwith "boom" else v * 2) (Lwd.get x) in
       let o = Lwd.observe d in
       (try ignore (Lwd.quick_sample o) with Failure _ -> ());
       assert (Lwd.is_damaged o);
       (* nothing is being evaluated right now: this `set` is entirely ordinary *)
       let logged = capture_unsafe (fun () -> Lwd.set x 5) in
       assert (logged = []);
       assert (Lwd.quick_sample o = 10);
       Lwd.quick_release o)

(* --- Exceptions: inner nodes left at Eval_progress ---

   When an exception escapes [sub_sample], every operator on the path from
   the raising node up to the root is left at [Eval_progress]. Only the root's
   state matters for the unsafe-mutation diagnostic; the tests below check
   that the inner nodes left in that state are harmless: values, invalidation
   callbacks, primitive acquire/release balance and the diagnostic must all
   behave as if those nodes had been reset to [Eval_none].

   They rely on the root itself being reset when sampling raises (see the
   "FIXED:" test above). *)

let sample_fails o =
  match Lwd.quick_sample o with
  | _ -> failwith "expected an exception"
  | exception Failure msg -> assert (msg = "boom")

let counting_root d =
  let invalidations = ref 0 in
  let o = Lwd.observe ~on_invalidate:(fun _ -> incr invalidations) d in
  o, invalidations

let no_unsafe f = assert (capture_unsafe f = [])

let () =
  test "exceptions: a deep chain left at Eval_progress recovers, recomputes once \
        per node, and is never flagged unsafe"
    (fun () ->
       let x = Lwd.var 0 in
       let calls = ref 0 in
       let step d = Lwd.map d ~f:(fun v -> incr calls; v + 1) in
       let leaf = Lwd.map (Lwd.get x) ~f:(fun v ->
           incr calls; if v = 0 then failwith "boom" else v) in
       let d = step (step (step leaf)) in
       let o, invalidations = counting_root d in
       sample_fails o;
       assert (!calls = 1);
       assert (Lwd.is_damaged o);
       (* every invalidation walks through stuck nodes up to the root *)
       no_unsafe (fun () -> Lwd.set x 0);
       no_unsafe (fun () -> Lwd.set x 1);
       (* the observer never received a value, so it is not notified *)
       assert (!invalidations = 0);
       calls := 0;
       assert (Lwd.quick_sample o = 4);
       assert (!calls = 4);
       (* back to normal: invalidation reaches the root and caching works *)
       no_unsafe (fun () -> Lwd.set x 2);
       assert (!invalidations = 1);
       calls := 0;
       assert (Lwd.quick_sample o = 5);
       assert (Lwd.quick_sample o = 5);
       assert (!calls = 4);
       Lwd.quick_release o)

let () =
  test "exceptions: failing twice in a row, then recovering, is never flagged unsafe"
    (fun () ->
       let x = Lwd.var 0 in
       let d = Lwd.map (Lwd.map (Lwd.get x) ~f:(fun v ->
           if v <= 0 then failwith "boom" else v)) ~f:succ in
       let o, invalidations = counting_root d in
       sample_fails o;
       no_unsafe (fun () -> Lwd.set x (-1));
       sample_fails o;
       no_unsafe (fun () -> Lwd.set x 1);
       assert (Lwd.quick_sample o = 2);
       (* and failing again after a success *)
       no_unsafe (fun () -> Lwd.set x 0);
       assert (!invalidations = 1);
       sample_fails o;
       no_unsafe (fun () -> Lwd.set x 3);
       assert (!invalidations = 1);
       assert (Lwd.quick_sample o = 4);
       Lwd.quick_release o)

let () =
  test "exceptions: a stuck node shared with another root is recomputed by that \
        root, and invalidations then reach both roots correctly"
    (fun () ->
       let x = Lwd.var 0 and fail = Lwd.var true in
       let calls = ref 0 in
       let shared = Lwd.map2 (Lwd.get x) (Lwd.get fail) ~f:(fun v fail ->
           incr calls; if fail then failwith "boom" else v) in
       let mid = Lwd.map shared ~f:(fun v -> v * 10) in
       let oa, inv_a = counting_root (Lwd.map mid ~f:succ) in
       let ob, inv_b = counting_root mid in
       sample_fails oa;
       no_unsafe (fun () -> Lwd.set fail false);
       (* [ob] recomputes the stuck [shared] and [mid] on its own *)
       assert (Lwd.quick_sample ob = 0);
       assert (!calls = 2);
       (* [oa] reuses what [ob] cached *)
       assert (Lwd.quick_sample oa = 1);
       assert (!calls = 2);
       no_unsafe (fun () -> Lwd.set x 7);
       assert (!inv_a = 1 && !inv_b = 1);
       assert (Lwd.quick_sample oa = 71);
       assert (Lwd.quick_sample ob = 70);
       assert (!calls = 3);
       Lwd.quick_release oa;
       Lwd.quick_release ob)

let () =
  test "exceptions: a stuck node with more than 4 parents (Tn trace) recovers"
    (fun () ->
       let x = Lwd.var 0 in
       let shared = Lwd.map (Lwd.get x) ~f:(fun v ->
           if v = 0 then failwith "boom" else v) in
       let roots = List.init 6 (fun i -> counting_root (Lwd.map shared ~f:(( + ) i))) in
       List.iter (fun (o, _) -> sample_fails o) roots;
       no_unsafe (fun () -> Lwd.set x 1);
       List.iteri (fun i (o, _) -> assert (Lwd.quick_sample o = i + 1)) roots;
       no_unsafe (fun () -> Lwd.set x 2);
       List.iteri (fun i (o, inv) ->
           assert (!inv = 1);
           assert (Lwd.quick_sample o = i + 2)) roots;
       List.iter (fun (o, _) -> Lwd.quick_release o) roots)

let () =
  test "exceptions: a fix whose body raises is left at Eval_progress; invalidating \
        it stops there, yet the root is still damaged and the fix is re-armed \
        after recovery"
    (fun () ->
       let x = Lwd.var 0 in
       let wrt = Lwd.var () in
       let body = Lwd.map2 (Lwd.get x) (Lwd.get wrt) ~f:(fun v () ->
           if v = 0 then failwith "boom" else v) in
       let o, invalidations = counting_root (Lwd.map (Lwd.fix body ~wrt:(Lwd.get wrt)) ~f:succ) in
       sample_fails o;
       (* reaches the stuck Fix node through [body], which resets it to
          Eval_none without propagating any further *)
       no_unsafe (fun () -> Lwd.set x 1);
       assert (Lwd.is_damaged o);
       assert (Lwd.quick_sample o = 2);
       no_unsafe (fun () -> Lwd.set x 2);
       assert (!invalidations = 1);
       assert (Lwd.quick_sample o = 3);
       no_unsafe (fun () -> Lwd.set wrt ());
       assert (!invalidations = 2);
       assert (Lwd.quick_sample o = 3);
       Lwd.quick_release o)

let () =
  test "exceptions: a bind whose selector raises is left at Eval_progress; an \
        invalidation of the previous branch is ignored by the Join, but the root \
        recovers and the old branch is released exactly once"
    (fun () ->
       let acquired = ref 0 and released = ref 0 in
       let branch_var = Lwd.var 10 in
       let old_branch =
         Lwd.map2 (Lwd.get branch_var)
           (Lwd.get_prim (Lwd.prim
                            ~acquire:(fun _ -> incr acquired; 0)
                            ~release:(fun _ _ -> incr released)))
           ~f:( + )
       in
       let sel = Lwd.var `Old in
       let d = Lwd.bind (Lwd.get sel) ~f:(function
           | `Old -> old_branch
           | `Fail -> failwith "boom"
           | `New -> Lwd.return 42) in
       let o, invalidations = counting_root d in
       assert (Lwd.quick_sample o = 10);
       no_unsafe (fun () -> Lwd.set sel `Fail);
       assert (!invalidations = 1);
       sample_fails o;
       (* the Join is still holding [old_branch] as its intermediate; this
          invalidation hits the Join while its child is at Eval_progress *)
       no_unsafe (fun () -> Lwd.set branch_var 20);
       assert (!invalidations = 1);
       assert (Lwd.is_damaged o);
       no_unsafe (fun () -> Lwd.set sel `Old);
       assert (Lwd.quick_sample o = 20);
       assert (!acquired = 1 && !released = 0);
       no_unsafe (fun () -> Lwd.set branch_var 30);
       assert (!invalidations = 2);
       assert (Lwd.quick_sample o = 30);
       no_unsafe (fun () -> Lwd.set sel `New);
       assert (Lwd.quick_sample o = 42);
       assert (!acquired = 1 && !released = 1);
       Lwd.quick_release o;
       assert (!acquired = 1 && !released = 1))

let () =
  test "exceptions: a bind whose selected branch raises keeps that branch \
        acquired; switching away releases its primitives exactly once"
    (fun () ->
       let acquired = ref 0 and released = ref 0 in
       let bad_branch =
         Lwd.map2
           (Lwd.get_prim (Lwd.prim
                            ~acquire:(fun _ -> incr acquired; 0)
                            ~release:(fun _ _ -> incr released)))
           (Lwd.return ())
           ~f:(fun _ () -> failwith "boom")
       in
       let sel = Lwd.var true in
       let d = Lwd.bind (Lwd.get sel) ~f:(fun bad ->
           if bad then bad_branch else Lwd.return 1) in
       let o, _ = counting_root d in
       sample_fails o;
       assert (!acquired = 1 && !released = 0);
       (* sampling again reuses the cached primitive *)
       no_unsafe (fun () -> Lwd.set sel true);
       sample_fails o;
       assert (!acquired = 1 && !released = 0);
       no_unsafe (fun () -> Lwd.set sel false);
       assert (Lwd.quick_sample o = 1);
       assert (!acquired = 1 && !released = 1);
       Lwd.quick_release o;
       assert (!acquired = 1 && !released = 1))

let () =
  test "exceptions: a prim whose acquire raises is retried on the next sample, \
        Lwd.invalidate on it does not call release, and release stays balanced"
    (fun () ->
       let attempts = ref 0 and acquired = ref 0 and released = ref 0 in
       let self = ref None in
       let p = Lwd.prim
           ~acquire:(fun p ->
               self := Some p;
               incr attempts;
               if !attempts = 1 then failwith "boom";
               incr acquired; !attempts)
           ~release:(fun _ _ -> incr released)
       in
       let o, _ = counting_root (Lwd.map (Lwd.get_prim p) ~f:succ) in
       sample_fails o;
       (match !self with
        | None -> assert false
        | Some p -> no_unsafe (fun () -> Lwd.invalidate p));
       assert (!released = 0);
       assert (Lwd.quick_sample o = 3);
       assert (!acquired = 1 && !released = 0);
       Lwd.quick_release o;
       assert (!acquired = 1 && !released = 1))

let () =
  test "exceptions: releasing a root right after a failed sample releases the \
        primitives that were acquired, and the graph can be observed again"
    (fun () ->
       let acquired = ref 0 and released = ref 0 in
       let p = Lwd.prim
           ~acquire:(fun _ -> incr acquired; 1)
           ~release:(fun _ _ -> incr released)
       in
       let x = Lwd.var 0 in
       let d = Lwd.map2 (Lwd.get_prim p) (Lwd.get x) ~f:(fun a v ->
           if v = 0 then failwith "boom" else a + v) in
       let o, _ = counting_root (Lwd.map d ~f:succ) in
       sample_fails o;
       assert (!acquired = 1);
       Lwd.quick_release o;
       assert (!released = 1);
       no_unsafe (fun () -> Lwd.set x 1);
       let o', _ = counting_root (Lwd.map d ~f:succ) in
       assert (Lwd.quick_sample o' = 3);
       assert (!acquired = 2 && !released = 1);
       Lwd.quick_release o';
       assert (!released = 2))

(* --- Primitives --- *)

let () =
  test "prim: acquire/release lifecycle runs exactly once per acquire" (fun () ->
      let acquired = ref 0 and released = ref 0 in
      let p =
        Lwd.prim
          ~acquire:(fun _ -> incr acquired; 42)
          ~release:(fun _ _ -> incr released)
      in
      let o = Lwd.observe (Lwd.get_prim p) in
      assert (Lwd.quick_sample o = 42);
      assert (Lwd.quick_sample o = 42);
      assert (!acquired = 1 && !released = 0);
      Lwd.quick_release o;
      assert (!acquired = 1 && !released = 1))

let () =
  test "prim: acquire receives a usable self-reference (recursive primitives)" (fun () ->
      let captured = ref None in
      let p =
        Lwd.prim ~acquire:(fun self -> captured := Some self; 1) ~release:(fun _ _ -> ())
      in
      let o = Lwd.observe (Lwd.get_prim p) in
      assert (Lwd.quick_sample o = 1);
      match !captured with
      | None -> failwith "acquire was not called"
      | Some self ->
        Lwd.invalidate self;
        assert (Lwd.quick_sample o = 1);
        Lwd.quick_release o)

let () =
  test "invalidate on a prim that was never acquired is a no-op" (fun () ->
      let released = ref false in
      let p = Lwd.prim ~acquire:(fun _ -> 1) ~release:(fun _ _ -> released := true) in
      Lwd.invalidate p;
      assert (not !released))

let () =
  test "Release_failure aggregates exceptions from multiple failing prim releases"
    (fun () ->
       let p1 = Lwd.prim ~acquire:(fun _ -> 1) ~release:(fun _ _ -> failwith "release1") in
       let p2 = Lwd.prim ~acquire:(fun _ -> 2) ~release:(fun _ _ -> failwith "release2") in
       let d = Lwd.map2 (Lwd.get_prim p1) (Lwd.get_prim p2) ~f:(+) in
       let o = Lwd.observe d in
       assert (Lwd.quick_sample o = 3);
       match Lwd.quick_release o with
       | () -> failwith "expected Release_failure"
       | exception Lwd.Release_failure (main_exn, failures) ->
         assert (main_exn = None);
         assert (List.length failures = 2);
         let messages =
           List.map (fun (exn, _) -> Printexc.to_string exn) failures |> List.sort compare
         in
         assert (messages = List.sort compare [ Printexc.to_string (Failure "release1");
                                                 Printexc.to_string (Failure "release2") ]))

(* --- join/bind dynamic switching --- *)

let () =
  test "bind: switching branches acquires the new branch before releasing the old one"
    (fun () ->
       let events = ref [] in
       let log s = events := s :: !events in
       let mk name v =
         let p =
           Lwd.prim
             ~acquire:(fun _ -> log ("acquire " ^ name); v)
             ~release:(fun _ _ -> log ("release " ^ name))
         in
         Lwd.get_prim p
       in
       let switch = Lwd.var true in
       let a = mk "A" 1 and b = mk "B" 2 in
       let d = Lwd.bind (Lwd.get switch) ~f:(fun s -> if s then a else b) in
       let o = Lwd.observe d in
       assert (Lwd.quick_sample o = 1);
       assert (List.rev !events = [ "acquire A" ]);
       Lwd.set switch false;
       assert (Lwd.quick_sample o = 2);
       (* B must be acquired, and A released, as part of settling this sample;
          old branch is torn down through the release queue, not before the
          new branch is in place. *)
       assert (List.rev !events = [ "acquire A"; "acquire B"; "release A" ]);
       Lwd.quick_release o;
       assert (List.rev !events = [ "acquire A"; "acquire B"; "release A"; "release B" ]))

(* --- unsafe-mutation diagnostics --- *)

let () =
  test "mutating an unrelated var during evaluation is not flagged unsafe" (fun () ->
      let x = Lwd.var 0 and y = Lwd.var 0 in
      let d = Lwd.map ~f:(fun v -> Lwd.set y (v + 1); v) (Lwd.get x) in
      let o = Lwd.observe d in
      let logged = capture_unsafe (fun () -> ignore (Lwd.quick_sample o)) in
      assert (logged = []);
      Lwd.quick_release o)

let () =
  test "mutating the var currently being evaluated (via bind) is flagged unsafe" (fun () ->
      let x = Lwd.var 0 in
      let d = Lwd.bind (Lwd.get x) ~f:(fun v -> if v = 0 then Lwd.set x 1; Lwd.return v) in
      let o = Lwd.observe d in
      let logged = capture_unsafe (fun () -> ignore (Lwd.quick_sample o)) in
      assert (logged = [ `Mutation ]);
      Lwd.quick_release o)

let () =
  test "map2: the right operand mutating a dependency of the left one, which was \
        already read, is flagged unsafe (siblings are NOT treated as isolated, despite \
        being conceptually parallel)"
    (fun () ->
       (* Per a comment in lwd.ml above invalidate_node, the author considered
          (but did not implement) relaxing this so that map2/pair/app operands
          could mutate each other's dependencies freely. Today, doing so is
          flagged the same as any other re-entrant mutation. *)
       let x = Lwd.var 0 and y = Lwd.var 0 in
       let lhs = Lwd.get x in
       let rhs = Lwd.map ~f:(fun v -> Lwd.set x 100; v) (Lwd.get y) in
       let d = Lwd.map2 lhs rhs ~f:(fun a b -> (a, b)) in
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> assert (Lwd.quick_sample o = (0, 0))) in
       assert (logged = [ `Mutation ]);
       Lwd.quick_release o)

let () =
  test "map2: the left operand mutating a dependency of the right one is only flagged \
        if that dependency already holds a cached value: on the first sample it was \
        never evaluated, so the invalidation stops there and the right operand reads \
        the new value; on later samples it is cached and the mutation is flagged"
    (fun () ->
       let x = Lwd.var 0 and y = Lwd.var 0 in
       let lhs = Lwd.map ~f:(fun v -> Lwd.set y (v + 100); v) (Lwd.get x) in
       let rhs = Lwd.get y in
       let d = Lwd.map2 lhs rhs ~f:(fun a b -> (a, b)) in
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> assert (Lwd.quick_sample o = (0, 100))) in
       assert (logged = []);
       Lwd.set x 1;
       let logged = capture_unsafe (fun () -> assert (Lwd.quick_sample o = (1, 101))) in
       assert (logged = [ `Mutation ]);
       Lwd.quick_release o)

let () =
  test "a released and re-acquired graph behaves like a fresh one: the left operand \
        mutating a dependency of the right one is not flagged in either case \
        (released nodes used to be left at Eval_progress, so the invalidation \
        propagated up to the root and was flagged)"
    (fun () ->
       let x = Lwd.var 0 and y = Lwd.var 0 in
       let lhs = Lwd.map ~f:(fun v -> Lwd.set y (v + 100); v) (Lwd.get x) in
       let rhs = Lwd.map ~f:(fun v -> v) (Lwd.get y) in
       let d = Lwd.map2 lhs rhs ~f:(fun a b -> (a, b)) in
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> assert (Lwd.quick_sample o = (0, 100))) in
       assert (logged = []);
       Lwd.quick_release o;
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> assert (Lwd.quick_sample o = (0, 100))) in
       assert (logged = []);
       Lwd.quick_release o)

let () =
  test "bind: mutating a var read by the freshly-selected branch, during that branch's \
        own evaluation, is flagged unsafe"
    (fun () ->
       let switch = Lwd.var true in
       let inner = Lwd.var 0 in
       let d =
         Lwd.bind (Lwd.get switch) ~f:(fun _ ->
             Lwd.map ~f:(fun v -> Lwd.set inner (v + 1); v) (Lwd.get inner))
       in
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> ignore (Lwd.quick_sample o)) in
       assert (logged = [ `Mutation ]);
       Lwd.quick_release o)

let () =
  test "bind: mutating a var only the OLD (about-to-be-replaced) branch depends on, \
        while the selector function itself is still running, IS considered safe"
    (fun () ->
       (* This is the specific case documented by the comment in invalidate_node:
          "While the child node (the outer graph) of a Join is evaluated, it is
          safe to invalidate the inner graph (because it will be re-evaluated
          next, if necessary)." This test pins that the safe path really is
          taken, as opposed to merely being aspirational. *)
       let switch = Lwd.var 0 in
       let old_dep = Lwd.var "old" in
       let d =
         Lwd.bind (Lwd.get switch) ~f:(fun s ->
             if s = 0 then Lwd.map ~f:Fun.id (Lwd.get old_dep)
             else begin
               (* we are here strictly inside [f], i.e. while the join's
                  selector node is still Eval_progress *)
               Lwd.set old_dep "mutated-while-outer-in-progress";
               Lwd.return "new-branch"
             end)
       in
       let o = Lwd.observe d in
       let logged =
         capture_unsafe (fun () ->
             assert (Lwd.quick_sample o = "old");
             Lwd.set switch 1;
             assert (Lwd.quick_sample o = "new-branch"))
       in
       assert (logged = []);
       Lwd.quick_release o)

let () =
  test "unsafe status is a per-`set`-call flag, not per-observer: an unrelated, \
        perfectly well-behaved observer of the same var does not prevent the \
        call from being flagged, and is unaffected by the flag"
    (fun () ->
       let x = Lwd.var 0 in
       let a = Lwd.map ~f:(fun v -> if v = 0 then Lwd.set x 1; v) (Lwd.get x) in
       let oa = Lwd.observe a in
       let b = Lwd.map ~f:(fun v -> v * 10) (Lwd.get x) in
       let ob = Lwd.observe b in
       ignore (Lwd.quick_sample ob);
       let logged = capture_unsafe (fun () -> ignore (Lwd.quick_sample oa)) in
       assert (logged = [ `Mutation ]);
       (* b's own recomputation is entirely correct despite the flag having
          fired because of a's unrelated behavior *)
       assert (Lwd.quick_sample ob = 10);
       Lwd.quick_release oa;
       Lwd.quick_release ob)

let () =
  test "nested sampling: calling `sample` again from within on_invalidate is detected \
        and reported, but does not crash and yields a self-consistent value"
    (fun () ->
       let queue = Lwd.make_release_queue () in
       let x = Lwd.var 0 in
       let o = Lwd.observe (Lwd.get x) in
       let nested_value = ref None in
       Lwd.set_on_invalidate o (fun _ -> nested_value := Some (Lwd.sample queue o));
       assert (Lwd.sample queue o = 0);
       let logged = capture_unsafe (fun () -> Lwd.set x 42) in
       assert (logged = [ `Nested_sampling ]);
       assert (!nested_value = Some 42);
       assert (Lwd.sample queue o = 42);
       ignore (Lwd.flush_release_queue queue))

(* --- fix --- *)

let () =
  test "fix: converges in the expected number of rounds for a convergent computation"
    (fun () ->
       let x = Lwd.var 0 in
       let rounds = ref 0 in
       let d =
         Lwd.fix
           (Lwd.map
              ~f:(fun v ->
                  incr rounds;
                  if v < 5 then Lwd.set x (v + 1);
                  v)
              (Lwd.get x))
           ~wrt:(Lwd.get x)
       in
       let o = Lwd.observe d in
       (* mutating x while it's mid-fix-round is itself unsafe by the same rules
          exercised above; not the point of this test, so it's silenced here *)
       ignore (capture_unsafe (fun () -> assert (Lwd.quick_sample o = 5)));
       assert (!rounds = 6);
       Lwd.quick_release o)

let () =
  test "fix: is correctly \"re-armed\" after convergence -- a later, independent mutation \
        of the witness re-triggers the whole stabilization loop rather than being silently \
        ignored. Historically a real bug (git commits 7390d23/6a1f474, \"Fix was not \
        re-armed after convergence\"): if fix's own dirty bit isn't cleared once it has \
        converged, a subsequent invalidation of the witness can be missed entirely, and \
        the observing root never re-samples."
    (fun () ->
       let state = Lwd.var 0 in
       let steps = ref 0 in
       let d =
         Lwd.map ~f:(fun st -> incr steps; if st < 2 then Lwd.set state (st + 1); st) (Lwd.get state)
       in
       let stabilized = Lwd.fix d ~wrt:(Lwd.get state) in
       let invalidated = ref false in
       let o = Lwd.observe ~on_invalidate:(fun _ -> invalidated := true) stabilized in
       ignore (capture_unsafe (fun () -> assert (Lwd.quick_sample o = 2)));
       assert (!steps = 3);
       assert (not (Lwd.is_damaged o));
       assert (not !invalidated);
       (* second cycle: if fix weren't re-armed, this next `set` would fail to
          reach/damage the root at all *)
       Lwd.set state 0;
       assert (Lwd.is_damaged o);
       assert !invalidated;
       ignore (capture_unsafe (fun () -> assert (Lwd.quick_sample o = 2)));
       assert (!steps = 6);
       assert (not (Lwd.is_damaged o));
       Lwd.quick_release o)

let () =
  test "bind: switching back and forth between branches without flushing the release \
        queue in between does not corrupt the graph (a previously-referenced branch can \
        be re-acquired while its removal is still only queued, not yet actually released). \
        This general shape of scenario is what git commits 85c90f7 (\"Separate removing \
        reference from releasing graph\") and 86af55b (\"fix assertions in nodes acquired \
        while in release queue\", introducing the `TA` trace state) targeted -- this exact \
        sequence does not happen to reproduce the historical failure (verified: it also \
        passes against the pre-fix code), but is kept as a stress test of the same area."
    (fun () ->
       let idx = Lwd.var 0 in
       let acquired = Array.make 3 0 and released = Array.make 3 0 in
       let branches =
         Array.init 3 (fun i ->
             let p =
               Lwd.prim
                 ~acquire:(fun _ -> acquired.(i) <- acquired.(i) + 1; i * 10)
                 ~release:(fun _ _ -> released.(i) <- released.(i) + 1)
             in
             Lwd.get_prim p)
       in
       let d = Lwd.bind (Lwd.get idx) ~f:(fun i -> branches.(i)) in
       let o = Lwd.observe d in
       let queue = Lwd.make_release_queue () in
       (* bounce between branches 0,1,0,2,0,1 without ever flushing the queue *)
       let visit i expected =
         Lwd.set idx i;
         assert (Lwd.sample queue o = expected)
       in
       assert (Lwd.sample queue o = 0);
       visit 1 10;
       visit 0 0;
       visit 2 20;
       visit 0 0;
       visit 1 10;
       visit 0 0;
       (* nothing has been flushed yet: branch 0 is currently selected (and
          acquired), but was also queued for release multiple times along the
          way without ever actually being torn down since it kept getting
          re-selected before the queue was flushed *)
       ignore (Lwd.flush_release_queue queue);
       (* the currently-selected branch (0) must have survived intact *)
       assert (Lwd.sample queue o = 0);
       ignore (Lwd.flush_release_queue queue);
       Lwd.release queue o;
       ignore (Lwd.flush_release_queue queue))

let () =
  test "fix: a non-convergent computation is not bounded -- it spins until an external \
        limit (here, a wall-clock alarm) interrupts it. `fix` has no built-in iteration \
        cap; a real application must ensure convergence itself."
    (fun () ->
       let x = Lwd.var 0 in
       let rounds = ref 0 in
       let d =
         Lwd.fix
           (Lwd.map ~f:(fun v -> incr rounds; Lwd.set x (v + 1); v) (Lwd.get x))
           ~wrt:(Lwd.get x)
       in
       let o = Lwd.observe d in
       (* each round re-triggers the unsafe-mutation diagnostic (see the
          dedicated tests above); silence it here so that hundreds of
          thousands of backtrace prints don't dominate the test run. *)
       let saved = !Lwd.unsafe_action_logger in
       Lwd.unsafe_action_logger := ignore;
       let outcome = with_timeout 2 (fun () -> Lwd.quick_sample o) in
       Lwd.unsafe_action_logger := saved;
       match outcome with
       | `Result _ -> failwith "expected the computation to still be running after 2s"
       | `Timeout -> assert (!rounds > 100_000))

(* --- absence of cycle detection --- *)

let sample_cycles o =
  match Lwd.quick_sample o with
  | _ -> failwith "expected Lwd.Cycle"
  | exception Lwd.Cycle -> ()

let counting_prim v =
  let acquired = ref 0 and released = ref 0 in
  let p = Lwd.prim ~acquire:(fun _ -> incr acquired; v) ~release:(fun _ _ -> incr released) in
  Lwd.get_prim p, acquired, released

let () =
  test "FIXED: a self-referential document raises Lwd.Cycle instead of overflowing the \
        stack, and the root recovers once the cycle is gone. See FINDINGS.md."
    (fun () ->
       let x = Lwd.var 0 in
       let rec d =
         lazy (Lwd.bind (Lwd.get x) ~f:(fun v -> if v > 3 then Lwd.return v else Lazy.force d))
       in
       let o = Lwd.observe (Lazy.force d) in
       let logged = capture_unsafe (fun () ->
           (match with_timeout 5 (fun () -> sample_cycles o) with
            | `Result () -> ()
            | `Timeout -> failwith "timeout");
           assert (Lwd.is_damaged o);
           Lwd.set x 5;
           assert (Lwd.quick_sample o = 5);
           Lwd.set x 0;
           sample_cycles o;
           Lwd.set x 4;
           assert (Lwd.quick_sample o = 4))
       in
       assert (logged = []);
       Lwd.quick_release o)

let () =
  test "cycle: the intermediate closing the cycle is dropped, so the loop of references \
        is broken and every primitive is released"
    (fun () ->
       let sel = Lwd.var true in
       let p_child, acq_child, rel_child = counting_prim 0 in
       let p_cycle, acq_cycle, rel_cycle = counting_prim 0 in
       let rec j =
         lazy (Lwd.bind (Lwd.map2 (Lwd.get sel) p_child ~f:(fun b _ -> b)) ~f:(fun b ->
             if b then Lwd.map2 p_cycle (Lazy.force j) ~f:( + ) else Lwd.return 1))
       in
       let o = Lwd.observe (Lazy.force j) in
       sample_cycles o;
       (* the branch closing the cycle was released when the queue was flushed *)
       assert (!acq_cycle = 1 && !rel_cycle = 1);
       Lwd.set sel false;
       assert (Lwd.quick_sample o = 1);
       Lwd.set sel true;
       sample_cycles o;
       assert (!acq_cycle = 2 && !rel_cycle = 2);
       (* nothing references the join anymore: it is released with the root *)
       assert (!acq_child = 1 && !rel_child = 0);
       Lwd.quick_release o;
       assert (!acq_child = 1 && !rel_child = 1))

let () =
  test "cycle: a cycle entering the join through its child is detected, and broken by \
        dropping the intermediate of the inner join"
    (fun () ->
       let v = Lwd.var () in
       let p_child, acq_child, rel_child = counting_prim () in
       let p_cycle, acq_cycle, rel_cycle = counting_prim 0 in
       let rec j =
         lazy (
           let inner = Lwd.bind (Lwd.get v) ~f:(fun () ->
               Lwd.map2 p_cycle (Lazy.force j) ~f:( + )) in
           Lwd.join (Lwd.map2 p_child inner ~f:(fun () _ -> Lwd.return 0)))
       in
       let o = Lwd.observe (Lazy.force j) in
       sample_cycles o;
       assert (!acq_cycle = 1 && !rel_cycle = 1);
       Lwd.quick_release o;
       assert (!acq_child = 1 && !rel_child = 1))

let () =
  test "cycle: no false positives for a join reached twice in the same sample, a join \
        whose evaluation raised, or a join released and re-acquired"
    (fun () ->
       let x = Lwd.var 0 in
       let j = Lwd.bind (Lwd.get x) ~f:(fun v ->
           if v < 0 then failwith "boom" else Lwd.return v) in
       let o = Lwd.observe (Lwd.map2 j (Lwd.map j ~f:succ) ~f:( + )) in
       assert (Lwd.quick_sample o = 1);
       Lwd.set x (-1);
       (match Lwd.quick_sample o with
        | _ -> failwith "expected an exception"
        | exception Failure _ -> ());
       Lwd.set x 1;
       let o' = Lwd.observe j in
       assert (Lwd.quick_sample o' = 1);
       assert (Lwd.quick_sample o = 3);
       Lwd.quick_release o;
       Lwd.quick_release o';
       let o = Lwd.observe (Lwd.map j ~f:succ) in
       assert (Lwd.quick_sample o = 2);
       Lwd.quick_release o)

let () =
  test "cycle: no false positives when sampling again right after a join's evaluation \
        raised, whether it raised in its child or in its intermediate"
    (fun () ->
       let fails o =
         match Lwd.quick_sample o with
         | _ -> failwith "expected an exception"
         | exception Failure msg -> assert (msg = "boom")
       in
       let in_child = Lwd.bind (Lwd.get (Lwd.var ())) ~f:(fun () -> failwith "boom") in
       let in_intermediate =
         Lwd.bind (Lwd.get (Lwd.var ())) ~f:(fun () ->
             Lwd.map (Lwd.get (Lwd.var ())) ~f:(fun () -> failwith "boom"))
       in
       List.iter (fun d ->
           let o = Lwd.observe d in
           fails o;
           fails o;
           let o' = Lwd.observe d in
           fails o';
           Lwd.quick_release o;
           Lwd.quick_release o')
         [in_child; in_intermediate])

(* --- deep chains --- *)

let () =
  test "a long linear chain of maps recomputes correctly without stack overflow"
    (fun () ->
       let depth = 100_000 in
       let x = Lwd.var 0 in
       let d = ref (Lwd.get x) in
       for _ = 1 to depth do
         d := Lwd.map ~f:succ !d
       done;
       let o = Lwd.observe !d in
       assert (Lwd.quick_sample o = depth);
       Lwd.set x 1;
       assert (Lwd.quick_sample o = depth + 1);
       Lwd.quick_release o)

let init = ()
