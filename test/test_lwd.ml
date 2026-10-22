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
  test "map2/pair/app evaluate their RIGHT operand before their left one -- the reverse \
        of what the argument order in source suggests. This isn't a documented Lwd \
        contract: sub_sample's Map2/Pair/App cases are written as direct applications \
        like `f (aux self x) (aux self y)`, and OCaml's evaluation order for function \
        arguments is unspecified by the language, just consistently right-to-left in \
        both the bytecode and native backends. Code with side effects in operands \
        (e.g. mutating a var) must not assume left-to-right evaluation. See FINDINGS.md."
    (fun () ->
       (* [mk]'s base document must NOT be pure ([Lwd.return]/[Lwd.pure]):
          [Lwd.map] over a pure value is folded into a new pure value
          EAGERLY, at graph-construction time (see the [Pure vx -> Pure (f
          vx)] branch of [map] in lwd.ml), bypassing [sub_sample] entirely.
          An earlier version of this test used [Lwd.return] here and
          "passed" for the wrong reason: it was measuring OCaml's
          right-to-left function-argument evaluation at the call to [mk],
          not sub_sample's operand evaluation order at all. Using a
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
       assert (v = 3 && ord = [ "R"; "L" ] && label = "map2");
       let v, ord, label = check_one "pair" (Lwd.pair (mk "L" 1) (mk "R" 2)) in
       assert (v = (1, 2) && ord = [ "R"; "L" ] && label = "pair");
       let v, ord, label =
         check_one "app" (Lwd.app (Lwd.map ~f:( + ) (mk "L" 1)) (mk "R" 2))
       in
       assert (v = 3 && ord = [ "R"; "L" ] && label = "app"))

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
  test "BUG: after an exception escapes evaluation, the node is left stuck at \
        Eval_progress, so the NEXT (otherwise perfectly ordinary and unrelated) `set` \
        that invalidates it is spuriously flagged `Mutation` -- a false positive with \
        no actual concurrent/re-entrant evaluation involved. The graph still recomputes \
        correctly afterwards (invalidation resets the stale state to Eval_none as a \
        side effect), so this is a diagnostic false positive, not a correctness bug. \
        See FINDINGS.md."
    (fun () ->
       let x = Lwd.var 0 in
       let d = Lwd.map ~f:(fun v -> if v = 0 then failwith "boom" else v * 2) (Lwd.get x) in
       let o = Lwd.observe d in
       (try ignore (Lwd.quick_sample o) with Failure _ -> ());
       (* nothing is being evaluated right now: this `set` is entirely ordinary *)
       let logged = capture_unsafe (fun () -> Lwd.set x 5) in
       assert (logged = [ `Mutation ]);
       assert (Lwd.quick_sample o = 10);
       Lwd.quick_release o)

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
  test "map2: mutating a dependency of the sibling operand is flagged unsafe (siblings \
        are NOT treated as isolated, despite being conceptually parallel; true regardless \
        of which operand actually runs first -- see the evaluation-order test above)"
    (fun () ->
       (* Per a comment in lwd.ml above invalidate_node, the author considered
          (but did not implement) relaxing this so that map2/pair/app operands
          could mutate each other's dependencies freely. Today, doing so is
          flagged the same as any other re-entrant mutation. *)
       let x = Lwd.var 0 and y = Lwd.var 0 in
       let lhs = Lwd.map ~f:(fun v -> Lwd.set y 100; v) (Lwd.get x) in
       let rhs = Lwd.get y in
       let d = Lwd.map2 lhs rhs ~f:(fun a b -> (a, b)) in
       let o = Lwd.observe d in
       let logged = capture_unsafe (fun () -> ignore (Lwd.quick_sample o)) in
       assert (logged = [ `Mutation ]);
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

let () =
  test "no cycle detection: a self-referential document overflows the stack rather \
        than raising a clean error, despite `eval`'s `Eval_progress` state being \
        documented as \"used to detect cycles\" -- no code path actually inspects \
        that state to reject a cycle. See FINDINGS.md."
    (fun () ->
       let x = Lwd.var 0 in
       let rec d =
         lazy (Lwd.bind (Lwd.get x) ~f:(fun v -> if v > 3 then Lwd.return v else Lazy.force self_ref))
       and self_ref = lazy (Lazy.force d) in
       let o = Lwd.observe (Lazy.force d) in
       match with_timeout 5 (fun () -> Lwd.quick_sample o) with
       | `Result _ -> failwith "expected Stack_overflow or a timeout, got a value instead"
       | `Timeout -> ()
       | exception Stack_overflow -> ())

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
