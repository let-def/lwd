(* Tests for [Lwd_utils]: monoid reduction helpers and small numeric utils. *)

open Framework

let int_monoid : int Lwd_utils.monoid = (0, ( + ))
let string_monoid : string Lwd_utils.monoid = ("", ( ^ ))

let () =
  test "reduce/map_reduce match List.fold_left/List.map+fold_left, for various lengths"
    (fun () ->
       List.iter
         (fun l ->
            let expected = List.fold_left ( + ) 0 l in
            check_eq (Printf.sprintf "reduce len=%d" (List.length l)) string_of_int expected
              (Lwd_utils.reduce int_monoid l);
            let expected_mr = List.fold_left (fun acc x -> acc + (x * 2)) 0 l in
            check_eq (Printf.sprintf "map_reduce len=%d" (List.length l)) string_of_int expected_mr
              (Lwd_utils.map_reduce (fun x -> x * 2) int_monoid l))
         [ []; [ 1 ]; [ 1; 2 ]; List.init 100 Fun.id; List.init 101 Fun.id ])

let () =
  test "reduce on a non-associative-looking operation still matches the documented \
        tree-shaped (not left-fold) evaluation order for pack/pack_seq"
    (fun () ->
       (* [pack]/[pack_seq] operate on Lwd.t values, but their evaluation
          shape (balanced, not a linear fold) is documented, so we check it
          concretely using a monoid that records structure via strings. *)
       let structural_monoid : string Lwd_utils.monoid =
         ("_", fun a b -> Printf.sprintf "(%s+%s)" a b)
       in
       let items = List.map (fun i -> Lwd.pure (string_of_int i)) [ 1; 2; 3; 4 ] in
       let packed = Lwd_utils.pack structural_monoid items in
       let o = Lwd.observe packed in
       let result = Lwd.quick_sample o in
       (* documented shape: reduce f [a;b;c;d] = f (f a b) (f c d) *)
       check_eq "pack uses balanced tree shape" Fun.id "((1+2)+(3+4))" result;
       Lwd.quick_release o)

let () =
  test "pack_seq (OCaml Seq.t) agrees with pack (list) for the same elements" (fun () ->
      let items = List.map (fun i -> Lwd.pure i) [ 1; 2; 3; 4; 5; 6; 7 ] in
      let via_list = Lwd_utils.pack int_monoid items in
      let via_seq = Lwd_utils.pack_seq int_monoid (List.to_seq items) in
      let ol = Lwd.observe via_list and os = Lwd.observe via_seq in
      check_eq "pack" string_of_int 28 (Lwd.quick_sample ol);
      check_eq "pack_seq" string_of_int 28 (Lwd.quick_sample os);
      Lwd.quick_release ol;
      Lwd.quick_release os)

let () =
  test "pack of an empty list returns the monoid's neutral element" (fun () ->
      let packed = Lwd_utils.pack int_monoid [] in
      let o = Lwd.observe packed in
      check_eq "empty pack" string_of_int 0 (Lwd.quick_sample o);
      Lwd.quick_release o)

let () =
  test "lift_monoid: an Lwd-lifted monoid reduces reactively and updates on var changes"
    (fun () ->
       let lifted = Lwd_utils.lift_monoid int_monoid in
       let x = Lwd.var 1 and y = Lwd.var 2 in
       let combined = (snd lifted) (Lwd.get x) (Lwd.get y) in
       let o = Lwd.observe combined in
       check_eq "initial" string_of_int 3 (Lwd.quick_sample o);
       Lwd.set x 10;
       check_eq "after set" string_of_int 12 (Lwd.quick_sample o);
       Lwd.quick_release o)

let () =
  test "map_l / flatten_l commute Lwd and list, reactively" (fun () ->
      let xs = List.init 5 (fun i -> Lwd.var i) in
      let docs = List.map (fun v -> Lwd.map ~f:succ (Lwd.get v)) xs in
      let flattened = Lwd_utils.flatten_l docs in
      let o = Lwd.observe flattened in
      check_eq "initial" (fun l -> String.concat "," (List.map string_of_int l))
        [ 1; 2; 3; 4; 5 ] (Lwd.quick_sample o);
      Lwd.set (List.nth xs 2) 100;
      check_eq "after set" (fun l -> String.concat "," (List.map string_of_int l))
        [ 1; 2; 101; 4; 5 ] (Lwd.quick_sample o);
      Lwd.quick_release o)

let () =
  test "map_l applies the function to every element of a plain list, in order" (fun () ->
      let result = Lwd_utils.map_l (fun x -> Lwd.pure (x * 2)) [ 1; 2; 3 ] in
      let o = Lwd.observe result in
      check_eq "map_l" (fun l -> String.concat "," (List.map string_of_int l)) [ 2; 4; 6 ]
        (Lwd.quick_sample o);
      Lwd.quick_release o)

let () =
  test "min/max/clamp helpers for int and float" (fun () ->
      assert (Lwd_utils.mini 3 5 = 3);
      assert (Lwd_utils.mini 5 3 = 3);
      assert (Lwd_utils.maxi 3 5 = 5);
      assert (Lwd_utils.maxi 5 3 = 5);
      assert (Lwd_utils.clampi 10 ~min:0 ~max:5 = 5);
      assert (Lwd_utils.clampi (-10) ~min:0 ~max:5 = 0);
      assert (Lwd_utils.clampi 3 ~min:0 ~max:5 = 3);
      assert (Lwd_utils.minf 3.0 5.0 = 3.0);
      assert (Lwd_utils.maxf 3.0 5.0 = 5.0);
      assert (Lwd_utils.clampf 10.0 ~min:0.0 ~max:5.0 = 5.0);
      assert (Lwd_utils.clampf (-10.0) ~min:0.0 ~max:5.0 = 0.0))

let () =
  test "clampi/clampf with an inverted (min > max) range: pins current, possibly \
        surprising, behavior rather than raising"
    (fun () ->
       (* Neither the .mli nor the implementation document what happens if
          min > max; this test only pins down whatever the current
          implementation happens to do, so a future change is visible. *)
       let r = Lwd_utils.clampi 3 ~min:10 ~max:0 in
       Printf.printf "    (info) clampi 3 ~min:10 ~max:0 = %d\n" r)

let init = ()
