type folder_kind =
  | Accumulate
  | Mark_new
  | Mark_old
  | Clear

type !'a t = {
  value: 'a;
  mutable mark: int;
  fold: 'a foldable;
  mutable cache: cache;
}

and cache =
  | More of {trace: trace; mutable next: cache}
  | Done

and 'a foldable = 'a -> folder_kind -> folder -> folder

and handle = Handle : 'a t -> handle [@@ocaml.unboxed]

and folder = handle list

and ('a, 'b) map = {
  func: tape -> 'a -> 'b;
  finalize: 'a -> 'b -> unit;
  mutable slot: ('b * trace) option;
}

and trace = Trace : {
    map: ('a, 'b) map;
    input: 'a t;
    output: 'b;
    sub: trace list;
  } -> trace

and tape = trace list ref

let make value fold = {fold; value; mark = 0; cache = Done}
let peek t = t.value

type ('a, 'b) resumption = R of ('a -> 'b * ('a, 'b) resumption) [@@ocaml.unboxed]

let map ?finalize func =
  let finalize = match finalize with
    | None -> (fun _ _ -> ())
    | Some finalizer ->
      (fun a b -> try finalizer a b with exn ->
          Printf.eprintf "Lwd_alg.finalize exception: %s\n"
            (Printexc.to_string exn))
  in
  { func; finalize; slot = None }

(* How to update a computation, given a tape
   -----------------------------------------

   - Synchronized visit of the tape and the new value, to mark visited values
     and populate the value cache.
   - Now apply the user-provided computation:
     For each pair [(computation, value)], look into the value cache to find a
     past occurrence of this transformation
*)

external ( =!= ) : _ map -> _ map -> bool = "%noteq"

let rec find_next_cached map = function
  | Done -> assert false
  | More {next = Done; _} -> ()
  | More ({next = More t' as next} as t) ->
    let Trace tr = t'.trace in
    if tr.map =!= map then
      find_next_cached map next
    else (
      assert (Option.is_none tr.map.slot);
      (* Update slot *)
      tr.map.slot <- Some (tr.output, t'.trace);
      (* Remove from list *)
      t.next <- t'.next
    )

let apply tape map input =
  (* Try to re-use cached computation *)
  begin match input.cache with
    | Done -> ()
    | More t' ->
      let Trace tr = t'.trace in
      if tr.map =!= map then
        find_next_cached map input.cache
      else (
        assert (Option.is_none tr.map.slot);
        (* Update slot *)
        tr.map.slot <- Some (tr.output, t'.trace);
        (* Remove from list *)
        input.cache <- t'.next
      )
  end;
  (* Check if we could populate the slot *)
  match map.slot with
  | Some (result, trace) ->
    (* Success, reuse result *)
    map.slot <- None;
    tape := trace :: !tape;
    result
  | None ->
    (* Failure: run computation *)
    let tape' = ref [] in
    let output = map.func tape' input.value in
    let trace = Trace {map; input; output; sub = !tape'} in
    tape := trace :: !tape;
    output

let mark_old = 1
let mark_new = 2
let mark_both = 3

let rec saturate_marks = function
  | [] -> ()
  | Handle x :: xs ->
    if x.mark = mark_old || x.mark = mark_new then (
      x.mark <- mark_both;
      saturate_marks (x.fold x.value Accumulate xs)
    ) else
      saturate_marks xs

let clear t xs =
  if t.mark = 0 then
    xs
  else (
    t.mark <- 0;
    Handle t :: xs
  )

let add (type a) (t : a t) kind xs =
  match kind with
  | Accumulate -> Handle t :: xs
  | Mark_new ->
    if t.mark = 0 then (
      t.mark <- mark_new;
      Handle t :: xs
    ) else (
      if t.mark = mark_old then
        saturate_marks [Handle t];
      xs
    )
  | Mark_old ->
    if t.mark = 0 then (
      t.mark <- mark_old;
      Handle t :: xs
    ) else (
      if t.mark = mark_new then
        saturate_marks [Handle t];
      xs
    )
  | Clear ->
    clear t xs

let propagate_mark t acc =
  if t.mark = mark_old then (
    t.fold t.value Mark_old acc
  ) else if t.mark = mark_new then (
    t.fold t.value Mark_new acc
  ) else (
    assert (t.mark = mark_both);
    acc
  )

let rec propagate_marks acc = function
  | Handle t :: xs ->
    propagate_marks (propagate_mark t acc) xs
  | [] ->
    match acc with
    | [] -> ()
    | xs -> propagate_marks [] xs

let mark_roots told tnew =
  assert (told.mark = 0);
  assert (tnew.mark = 0);
  told.mark <- mark_old;
  tnew.mark <- mark_new;
  propagate_marks [] [Handle told; Handle tnew]

let rec clear_submarks = function
  | [] -> ()
  | Handle t :: xs ->
    assert (t.mark = 0);
    clear_submarks (t.fold t.value Clear xs)

let clear_roots told tnew =
  clear_submarks (clear told (clear tnew []))

let rec populate_cache (Trace tr as trace) =
  if tr.input.mark land mark_new = mark_new then (
    tr.input.cache <- More {trace; next = tr.input.cache};
    List.iter populate_cache tr.sub
  )

let rec clear_cacheline = function
  | Done -> ()
  | More {trace = Trace tr; next} ->
    tr.map.finalize tr.input.value tr.output;
    clear_cacheline next

let rec clear_cache (Trace tr) =
  clear_cacheline tr.input.cache;
  tr.input.cache <- Done;
  if tr.input.mark land mark_new = mark_new then
    List.iter populate_cache tr.sub

let cleanup traces told tnew =
  List.iter clear_cache traces;
  clear_roots told tnew

let rec delta_transform traces map told tnew =
  mark_roots told tnew;
  List.iter populate_cache traces;
  let tape = ref [] in
  match apply tape map tnew with
  | exception exn ->
    cleanup traces told tnew;
    raise exn
  | result ->
    cleanup traces told tnew;
    (result, R (delta_transform !tape map tnew))

let transform map =
  R (fun input ->
      let tape = ref [] in
      let result = apply tape map input in
      (result, R (delta_transform !tape map input)))
