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

and 'a foldable = folder_kind -> folder -> 'a -> folder

and handle = Handle : 'a t -> handle [@@ocaml.unboxed]

and folder = handle list

and ('a, 'b) map = {
  mutable defined: bool;
  mutable func: tape -> 'a -> 'b;
  mutable finalize: 'a -> 'b -> unit;
  mutable slot: ('b * trace) option;
}

and trace = Trace : {
    map: ('a, 'b) map;
    input: 'a t;
    output: 'b;
    sub: trace list;
  } -> trace

and tape = trace list ref

let make fold value = {fold; value; mark = 0; cache = Done}
let peek t = t.value

let map ?finalize func =
  let finalize = match finalize with
    | None -> (fun _ _ -> ())
    | Some finalizer ->
      (fun a b -> try finalizer a b with exn ->
          Printf.eprintf "Lwd_alg.finalize exception: %s\n"
            (Printexc.to_string exn))
  in
  { defined = true; func; finalize; slot = None }

let undefined () = {
  defined = false;
  func = (fun _ -> failwith "Lwd_alg.undefined: map has not been defined before use");
  finalize = (fun _ _ -> ());
  slot = None;
}

let define map ?finalize func =
  if map.defined then
    invalid_arg "Lwd_alg.define: map has already been defined";
  map.defined <- true;
  map.func <- func;
  begin match finalize with
    | None -> ()
    | Some f -> map.finalize <- f
  end

(* How to update a computation, given a tape
   -----------------------------------------

   - Synchronized visit of the tape and the new value, to mark visited values
     and populate the value cache.
   - Now apply the user-provided computation:
     For each pair [(computation, value)], look into the value cache to find a
     past occurrence of this transformation
*)

external not_eq : _ map -> _ map -> bool = "%noteq"

let rec find_next_cached map = function
  | Done -> assert false
  | More {next = Done; _} -> ()
  | More ({next = More t' as next; _} as t) ->
    let Trace tr = t'.trace in
    if not_eq tr.map map then
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
      if not_eq tr.map map then
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
      saturate_marks (x.fold Accumulate xs x.value)
    ) else
      saturate_marks xs

let clear t xs =
  if t.mark = 0 then
    xs
  else (
    t.mark <- 0;
    Handle t :: xs
  )

let add (type a) kind xs (t : a t) =
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
    t.fold Mark_old acc t.value
  ) else if t.mark = mark_new then (
    t.fold Mark_new acc t.value
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
  (* [lor]: when [told == tnew], the root is shared and marking stops there *)
  tnew.mark <- tnew.mark lor mark_new;
  propagate_marks [] [Handle told; Handle tnew]

let rec clear_submarks = function
  | [] -> ()
  | Handle t :: xs ->
    assert (t.mark = 0);
    clear_submarks (t.fold Clear xs t.value)

let clear_roots told tnew =
  clear_submarks (clear told (clear tnew []))

(* Offering old traces for reuse
   -----------------------------

   After marking, a value is:
   - [mark_old]: only reachable from the old root, it has been dropped;
   - [mark_new] bit set: reachable from the new root (shared with the old one,
     or the new root itself when it is physically equal to the old one);
   - 0: never visited, the value lives below the frontier of shared values.

   The marked region is O(delta), and old traces are only walked inside it:
   traces of dropped values are traversed, and the first trace met on a
   shared value is offered for reuse without looking further: its whole
   sub-computation comes along if it is claimed by [apply].

   Not looking below the frontier also guarantees that a trace is never
   claimed more than once, so traces always form a tree. A trace that is not
   claimed is therefore dead along with all its sub-traces, which can be
   finalized without risking to finalize a trace that is still in use. *)

let rec populate_cache (Trace tr as trace) =
  let mark = tr.input.mark in
  if mark land mark_new <> 0 then
    tr.input.cache <- More {trace; next = tr.input.cache}
  else if mark = mark_old then
    List.iter populate_cache tr.sub

let rec finalize_trace (Trace tr) =
  tr.map.finalize tr.input.value tr.output;
  List.iter finalize_trace tr.sub

let rec clear_cacheline finalize = function
  | Done -> ()
  | More {trace; next} ->
    if finalize then finalize_trace trace;
    clear_cacheline finalize next

(* Symmetric with [populate_cache]: resets the caches populated during the
   traversal and, when [finalize] is set, finalizes the traces that have not
   been claimed. On failure, [finalize] is false: the old traces are kept for
   the next attempt. *)
let rec clear_cache finalize (Trace tr as trace) =
  let mark = tr.input.mark in
  if mark land mark_new <> 0 then (
    clear_cacheline finalize tr.input.cache;
    tr.input.cache <- Done
  ) else if mark = mark_old then (
    if finalize then tr.map.finalize tr.input.value tr.output;
    List.iter (clear_cache finalize) tr.sub
  ) else if finalize then
    (* Not visited by marking, but owned by a dropped trace *)
    finalize_trace trace

let cleanup ~finalize traces told tnew =
  List.iter (clear_cache finalize) traces;
  clear_roots told tnew

let delta_transform r map =
  let rec run traces told tnew =
    mark_roots told tnew;
    List.iter populate_cache traces;
    let tape = ref [] in
    match apply tape map tnew with
    | exception exn ->
      cleanup ~finalize:false traces told tnew;
      raise exn
    | result ->
      cleanup ~finalize:true traces told tnew;
      r := run !tape tnew;
      result
  in
  run

let transform map =
  let r = ref (fun _ -> assert false) in
  let first input =
    let tape = ref [] in
    let result = apply tape map input in
    r := delta_transform r map !tape input;
    result
  in
  r := first;
  fun x -> !r x

module Resumption = struct
  type ('a, 'b) r = {result: 'b; next: 'a -> ('a, 'b) r}

  let rec delta traces map told tnew =
    mark_roots told tnew;
    List.iter populate_cache traces;
    let tape = ref [] in
    match apply tape map tnew with
    | exception exn ->
      cleanup ~finalize:false traces told tnew;
      raise exn
    | result ->
      cleanup ~finalize:true traces told tnew;
      {result; next = delta !tape map tnew}

  (** Instantiating an incremental computation yields a resumption consumming
      incremental values. *)
  let of_map map input =
    let tape = ref [] in
    let result = apply tape map input in
    {result; next = delta !tape map input}
end
