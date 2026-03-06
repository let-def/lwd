
type !'a t = {
  value: 'a;
  mutable mark: int;
  foldable: 'a foldable;
  mutable cache: cache;
}

and cache =
  | More of {trace: trace; mutable next: cache}
  | Done

and 'a foldable = 'a -> folder -> folder

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

let accumulate (type a) (t : a t) xs = Handle t :: xs

let make value foldable = {foldable; value; mark = 0; cache = Done}
let peek t = t.value

type ('a, 'b) resumption = R of ('a -> 'b * ('a, 'b) resumption) [@@ocaml.unboxed]

let map ?(finalize=fun _ _ -> ()) func = { func; finalize; slot = None }

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

(* val transform : ('a, 'b) map -> ('a t, 'b t) resumption *)
