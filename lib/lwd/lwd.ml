type 'a t =
  | Pure of 'a
  | Impure : {
      mutable value : 'a option;
      mutable trace : trace;
      mutable trace_idx : trace_idx;
      desc: 'a desc;
    } -> 'a t
  | Root : {
      mutable value : 'a option;
      mutable trace_idx : trace_idx;
      mutable on_invalidate : 'a -> unit;
      mutable child : 'a t option;
    } -> 'a t

and _ desc =
  | Map  : 'a t * ('a -> 'b) -> 'b desc
  | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c desc
  | Pair : 'a t * 'b t -> ('a * 'b) desc
  | App  : ('a -> 'b) t * 'a t -> 'b desc
  | Join : { child : 'a t t; mutable intermediate : 'a t option } -> 'a desc
  | Var  : { mutable binding : 'a } -> 'a desc
  | Prim : { acquire : unit -> 'a;
             release : 'a -> unit } -> 'a desc

and trace =
  | T0
  | T1 : _ t -> trace
  | T2 : _ t * _ t -> trace
  | T3 : _ t * _ t * _ t -> trace
  | T4 : _ t * _ t * _ t * _ t -> trace
  | Tn : { mutable active : int; mutable count : int;
           mutable entries : Obj.t t array } -> trace

and trace_idx =
  | I0
  | I1 : { mutable idx : int ;
           obj : 'a t;
           mutable next : trace_idx } -> trace_idx

(* Basic combinators *)
let return x = Pure x
let pure x = Pure x

let dummy = Pure (Obj.repr ())

let impure desc =
  Impure { value = None; trace = T0; desc; trace_idx = I0 }

let map f x = impure (Map (x, f))
let map2 f x y = impure (Map2 (x, y, f))
let map' x f = impure (Map (x, f))
let map2' x y f = impure (Map2 (x, y, f))
let pair x y = impure (Pair (x, y))
let app f x = impure (App (f, x))
let join child = impure (Join { child; intermediate = None })
let bind x f = join (map f x)

(* Management of trace indices *)

external t_equal : _ t -> _ t -> bool = "%eq"
external obj_t : 'a t -> Obj.t t = "%identity"

let add_idx obj idx = function
  | Pure _ -> assert false
  | Root t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }
  | Impure t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }

let rec rem_idx obj = function
  | I0 -> assert false
  | I1 t as self ->
    if t_equal t.obj obj
    then (t.idx, t.next)
    else
      let idx, result = rem_idx obj t.next in
      t.next <- result;
      (idx, self)

let rem_idx obj = function
  | Pure _ -> assert false
  | Root t' ->
    let idx, trace_idx = rem_idx obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx
  | Impure t' ->
    let idx, trace_idx = rem_idx obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx

let rec mov_idx obj oldidx newidx = function
  | I0 -> assert false
  | I1 t ->
    if t.idx = oldidx && t_equal t.obj obj
    then t.idx <- newidx
    else mov_idx obj oldidx newidx t.next

let mov_idx obj oldidx newidx = function
  | Pure _ -> assert false
  | Root t' -> mov_idx obj oldidx newidx t'.trace_idx
  | Impure t' -> mov_idx obj oldidx newidx t'.trace_idx

let rec get_idx obj = function
  | I0 -> assert false
  | I1 t ->
    if t_equal t.obj obj
    then t.idx
    else get_idx obj t.next

let get_idx obj = function
  | Pure _ -> assert false
  | Root t' -> get_idx obj t'.trace_idx
  | Impure t' -> get_idx obj t'.trace_idx

(* Propagating invalidation *)
let rec invalidate_node : type a . a t -> unit = function
  | Pure _ -> assert false
  | Root { value = None; _ } -> ()
  | Root ({ value = Some x; _ } as t) ->
    t.value <- None;
    t.on_invalidate x
  | Impure t ->
    begin match t.value with
      | None -> ()
      | Some _ ->
        t.value <- None;
        invalidate_trace t.trace
    end

and invalidate_trace = function
  | T0 -> ()
  | T1 x -> invalidate_node x
  | T2 (x, y) ->
    invalidate_node x;
    invalidate_node y
  | T3 (x, y, z) ->
    invalidate_node x;
    invalidate_node y;
    invalidate_node z
  | T4 (x, y, z, w) ->
    invalidate_node x;
    invalidate_node y;
    invalidate_node z;
    invalidate_node w
  | Tn t ->
    let active = t.active in
    t.active <- 0;
    for i = 0 to active - 1 do
      invalidate_node t.entries.(i)
    done

(* Variables *)
type 'a var = 'a t
let var x = impure (Var {binding = x})
let get x = x

let set vx x =
  match vx with
  | Impure ({desc = Var v; _}) ->
    invalidate_node vx;
    v.binding <- x
  | _ -> assert false

let peek = function
  | Impure ({desc = Var v; _}) -> v.binding
  | _ -> assert false

(* Primitives *)
type 'a prim = 'a t
let prim ~acquire ~release =
  impure (Prim { acquire; release })
let get_prim x = x

let invalidate = function
  | Impure ({ desc = Prim p; _ } as t) ->
    let value = t.value in
    t.value <- None;
    invalidate_trace t.trace;
    begin match value with
      | None -> ()
      | Some v -> p.release v
    end
  | _ -> assert false

type release_failure = exn * Printexc.raw_backtrace
exception Release_failure of release_failure list

(* [sub_release] cannot raise.
   If a primitive raises, the exception is caught and a warning is emitted. *)
let rec sub_release
  : type a b . release_failure list -> a t -> b t -> release_failure list
  = fun failures origin -> function
    | Root _ -> assert false
    | Pure _ -> failures
    | Impure t as self ->
      let trace = match t.trace with
        | T0 -> assert false
        | T1 x -> assert (t_equal x origin); T0
        | T2 (x, y) ->
          if t_equal x origin then T1 y
          else if t_equal y origin then T1 x
          else assert false
        | T3 (x, y, z) ->
          if t_equal x origin then T2 (y, z)
          else if t_equal y origin then T2 (x, z)
          else if t_equal z origin then T2 (x, y)
          else assert false
        | T4 (x, y, z, w) ->
          if t_equal x origin then T3 (y, z, w)
          else if t_equal y origin then T3 (x, z, w)
          else if t_equal z origin then T3 (x, y, w)
          else assert false
        | Tn tn as trace ->
          let revidx = rem_idx self origin in
          assert (t_equal tn.entries.(revidx) origin);
          let count = tn.count - 1 in
          tn.count <- count;
          if revidx < count then (
            let obj = tn.entries.(count) in
            tn.entries.(revidx) <- obj;
            mov_idx self count revidx obj
          );
          tn.entries.(count) <- dummy;
          if tn.active > count then tn.active <- count;
          if count = 4 then (
            let a = tn.entries.(0) and b = tn.entries.(1) in
            let c = tn.entries.(2) and d = tn.entries.(3) in
            ignore (rem_idx self a : int);
            ignore (rem_idx self b : int);
            ignore (rem_idx self c : int);
            ignore (rem_idx self d : int);
            T4 (a, b, c, d)
          ) else
            let len = Array.length tn.entries in
            if count <= len lsr 2 then
              Tn { active = tn.active; count = tn.count;
                   entries = Array.sub tn.entries 0 (len lsr 1) }
            else
              trace
      in
      t.trace <- trace;
      match trace with
      | T0 ->
        let value = t.value in
        t.value <- None;
        begin match t.desc with
          | Map  (x, _) -> sub_release failures self x
          | Map2 (x, y, _) ->
            sub_release (sub_release failures self x) self y
          | Pair (x, y) ->
            sub_release (sub_release failures self x) self y
          | App  (x, y) ->
            sub_release (sub_release failures self x) self y
          | Join ({ child; intermediate } as t) ->
            let failures = sub_release failures self child in
            begin match intermediate with
              | None -> failures
              | Some child' ->
                t.intermediate <- None;
                sub_release failures self child'
            end
          | Var  _ -> failures
          | Prim t ->
            begin match value with
              | None -> failures
              | Some x ->
                begin match t.release x with
                  | () -> failures
                  | exception exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    (exn, bt) :: failures
                end
            end
        end
      | _ -> failures

(* [sub_acquire] cannot raise *)
let rec sub_acquire : type a b . a t -> b t -> unit = fun origin ->
  function
  | Root _ -> assert false
  | Pure _ -> ()
  | Impure t as self ->
    let acquire = match t.trace with T0 -> true | _ -> false in
    let trace = match t.trace with
      | T0 -> T1 origin
      | T1 x -> T2 (origin, x)
      | T2 (x, y) -> T3 (origin, x, y)
      | T3 (x, y, z) -> T4 (origin, x, y, z)
      | T4 (x, y, z, w) ->
        let obj = obj_t origin in
        let entries =
          [| obj_t x; obj_t y; obj_t z; obj_t w; obj; dummy; dummy; dummy |]
        in
        for i = 0 to 4 do add_idx self i entries.(i) done;
        Tn { active = 5; count = 5; entries }
      | Tn tn as trace ->
        let index = tn.count in
        let entries, trace =
          if index < Array.length tn.entries then (
            tn.count <- tn.count + 1;
            (tn.entries, trace)
          ) else (
            let entries = Array.make (index * 2) dummy in
            Array.blit tn.entries 0 entries 0 index;
            (entries, Tn { active = tn.active; count = index + 1; entries })
          )
        in
        let obj = obj_t origin in
        entries.(index) <- obj;
        add_idx self index obj;
        trace
    in
    t.trace <- trace;
    if acquire then
      match t.desc with
      | Map  (x, _) -> sub_acquire self x
      | Map2 (x, y, _) ->
        sub_acquire self x;
        sub_acquire self y
      | Pair (x, y) ->
        sub_acquire self x;
        sub_acquire self y
      | App  (x, y) ->
        sub_acquire self x;
        sub_acquire self y
      | Join { child; intermediate } ->
        sub_acquire self child;
        begin match intermediate with
          | None -> ()
          | Some _ -> assert false
        end
      | Var  _ -> ()
      | Prim _ -> ()

let activate_tracing self origin = function
  | Tn tn ->
    let idx = get_idx self origin in
    let active = tn.active in
    if idx >= active then
      tn.active <- active + 1;
    if idx > active then (
      let old = tn.entries.(active) in
      tn.entries.(idx) <- old;
      tn.entries.(active) <- obj_t origin;
      mov_idx self active idx old;
      mov_idx self idx active origin
    )
  | _ -> ()

(* [sub_sample] raise if any user-provided computation raises.
   Graph will be left in a coherent state but exception will be propagated
   to the observer. *)
let rec sub_sample : type a b . a t -> b t -> b = fun origin ->
  function
  | Root _ -> assert false
  | Pure x -> x
  | Impure t as self ->
    match t.value with
    | Some value -> value
    | None ->
      let value : b = match t.desc with
        | Map  (x, f) -> f (sub_sample self x)
        | Map2 (x, y, f) -> f (sub_sample self x) (sub_sample self y)
        | Pair (x, y) -> (sub_sample self x, sub_sample self y)
        | App  (f, x) -> (sub_sample self f) (sub_sample self x)
        | Join x ->
          let old_intermediate = x.intermediate in
          let intermediate =
            (* We haven't touched any state yet,
               it is safe for [sub_sample] to raise *)
            sub_sample self x.child
          in
          x.intermediate <- Some intermediate;
          sub_acquire self intermediate;
          let result = sub_sample self intermediate in
          begin match old_intermediate with
            | None -> result
            | Some x' ->
              match sub_release [] self x' with
              | [] -> result
              | failures ->
                (* Commit result, just like normal continuation *)
                t.value <- Some result;
                activate_tracing self origin t.trace;
                (* Raise release exception *)
                raise (Release_failure failures)
          end
        | Var  x -> x.binding
        | Prim t -> t.acquire ()
      in
      t.value <- Some value;
      activate_tracing self origin t.trace;
      value

type 'a root = 'a t

(* TODO: use of Root after release is not detected and will break invariant *)
let observe ?(on_invalidate=ignore) child =
  let root = Root {
      child = Some child;
      value = None;
      on_invalidate;
      trace_idx = I0
    } in
  sub_acquire root child;
  root

let sample = function
  | Pure _ | Impure _ -> assert false
  | Root t as self ->
    match t.value with
    | Some value -> value
    | None ->
      match t.child with
      | None -> invalid_arg "sample: root has been released"
      | Some child ->
        let value = sub_sample self child in
        t.value <- Some value;
        value

let is_damaged = function
  | Pure _ | Impure _ -> assert false
  | Root { value = None ; _ } -> true
  | Root { value = Some _ ; _ } -> false

let is_released = function
  | Pure _ | Impure _ -> assert false
  | Root { child = None ; _ } -> true
  | Root { child = Some _ ; _ } -> false

let release = function
  | Pure _ | Impure _ -> assert false
  | Root t as self ->
    match t.child with
    | None -> ()
    | Some child ->
      t.value <- None;
      t.child <- None;
      match sub_release [] self child with
      | [] -> ()
      | failures -> raise (Release_failure failures)

let set_on_invalidate x f =
  match x with
  | Pure _ | Impure _ -> assert false
  | Root t -> t.on_invalidate <- f

let unsafe_peek = function
  | Pure x -> Some x
  | Impure t -> t.value
  | Root t -> t.value

module Infix = struct
  let (let$) = bind
  let (and$) = pair
  let ($=) = set
end
