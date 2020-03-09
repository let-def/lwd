(** Create-only version of [Obj.t] *)
module Any : sig
  type t
  val any : 'a -> t
end = struct
  type t = Obj.t
  let any = Obj.repr
end

type 'a t =
  | Pure of 'a
  | Impure of 'a (* NOTE: is this really used anywhere? *)
  | Operator : {
      mutable value : 'a option; (* cached value *)
      mutable trace : trace; (* list of parents this can invalidate *)
      mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
      desc: 'a desc;
    } -> 'a t
  | Root : {
      mutable value : 'a option; (* cached value *)
      mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
      mutable on_invalidate : 'a -> unit;
      mutable acquired : bool;
      child : 'a t;
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

(* a set of (active) parents for a ['a t], used during invalidation *)
and trace =
  | T0
  | T1 : _ t -> trace
  | T2 : _ t * _ t -> trace
  | T3 : _ t * _ t * _ t -> trace
  | T4 : _ t * _ t * _ t * _ t -> trace
  | Tn : { mutable active : int; mutable count : int;
           mutable entries : Any.t t array } -> trace

(* a set of direct children for a composite document *)
and trace_idx =
  | I0
  | I1 : { mutable idx : int ;
           obj : 'a t;
           mutable next : trace_idx } -> trace_idx

(* Basic combinators *)
let return x = Pure x
let pure x = Pure x

let impure = function
  | Pure x -> Impure x
  | other -> other

let dummy = Pure (Any.any ())

let operator desc =
  Operator { value = None; trace = T0; desc; trace_idx = I0 }

let map f x = match x with
  | Pure vx -> Pure (f vx)
  | x -> operator (Map (x, f))

let map2 f x y =
  match x, y with
  | Pure vx, Pure vy -> Pure (f vx vy)
  | _ -> operator (Map2 (x, y, f))

let map' x f = map f x
let map2' x y f = map2 f x y

let pair x y = match x, y with
  | Pure vx, Pure vy -> Pure (vx, vy)
  | _ -> operator (Pair (x, y))

let app f x = match f, x with
  | Pure vf, Pure vx -> Pure (vf vx)
  | _ -> operator (App (f, x))

let join child = match child with
  | Pure v -> v
  | _ -> operator (Join { child; intermediate = None })

let bind x f = join (map f x)

(* Management of trace indices *)

external t_equal : _ t -> _ t -> bool = "%eq"
external obj_t : 'a t -> Any.t t = "%identity"

let add_idx obj idx = function
  | Pure _ | Impure _ -> assert false
  | Root t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }
  | Operator t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }

let rec rem_idx_rec obj = function
  | I0 -> assert false
  | I1 t as self ->
    if t_equal t.obj obj
    then (t.idx, t.next)
    else (
      let idx, result = rem_idx_rec obj t.next in
      t.next <- result;
      (idx, self)
    )

(* remove [obj] from the lwd's trace. *)
let rem_idx obj = function
  | Pure _ | Impure _ -> assert false
  | Root t' ->
    let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx
  | Operator t' ->
    let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx

(* move [obj] from old index to new index. *)
let rec mov_idx_rec obj oldidx newidx = function
  | I0 -> assert false
  | I1 t ->
    if t.idx = oldidx && t_equal t.obj obj
    then t.idx <- newidx
    else mov_idx_rec obj oldidx newidx t.next

let mov_idx obj oldidx newidx = function
  | Pure _ | Impure _ -> assert false
  | Root t' -> mov_idx_rec obj oldidx newidx t'.trace_idx
  | Operator t' -> mov_idx_rec obj oldidx newidx t'.trace_idx

let rec get_idx_rec obj = function
  | I0 -> assert false
  | I1 t ->
    if t_equal t.obj obj
    then t.idx
    else get_idx_rec obj t.next

(* find index of [obj] in the given lwd *)
let get_idx obj = function
  | Pure _ | Impure _ -> assert false
  | Root t' -> get_idx_rec obj t'.trace_idx
  | Operator t' -> get_idx_rec obj t'.trace_idx

(* Propagating invalidation recursively.
   Each document is invalidated at most once,
   and only if it has [t.value = Some _]. *)
let rec invalidate_node : type a . a t -> unit = function
  | Pure _ | Impure _ -> assert false
  | Root { value = None; _ } -> ()
  | Root ({ value = Some x; _ } as t) ->
    t.value <- None;
    t.on_invalidate x (* user callback that {i observes} this root. *)
  | Operator t ->
    begin match t.value with
      | None -> ()
      | Some _ ->
        t.value <- None;
        invalidate_trace t.trace; (* invalidate parents recursively *)
    end

(* invalidate recursively documents in the given trace *)
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
let var x = operator (Var {binding = x})
let get x = x

let set (vx:_ var) x : unit =
  match vx with
  | Operator ({desc = Var v; _}) ->
    (* set the variable, and invalidate all observers *)
    invalidate_node vx;
    v.binding <- x
  | _ -> assert false

let peek = function
  | Operator ({desc = Var v; _}) -> v.binding
  | _ -> assert false

(* Primitives *)
type 'a prim = 'a t
let prim ~acquire ~release =
  operator (Prim { acquire; release })
let get_prim x = x

let invalidate = function
  | Operator ({ desc = Prim p; _ } as t) ->
    let value = t.value in
    t.value <- None;
    (* the value is invalidated, be sure to invalidate all parents as well *)
    invalidate_trace t.trace;
    begin match value with
      | None -> ()
      | Some v -> p.release v
    end
  | _ -> assert false

type release_failure = exn * Printexc.raw_backtrace
exception Release_failure of release_failure list

(* [sub_release [] origin self] is called when [origin] is released,
   where [origin] is reachable from [self]'s trace.
   We're going to remove [origin] from that trace as [origin] is now dead.

   [sub_release] cannot raise.
   If a primitive raises, the exception is caught and a warning is emitted. *)
let rec sub_release
  : type a b . release_failure list -> a t -> b t -> release_failure list
  = fun failures origin -> function
    | Root _ -> assert false
    | Pure _ | Impure _ -> failures
    | Operator t as self ->
      (* compute [t.trace \ {origin}] *)
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
            (* downgrade to [T4] to save space *)
            let a = tn.entries.(0) and b = tn.entries.(1) in
            let c = tn.entries.(2) and d = tn.entries.(3) in
            ignore (rem_idx self a : int);
            ignore (rem_idx self b : int);
            ignore (rem_idx self c : int);
            ignore (rem_idx self d : int);
            T4 (a, b, c, d)
          ) else (
            let len = Array.length tn.entries in
            if count <= len lsr 2 then
              Tn { active = tn.active; count = tn.count;
                   entries = Array.sub tn.entries 0 (len lsr 1) }
            else
              trace
          )
      in
      t.trace <- trace;
      match trace with
      | T0 ->
        (* [self] is not active anymore, since it's not reachable
           from any root. We can release its cached value and
           recursively release its subtree. *)
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
  | Pure _ | Impure _ -> ()
  | Operator t as self ->
    (* [acquire] is true if this is the first time this operator
       is used, in which case we need to acquire its children *)
    let acquire = match t.trace with T0 -> true | _ -> false in
    let trace = match t.trace with
      | T0 -> T1 origin
      | T1 x -> T2 (origin, x)
      | T2 (x, y) -> T3 (origin, x, y)
      | T3 (x, y, z) -> T4 (origin, x, y, z)
      | T4 (x, y, z, w) ->
        let obj_origin = obj_t origin in
        let entries =
          [| obj_t x; obj_t y; obj_t z; obj_t w; obj_origin; dummy; dummy; dummy |]
        in
        for i = 0 to 4 do add_idx self i entries.(i) done;
        Tn { active = 5; count = 5; entries }
      | Tn tn as trace ->
        let index = tn.count in
        let entries, trace =
          (* possibly resize array [entries] *)
          if index < Array.length tn.entries then (
            tn.count <- tn.count + 1;
            (tn.entries, trace)
          ) else (
            let entries = Array.make (index * 2) dummy in
            Array.blit tn.entries 0 entries 0 index;
            (entries, Tn { active = tn.active; count = index + 1; entries })
          )
        in
        let obj_origin = obj_t origin in
        entries.(index) <- obj_origin;
        add_idx self index obj_origin;
        trace
    in
    t.trace <- trace;
    if acquire then (
      (* acquire immediate children, and so on recursively *)
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
          | Some _ ->
            assert false (* this can't initialized already, first-time acquire *)
        end
      | Var  _ -> ()
      | Prim _ -> ()
    )

(* make sure that [origin] is in [self.trace], passed as last arg. *)
let activate_tracing self origin = function
  | Tn tn ->
    let idx = get_idx self origin in (* index of [self] in [origin.trace_idx] *)
    let active = tn.active in
    (* [idx < active] means [self] is already traced by [origin].
       We only have to add [self] to the entries if [idx >= active]. *)
    if idx >= active then (
      tn.active <- active + 1;
    );
    if idx > active then (
      (* swap with last entry in [tn.entries] *)
      let old = tn.entries.(active) in
      tn.entries.(idx) <- old;
      tn.entries.(active) <- obj_t origin;
      mov_idx self active idx old;
      mov_idx self idx active origin
    )
  | _ -> ()

(* [sub_sample origin self] computes a value for [self].

   [sub_sample] raise if any user-provided computation raises.
   Graph will be left in a coherent state but exception will be propagated
   to the observer. *)
let rec sub_sample : type a b . a t -> b t -> b = fun origin ->
  function
  | Root _ -> assert false
  | Pure x | Impure x -> x
  | Operator t as self ->
    (* try to use cached value, if present *)
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
              (* NOTE: if [intermediate==x'], should we stop there? *)
              (* release old value [x'], catching potential exceptions *)
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
      (* [self] just became active, so it may invalidate [origin] in case its
         value changes because of [t.desc], like if it's a variable and gets
         mutated, or if it's a primitive that gets invalidated.
         We need to put [origin] into [self.trace] in case it isn't there yet. *)
      activate_tracing self origin t.trace;
      value

type 'a root = 'a t

let observe ?(on_invalidate=ignore) child : _ root =
  let root = Root {
      child = child;
      value = None;
      on_invalidate;
      trace_idx = I0;
      acquired = false;
    } in
  root

let sample = function
  | Pure _ | Impure _ | Operator _ -> assert false
  | Root t as self ->
    match t.value with
    | Some value -> value
    | None ->
      (* no cached value, compute it now *)
      if not t.acquired then (
        t.acquired <- true;
        sub_acquire self t.child;
      );
      let value = sub_sample self t.child in
      t.value <- Some value; (* cache value *)
      value

let is_damaged = function
  | Pure _ | Impure _ | Operator _ -> assert false
  | Root { value = None ; _ } -> true
  | Root { value = Some _ ; _ } -> false

let release = function
  | Pure _ | Impure _ | Operator _ -> assert false
  | Root t as self ->
    if t.acquired then (
      (* release subtree, remove cached value *)
      t.value <- None;
      t.acquired <- false;
      match sub_release [] self t.child with
      | [] -> ()
      | failures -> raise (Release_failure failures)
    )

let set_on_invalidate x f =
  match x with
  | Pure _ | Impure _ | Operator _ -> assert false
  | Root t -> t.on_invalidate <- f
