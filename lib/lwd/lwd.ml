(**
    Lwd (Lightweight Document)
    A library for incremental computation. It builds a dependency graph where
    nodes cache their values and propagate invalidations upward when dependencies change.
*)

(** The state of a cached value. *)
type 'a eval =
  | Eval_none       (** Value is not computed or has been invalidated *)
  | Eval_progress   (** Value is currently being computed (used to detect cycles) *)
  | Eval_some of 'a (** Value is cached and valid *)

(** The internal representation of a node in the dependency graph. *)
type 'a t_ =
  | Pure of 'a (** A constant value that never changes *)
  | Operator : {
      mutable value : 'a eval;       (** Cached result of the operation *)
      mutable trace : trace;         (** Upward links: nodes that depend on this node *)
      mutable trace_idx : trace_idx; (** Downward links: nodes this node depends on *)
      desc: 'a desc;                 (** The logic/combinator defining this node *)
    } -> 'a t_
  | Root : {
      mutable value : 'a eval;       (** Cached result for the observer *)
      mutable trace_idx : trace_idx; (** Downward links to the child graph *)
      mutable on_invalidate : 'a -> unit; (** Callback triggered when an observation becomes invalid *)
      mutable acquired : bool;       (** Whether the node is being observed at the moment *)
      child : 'a t_;                 (** The node to observe *)
    } -> 'a t_

(** The definition of an operator node. *)
and _ desc =
  (* Lwd is a functor: *)
  | Map  : 'a t_ * ('a -> 'b) -> 'b desc
  (** Transforms a value *)
  (* ... an applicative-functor: *)
  | Map2 : 'a t_ * 'b t_ * ('a -> 'b -> 'c) -> 'c desc
  (** Applicative functor: Combines two values *)
  | Pair : 'a t_ * 'b t_ -> ('a * 'b) desc
  (** Applicative functor: Groups two values *)
  | App  : ('a -> 'b) t_ * 'a t_ -> 'b desc
  (** Applicative functor: Applies a computed function to a computed value *)
  (* ... a monad: *)
  | Join : { child : 'a t_ t_; mutable intermediate : 'a t_ option } -> 'a desc
  (** Flattens a nested computation *)
  (* Lwd specific nodes: *)
  | Var  : { mutable binding : 'a } -> 'a desc
  (** A mutable leaf node (the source of changes) *)
  | Prim : { acquire : 'a t -> 'a;
             release : 'a t -> 'a -> unit } -> 'a desc
  (** An external resource/primitive *)
  | Fix : { doc : 'a t_; wrt : _ t_ } -> 'a desc
  (** A fixed-point computation (computation that might not converge in a
      single evaluation, because of internal mutations) *)

(**
   A 'trace' is a set of parents to invalidate when the value change.
   To optimize memory, it uses specialized constructors for small numbers of
   parents (T1-T4) before falling back to a dynamic array (Tn).
*)
and trace =
  | T0
  | T1 : _ t_ -> trace
  | T2 : _ t_ * _ t_ -> trace
  | T3 : _ t_ * _ t_ * _ t_ -> trace
  | T4 : _ t_ * _ t_ * _ t_ * _ t_ -> trace
  | Tn : { mutable active : int; mutable count : int;
           mutable entries : Obj.t t_ array } -> trace

(** A `trace index' remembers the position of a parent node in the trace
    of one of its children.
    This is needed to keep maintenance O(1) when a node has a trace of
    the form [Tn _] (many dependencies).
    In this case the idx has the form [I1 {idx; obj; next}], where:
    - obj is the children with a trace Tn array
    - idx is the index of the node in array.entries
 *)
and trace_idx =
  | I0
  | I1 : { mutable idx : int ;
           obj : Obj.t t_;
           mutable next : trace_idx } -> trace_idx

(**
    The public handle to a node.
    A node is covariant in its parameter, but the OCaml typechecker cannot know
    that. We use explicit coercions to expose the covariant type in the public API.
*)
and +'a t
external inj : 'a t_ -> 'a t = "%identity"
external prj : 'a t -> 'a t_ = "%identity"
external prj2 : 'a t t -> 'a t_ t_ = "%identity"

(* --- Basic Combinators --- *)

let return x = inj (Pure x)
let pure x = inj (Pure x)

(** Returns the value if the node is a constant, otherwise None. *)
let is_pure x = match prj x with
  | Pure x -> Some x
  | _ -> None

(* Sometime we don't care about the actual type of a computation.
   To allow heterogeneous storage of obj in traces, we inject
   them to the `top` type Obj.t t_.*)
external obj_t : 'a t_ -> Obj.t t_ = "%identity"

(** Internal dummy node used to fill holes in trace arrays. *)
let dummy = obj_t (Pure ())

(** Internal helper to create an operator node. *)
let operator desc =
  Operator { value = Eval_none; trace = T0; desc; trace_idx = I0 }

let map x ~f = inj (
    match prj x with
    | Pure vx -> Pure (f vx)
    | x -> operator (Map (x, f))
  )

let map2 x y ~f = inj (
    match prj x, prj y with
    | Pure vx, Pure vy -> Pure (f vx vy)
    | x, y -> operator (Map2 (x, y, f))
  )

let pair x y = inj (
    match prj x, prj y with
    | Pure vx, Pure vy -> Pure (vx, vy)
    | x, y -> operator (Pair (x, y))
  )

let app f x = inj (
    match prj f, prj x with
    | Pure vf, Pure vx -> Pure (vf vx)
    | f, x -> operator (App (f, x))
  )

let join child = inj (
    match prj2 child with
    | Pure v -> v
    | child -> operator (Join { child; intermediate = None })
  )

let bind x ~f = join (map ~f x)

(* --- Management of trace indices (Downward links) --- *)

let addr oc obj =
  Printf.fprintf oc "0x%08x" (Obj.magic obj : int)

(** Debugging: Prints the parent trace of a node. *)
let rec dump_trace : type a. a t_ -> unit =
  fun obj -> match obj with
  | Pure _ -> Printf.eprintf "%a: Pure _\n%!" addr obj
  | Operator t ->
    Printf.eprintf "%a: Operator _ -> %a\n%!" addr obj dump_trace_aux t.trace;
    begin match t.trace with
      | T0 -> ()
      | T1 a -> dump_trace a
      | T2 (a,b) -> dump_trace a; dump_trace b
      | T3 (a,b,c) -> dump_trace a; dump_trace b; dump_trace c
      | T4 (a,b,c,d) -> dump_trace a; dump_trace b; dump_trace c; dump_trace d
      | Tn t -> Array.iter dump_trace t.entries
    end
  | Root _ -> Printf.eprintf "%a: Root _\n%!" addr obj

and dump_trace_aux oc = function
  | T0 -> Printf.fprintf oc "T0"
  | T1 a -> Printf.fprintf oc "T1 %a" addr a
  | T2 (a,b) ->
    Printf.fprintf oc "T2 (%a, %a)" addr a addr b
  | T3 (a,b,c) ->
    Printf.fprintf oc "T3 (%a, %a, %a)" addr a addr b addr c
  | T4 (a,b,c,d) ->
    Printf.fprintf oc "T4 (%a, %a, %a, %a)" addr a addr b addr c addr d
  | Tn t ->
    Printf.fprintf oc "Tn {active = %d; count = %d; entries = "
      t.active t.count;
    Array.iter (Printf.fprintf oc "(%a)" addr) t.entries;
    Printf.fprintf oc "}"

let dump_trace x = dump_trace (obj_t (prj x))

(** Registers that [obj] is a child of the given node. *)
let add_idx obj idx = function
  | Pure _ -> assert false
  | Root t' -> t'.trace_idx <- I1 { idx; obj = obj_t obj; next = t'.trace_idx }
  | Operator t' -> t'.trace_idx <- I1 { idx; obj = obj_t obj; next = t'.trace_idx }

(** Helper to remove a child from the trace index list. *)
let rec rem_idx_rec obj = function
  | I0 -> assert false
  | I1 t as self ->
    if t.obj == obj_t obj
    then (t.idx, t.next)
    else (
      let idx, result = rem_idx_rec obj t.next in
      t.next <- result;
      (idx, self)
    )

(** Removes [obj] from the node's child trace and returns its index. *)
let rem_idx obj = function
  | Pure _ -> assert false
  | Root t' ->
    let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx
  | Operator t' ->
    let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
    t'.trace_idx <- trace_idx; idx

(** Updates the index of a child node. *)
let rec mov_idx_rec obj oldidx newidx = function
  | I0 -> assert false
  | I1 t ->
    if t.idx = oldidx && t.obj == obj_t obj
    then t.idx <- newidx
    else mov_idx_rec obj oldidx newidx t.next

let mov_idx obj oldidx newidx = function
  | Pure _ -> assert false
  | Root t' -> mov_idx_rec obj oldidx newidx t'.trace_idx
  | Operator t' -> mov_idx_rec obj oldidx newidx t'.trace_idx

let rec get_idx_rec obj = function
  | I0 -> assert false
  | I1 t ->
    if t.obj == obj_t obj
    then t.idx
    else get_idx_rec obj t.next

(** Finds the index of [obj] in the given node's child trace. *)
let get_idx obj = function
  | Pure _ -> assert false
  | Root t' -> get_idx_rec obj t'.trace_idx
  | Operator t' -> get_idx_rec obj t'.trace_idx

(* --- Logging unsafe usages of Lwd API --- *)

type unsafe_action = [`Mutation | `Nested_sampling]

let unsafe_action_explanation = function
  | `Mutation -> "unsafe mutation (variable invalidated during evaluation)"
  | `Nested_sampling -> "nested sampling (sampling a root in its invalidation callback)"

let default_unsafe_action_logger action =
  let callstack = Printexc.get_callstack 20 in
  Printf.fprintf stderr
    "Lwd: %s at\n%a"
    (unsafe_action_explanation action)
    Printexc.print_raw_backtrace callstack

let unsafe_action_logger = ref default_unsafe_action_logger

(* --- Invalidation Logic --- *)

type status =
  | Neutral (** No issues detected *)
  | Safe    (** Invalidation occurred, but it's safe *)
  | Unsafe  (** Invalidation occurred during evaluation (a bug/race) *)

(* The invalidation status evolves monotonically: it starts Neutral, can become
   Safe then Unsafe, but can't go back. *)

(* Called when we found that invalidation is safe in the current
   context. *)
let mark_safe status =
  match !status with
  | Neutral -> status := Safe
  | Unsafe | Safe ->
     (* Status has already been marked in another context.
        If it was unsafe, we can't go back.
        If is was safe, nothing to change. *)
     ()

type sensitivity =
  | Strong   (** Force invalidation regardless of state *)
  | Fragile  (** Only invalidate if the node is in a specific state *)

(**
    Recursively invalidates a node and its parents.
    This is the "Push" phase of the incremental system.
*)
let rec invalidate_node : type a . status ref -> sensitivity -> a t_ -> unit =
  fun status sensitivity node ->
  match node, sensitivity with
  | Pure _, _ -> assert false
  | Root ({value; _} as t), _ ->
    t.value <- Eval_none;
    begin match value with
      | Eval_none -> ()
      | Eval_progress ->
        (* Invalidating a root being evaluated: unsafe *)
        status := Unsafe
      | Eval_some x ->
        begin match sensitivity with
          | Strong -> ()
          | Fragile -> status := Unsafe
        end;
        t.on_invalidate x; (* user callback that {i observes} this root. *)
        begin match t.value with
        | Eval_none -> ()
        | _ -> !unsafe_action_logger `Nested_sampling
        end;
    end
  | Operator {value = Eval_none; _}, Fragile ->
    mark_safe status
  | Operator {value = Eval_none; _}, _ -> ()
  | Operator {desc = Fix {wrt = Operator {value = Eval_none; _}; _}; _}, Fragile ->
    mark_safe status
  | Operator {desc = Fix {wrt = Operator {value = Eval_some _; _}; _}; _}, Fragile ->
    ()
  | Operator t, _ ->
    let sensitivity =
      match t.value with Eval_progress -> Fragile | _ -> sensitivity
    in
    t.value <- Eval_none;
    (* Propagate invalidation to parents *)
    invalidate_trace status sensitivity t.trace

(** Iterates through the parent trace to invalidate all observers. *)
and invalidate_trace status sensitivity = function
  | T0 -> ()
  | T1 x -> invalidate_node status sensitivity x
  | T2 (x, y) ->
    invalidate_node status sensitivity x;
    invalidate_node status sensitivity y
  | T3 (x, y, z) ->
    invalidate_node status sensitivity x;
    invalidate_node status sensitivity y;
    invalidate_node status sensitivity z
  | T4 (x, y, z, w) ->
    invalidate_node status sensitivity x;
    invalidate_node status sensitivity y;
    invalidate_node status sensitivity z;
    invalidate_node status sensitivity w
  | Tn t ->
    let active = t.active in
    t.active <- 0;
    for i = 0 to active - 1 do
      invalidate_node status sensitivity t.entries.(i)
    done

let do_invalidate sensitivity node =
  let status = ref Neutral in
  invalidate_node status sensitivity node;
  let unsafe =
    match !status with
    | Neutral | Safe -> false
    | Unsafe -> true
  in
  if unsafe then !unsafe_action_logger `Mutation

(* --- Variables --- *)

type 'a var = 'a t_
let var x = operator (Var {binding = x})
let get x = inj x

(** Updates a variable's value and triggers invalidation of all its parents. *)
let set (vx:_ var) x : unit =
  match vx with
  | Operator ({desc = Var v; _}) ->
    v.binding <- x;
    do_invalidate Strong vx
  | _ -> assert false

let peek = function
  | Operator ({desc = Var v; _}) -> v.binding
  | _ -> assert false

let update f v = set v (f (peek v))

let may_update f v =
  match f (peek v) with
  | None -> ()
  | Some x -> set v x

(* --- Primitives --- *)

type 'a prim = 'a t
let prim ~acquire ~release =
  inj (operator (Prim { acquire; release }))
let get_prim x = x

(** Manually invalidates a primitive node and calls its release function. *)
let invalidate x = match prj x with
  | Operator {desc = Prim p; value; _} as t ->
    begin match value with
      | Eval_none -> ()
      | Eval_progress -> do_invalidate Fragile t;
      | Eval_some v ->
        do_invalidate Strong t;
        p.release x v
    end
  | _ -> assert false

(* --- Fix point --- *)

let fix doc ~wrt = match prj wrt with
  | Root _ -> assert false
  | Pure _ -> doc
  | Operator _ as wrt -> inj (operator (Fix {doc = prj doc; wrt}))

(* --- Resource Lifecycle Management --- *)

type release_list =
  | Release_done
  | Release_more :
      { origin : 'a t_; element : 'b t_; next : release_list } -> release_list

type release_queue = release_list ref
let make_release_queue () = ref Release_done

type release_failure = exn * Printexc.raw_backtrace

(**
    Handles the removal of a node from the graph.
    When a node is no longer reachable from any root, it is "released",
    which may trigger further releases of its children.

    [sub_release [] origin self] is called when [origin] is released,
    where [origin] is reachable from [self]'s trace.
    We're going to remove [origin] from that trace as [origin] is now dead.

    [sub_release] cannot raise.
    If a primitive raises, the exception is caught and returned in the failure
    list.
*)
let rec sub_release
  : type a b . release_failure list -> a t_ -> b t_ -> release_failure list
  = fun failures origin -> function
    | Root _ -> assert false
    | Pure _ -> failures
    | Operator t as self ->
      (* Remove the origin from the parent trace *)
      let origin = obj_t origin in
      let trace = match t.trace with
        | T0 -> assert false
        | T1 x -> assert (obj_t x == origin); T0
        | T2 (x, y) ->
          if obj_t x == origin then T1 y
          else if obj_t y == origin then T1 x
          else assert false
        | T3 (x, y, z) ->
          if obj_t x == origin then T2 (y, z)
          else if obj_t y == origin then T2 (x, z)
          else if obj_t z == origin then T2 (x, y)
          else assert false
        | T4 (x, y, z, w) ->
          if obj_t x == origin then T3 (y, z, w)
          else if obj_t y == origin then T3 (x, z, w)
          else if obj_t z == origin then T3 (x, y, w)
          else if obj_t w == origin then T3 (x, y, z)
          else assert false
        | Tn tn as trace ->
          let revidx = rem_idx self origin in
          assert (tn.entries.(revidx) == origin);
          let count = tn.count - 1 in
          tn.count <- count;
          if revidx < count then (
            let obj = tn.entries.(count) in
            tn.entries.(revidx) <- obj;
            tn.entries.(count) <- dummy;
            mov_idx self count revidx obj
          ) else
            tn.entries.(revidx) <- dummy;
          if tn.active > count then tn.active <- count;
          if count = 4 then (
            (* Downgrade to T4 to save memory *)
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
        t.value <- Eval_progress;
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
          | Fix {doc; wrt} ->
            sub_release (sub_release failures self wrt) self doc
          | Prim t ->
            begin match value with
              | Eval_none  | Eval_progress -> failures
              | Eval_some x ->
                begin match t.release (inj self) x with
                  | () -> failures
                  | exception exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    (exn, bt) :: failures
                end
            end
        end
      | _ -> failures

(**
    Activates a node and its children.
    This is called when a Root is first sampled.

    [sub_acquire] cannot raise.
*)
let rec sub_acquire : type a b . a t_ -> b t_ -> unit = fun origin ->
  function
  | Root _ -> assert false
  | Pure _ -> ()
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
      | Fix  {doc; wrt} ->
        sub_acquire self doc;
        sub_acquire self wrt
      | Join { child; intermediate } ->
        sub_acquire self child;
        begin match intermediate with
          | None -> ()
          | Some _ ->
            assert false (* this can't be initialized already, first-time acquire *)
        end
      | Var  _ -> ()
      | Prim _ -> ()
    )

(** Activate [origin] in the trace of [self] if necessary. *)
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

let sub_is_damaged = function
  | Root _ -> assert false
  | Pure _ -> false
  | Operator {value; _} ->
    match value with
    | Eval_none -> true
    | Eval_some _ -> false
    | Eval_progress -> assert false

(**
    The "Pull" phase. Computes the value of a node by recursively
    sampling its dependencies.

    [sub_sample] raise if any user-provided computation raises.
    Graph will be left in a coherent state but exception will be propagated
    to the observer.
*)
let sub_sample queue =
  let rec aux : type a b . a t_ -> b t_ -> b = fun origin ->
    function
    | Root _ -> assert false
    | Pure x -> x
    | Operator t as self ->
      (* try to use cached value, if present *)
      match t.value with
      | Eval_some value ->
        activate_tracing self origin t.trace;
        value
      | _ ->
        t.value <- Eval_progress;
        let result : b = match t.desc with
          | Map  (x, f) -> f (aux self x)
          | Map2 (x, y, f) -> f (aux self x) (aux self y)
          | Pair (x, y) -> (aux self x, aux self y)
          | App  (f, x) -> (aux self f) (aux self x)
          | Fix {doc; wrt} ->
            let _ = aux self wrt in
            let result = aux self doc in
            if sub_is_damaged wrt then
              aux origin self
            else (
              if sub_is_damaged doc then
                do_invalidate Fragile self;
              result
            )
          | Join x ->
            let intermediate =
              (* We haven't touched any state yet,
                 it is safe for [aux] to raise *)
              aux self x.child
            in
            begin match x.intermediate with
              | None ->
                x.intermediate <- Some intermediate;
                sub_acquire self intermediate;
              | Some x' when x' != intermediate ->
                queue := Release_more {
                    origin = self;
                    element = x';
                    next = !queue;
                  };
                x.intermediate <- Some intermediate;
                sub_acquire self intermediate;
              | Some _ -> ()
            end;
            aux self intermediate
          | Var  x -> x.binding
          | Prim t -> t.acquire (inj self)
        in
        begin match t.value with
          | Eval_progress -> t.value <- Eval_some result;
          | Eval_none | Eval_some _ -> ()
        end;
        (* [self] just became active, so it may invalidate [origin] in case its
           value changes because of [t.desc], like if it's a variable and gets
           mutated, or if it's a primitive that gets invalidated.
           We need to put [origin] into [self.trace] in case it isn't there yet. *)
        activate_tracing self origin t.trace;
        result
  in
  aux

type 'a root = 'a t

(** Creates a root observer for a computation. *)
let observe ?(on_invalidate=ignore) child : _ root =
  let root = Root {
      child = prj child;
      value = Eval_none;
      on_invalidate;
      trace_idx = I0;
      acquired = false;
    } in
  inj root

exception Release_failure of exn option * release_failure list

let raw_flush_release_queue queue =
  let rec aux failures = function
    | Release_done -> failures
    | Release_more t ->
      let failures = sub_release failures t.origin t.element in
      aux failures t.next
  in
  aux [] queue

let flush_release_queue queue =
  let queue' = !queue in
  queue := Release_done;
  raw_flush_release_queue queue'

(** Samples the value of a root. *)
let sample queue x = match prj x with
  | Pure _ | Operator _ -> assert false
  | Root t as self ->
    match t.value with
    | Eval_some value -> value
    | _ ->
      (* no cached value, compute it now *)
      if not t.acquired then (
        t.acquired <- true;
        sub_acquire self t.child;
      );
      t.value <- Eval_progress;
      let value = sub_sample queue self t.child in
      begin match t.value with
        | Eval_progress -> t.value <- Eval_some value; (* cache value *)
        | Eval_none | Eval_some _ -> ()
      end;
      value

let is_damaged x = match prj x with
  | Pure _ | Operator _ -> assert false
  | Root {value = Eval_some _; _} -> false
  | Root {value = Eval_none | Eval_progress; _} -> true

(** Releases a root and its subtree. *)
let release queue x = match prj x with
  | Pure _ | Operator _ -> assert false
  | Root t as self ->
    if t.acquired then (
      (* release subtree, remove cached value *)
      t.value <- Eval_none;
      t.acquired <- false;
      queue := Release_more { origin = self; element = t.child; next = !queue }
    )

let set_on_invalidate x f =
  match prj x with
  | Pure _ | Operator _ -> assert false
  | Root t -> t.on_invalidate <- f

let flush_or_fail main_exn queue =
  match flush_release_queue queue with
  | [] -> ()
  | failures -> raise (Release_failure (main_exn, failures))

(** High-level API to sample a root without managing the release queue manually. *)
let quick_sample root =
  let queue = ref Release_done in
  match sample queue root with
  | result -> flush_or_fail None queue; result
  | exception exn -> flush_or_fail (Some exn) queue; raise exn

(** High-level API to release a root. *)
let quick_release root =
  let queue = ref Release_done in
  release queue root;
  flush_or_fail None queue

module Infix = struct
  let (>>=) x f = bind x ~f
  let (>|=) x f = map x ~f
  let (<*>) = app
end

(*$R
  let x = var 0 in
  let y = map ~f:succ (get x) in
  let o_y = Lwd.observe y in
  assert_equal 1 (quick_sample o_y);
  set x 10;
  assert_equal 11 (quick_sample o_y);
  *)
