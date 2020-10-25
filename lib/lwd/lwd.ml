(** For various reasons (mostly to avoid too many indirections), Lwd is made of
    different components that are tightly coupled.

    The type definitions below are all made in a single place, forming one, big
    recursive definition.

    Then individual modules implement each functionality. *)

type any
(** This is a "Top" type. Together with a ['a t_ -> any t_] injection
    function, it serves as an encoding of existentials for functions that only
    care about the structure of an Lwd graph (represented by type t_ defined
    below), and not about actual values. *)

(** The [eval] type represents the different states of an lwd value *)
type 'a eval =
  | Eval_none
  (** Unevaluated *)
  | Eval_progress
  (** Evaluation in progress. It is used to detect and report cycles and
      concurrent invalidations (nodes that changes in the middle of an
      evaluation cycle. *)
  | Eval_some of 'a
  (** Evaluation succeeded and produced a result. *)

(** The [t_] type represents a node of an Lwd graph.
    The graph is made of two parts:
    - a "forward" one, described by types [t_] and [desc], that describes a
      computation
    - a "backward" one, implemented by [trace] and [trace_children], that keep
      track of nodes invalidated by changes to the current node.
*)
type 'a t_ =
  | Pure of 'a
  (** Graph structure is not kept for pure values: they cannot change,
      they cannot invalidate anything. *)
  | Operator : {
      mutable value : 'a eval;
      (* A cache for result of evaluation *)
      mutable trace_children : trace_children;
      mutable trace_parent : trace;
      (* List of parents this to invalidate if this node changes. *)
      desc: 'a desc;
      (* The actual operation, described by [desc] type. *)
    } -> 'a t_
  | Root : {
      mutable value : 'a eval;
      (* A cache for result of evaluation *)
      mutable trace_children : trace_children; (* list of direct children that can invalidate this *)
      mutable on_invalidate : 'a -> unit;
      mutable acquired : bool;
      child : 'a t_;
    } -> 'a t_

(** [desc] is almost directly a free applicative functor / monad. *)
and _ desc =
  | Map  : 'a t_ * ('a -> 'b) -> 'b desc
  | Map2 : 'a t_ * 'b t_ * ('a -> 'b -> 'c) -> 'c desc
  | Pair : 'a t_ * 'b t_ -> ('a * 'b) desc
  | App  : ('a -> 'b) t_ * 'a t_ -> 'b desc
  | Join : { child : 'a t_ t_; mutable intermediate : 'a t_ option } -> 'a desc
  | Var  : { mutable binding : 'a } -> 'a desc
  | Prim : { acquire : 'a t_ -> 'a;
             release : 'a t_ -> 'a -> unit } -> 'a desc

(* a set of (active) parents for a ['a t], used during invalidation *)
and trace =
  | TP0
  | TP1 : _ t_ -> trace
  | TP2 : _ t_ * _ t_ -> trace
  | TP3 : _ t_ * _ t_ * _ t_ -> trace
  | TP4 : _ t_ * _ t_ * _ t_ * _ t_ -> trace
  | TPn : { mutable active : int; mutable count : int;
            mutable entries : any t_ array } -> trace

(* a set of direct children for a composite document *)
and trace_children =
  | TC0
  | TC1 : { mutable idx : int ;
           obj : 'a t_;
           mutable next : trace_children } -> trace_children

(** {1 Traces}

    Traces keep track of nodes that need to be invalidated if

*)

module Trace : sig
  val remove_parent : self:_ t_ -> parent:_ t_ -> trace -> trace
  val add_parent : self:_ t_ -> parent:_ t_ -> trace -> trace
  val is_empty : trace -> bool
  val activate : self: _ t_ -> parent:_ t_ -> trace -> unit
  val invalidate_node : 'a t_ -> unit
  val invalidate : trace -> unit
end = struct

  let is_empty = function TP0 -> true | _ -> false

  (* Management of trace indices *)

  external t_equal : _ t_ -> _ t_ -> bool = "%eq"
  external any_t : 'a t_ -> any t_ = "%identity"

  let add_idx obj idx = function
    | Pure _ -> assert false
    | Root t' ->
      t'.trace_children <- TC1 { idx; obj; next = t'.trace_children }
    | Operator t' ->
      t'.trace_children <- TC1 { idx; obj; next = t'.trace_children }

  let rec rem_idx_rec obj = function
    | TC0 -> assert false
    | TC1 t as self ->
      if t_equal t.obj obj
      then (t.idx, t.next)
      else (
        let idx, result = rem_idx_rec obj t.next in
        t.next <- result;
        (idx, self)
      )

  (* remove [obj] from the lwd's trace. *)
  let rem_idx obj = function
    | Pure _ -> assert false
    | Root t' ->
      let idx, trace_children = rem_idx_rec obj t'.trace_children in
      t'.trace_children <- trace_children; idx
    | Operator t' ->
      let idx, trace_children = rem_idx_rec obj t'.trace_children in
      t'.trace_children <- trace_children; idx

  (* move [obj] from old index to new index. *)
  let rec mov_idx_rec obj oldidx newidx = function
    | TC0 -> assert false
    | TC1 t ->
      if t.idx = oldidx && t_equal t.obj obj
      then t.idx <- newidx
      else mov_idx_rec obj oldidx newidx t.next

  let mov_idx obj oldidx newidx = function
    | Pure _ -> assert false
    | Root t' -> mov_idx_rec obj oldidx newidx t'.trace_children
    | Operator t' -> mov_idx_rec obj oldidx newidx t'.trace_children

  let rec get_idx_rec obj = function
    | TC0 -> assert false
    | TC1 t ->
      if t_equal t.obj obj
      then t.idx
      else get_idx_rec obj t.next

  (* find index of [obj] in the given lwd *)
  let get_idx obj = function
    | Pure _ -> assert false
    | Root t' -> get_idx_rec obj t'.trace_children
    | Operator t' -> get_idx_rec obj t'.trace_children

  (* Propagating invalidation recursively.
     Each document is invalidated at most once,
     and only if it has [t.value = Some _]. *)
  let rec invalidate_node : type a . a t_ -> unit = function
    | Pure _ -> assert false
    | Root ({ value; _ } as t) ->
      t.value <- Eval_none;
      begin match value with
        | Eval_none | Eval_progress -> ()
        | Eval_some x ->
          t.on_invalidate x (* user callback that {i observes} this root. *)
      end
    | Operator { value = Eval_none; _ } -> ()
    | Operator t ->
      t.value <- Eval_none;
      invalidate t.trace_parent; (* invalidate parents recursively *)

      (* invalidate recursively documents in the given trace *)
  and invalidate = function
    | TP0 -> ()
    | TP1 x -> invalidate_node x
    | TP2 (x, y) ->
      invalidate_node x;
      invalidate_node y
    | TP3 (x, y, z) ->
      invalidate_node x;
      invalidate_node y;
      invalidate_node z
    | TP4 (x, y, z, w) ->
      invalidate_node x;
      invalidate_node y;
      invalidate_node z;
      invalidate_node w
    | TPn t ->
      let active = t.active in
      t.active <- 0;
      for i = 0 to active - 1 do
        invalidate_node t.entries.(i)
      done

  let dummy = any_t (Pure ())

  let remove_parent ~self ~parent = function
    | TP0 -> assert false
    | TP1 x -> assert (t_equal x parent); TP0
    | TP2 (x, y) ->
      if t_equal x parent then TP1 y
      else if t_equal y parent then TP1 x
      else assert false
    | TP3 (x, y, z) ->
      if t_equal x parent then TP2 (y, z)
      else if t_equal y parent then TP2 (x, z)
      else if t_equal z parent then TP2 (x, y)
      else assert false
    | TP4 (x, y, z, w) ->
      if t_equal x parent then TP3 (y, z, w)
      else if t_equal y parent then TP3 (x, z, w)
      else if t_equal z parent then TP3 (x, y, w)
      else if t_equal w parent then TP3 (x, y, z)
      else assert false
    | TPn tn as trace ->
      let revidx = rem_idx self parent in
      assert (t_equal tn.entries.(revidx) parent);
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
        (* downgrade to [TP4] to save space *)
        let a = tn.entries.(0) and b = tn.entries.(1) in
        let c = tn.entries.(2) and d = tn.entries.(3) in
        ignore (rem_idx self a : int);
        ignore (rem_idx self b : int);
        ignore (rem_idx self c : int);
        ignore (rem_idx self d : int);
        TP4 (a, b, c, d)
      ) else (
        let len = Array.length tn.entries in
        if count <= len lsr 2 then
          TPn { active = tn.active; count = tn.count;
                entries = Array.sub tn.entries 0 (len lsr 1) }
        else
          trace
      )

  let add_parent ~self ~parent = function
    | TP0 -> TP1 parent
    | TP1 x -> TP2 (parent, x)
    | TP2 (x, y) -> TP3 (parent, x, y)
    | TP3 (x, y, z) -> TP4 (parent, x, y, z)
    | TP4 (x, y, z, w) ->
      let entries =
        [| any_t x      ; any_t y ; any_t z ; any_t w ;
           any_t parent ; dummy   ; dummy   ; dummy   |]
      in
      for i = 0 to 4 do add_idx self i entries.(i) done;
      TPn { active = 5; count = 5; entries }
    | TPn tn as trace ->
      let index = tn.count in
      let entries, trace =
        (* possibly resize array [entries] *)
        if index < Array.length tn.entries then (
          tn.count <- tn.count + 1;
          (tn.entries, trace)
        ) else (
          let entries = Array.make (index * 2) dummy in
          Array.blit tn.entries 0 entries 0 index;
          (entries, TPn { active = tn.active; count = index + 1; entries })
        )
      in
      let parent' = any_t parent in
      entries.(index) <- parent';
      add_idx self index parent';
      trace

  (* make sure that [parent] is in [self.trace_parent], passed as last arg. *)
  let activate ~self ~parent = function
    | TPn tn ->
      let idx = get_idx self parent in
      (* index of [self] in [parent.trace_children] *)
      let active = tn.active in
      (* [idx < active] means [self] is already traced by [parent].
         We only have to add [self] to the entries if [idx >= active]. *)
      if idx >= active then (
        tn.active <- active + 1;
      );
      if idx > active then (
        (* swap with last entry in [tn.entries] *)
        let old = tn.entries.(active) in
        tn.entries.(idx) <- old;
        tn.entries.(active) <- any_t parent;
        mov_idx self active idx old;
        mov_idx self idx active parent
      )
    | _ -> ()
end

type release_list =
  | Release_done
  | Release_more :
      { origin : 'a t_; element : 'b t_; next : release_list } -> release_list

type release_queue = release_list ref
let make_release_queue () = ref Release_done

type release_failure = exn * Printexc.raw_backtrace

module Eval : sig
  val release: release_failure list -> parent:_ t_ -> _ t_ -> release_failure list
  val acquire : parent:_ t_ -> _ t_ -> unit
  val sample : release_queue -> parent:_ t_ -> 'a t_ -> 'a
end = struct
  (* [sub_release [] origin self] is called when [origin] is released,
     where [origin] is reachable from [self]'s trace.
     We're going to remove [origin] from that trace as [origin] is now dead.

     [sub_release] cannot raise.
     If a primitive raises, the exception is caught and a warning is emitted. *)
  let rec release
    : type a b. release_failure list -> parent:a t_ -> b t_ -> release_failure list
    = fun failures ~parent -> function
      | Root _ -> assert false
      | Pure _ -> failures
      | Operator t as self ->
        (* compute [t.trace_parent \ {origin}] *)
        let trace = Trace.remove_parent ~self ~parent t.trace_parent in
        t.trace_parent <- trace;
        match trace with
        | TP0 ->
          (* [self] is not active anymore, since it's not reachable
             from any root. We can release its cached value and
             recursively release its subtree. *)
          let value = t.value in
          t.value <- Eval_progress;
          begin match t.desc with
            | Map  (x, _) -> release failures ~parent:self x
            | Map2 (x, y, _) ->
              release (release failures ~parent:self x) ~parent:self y
            | Pair (x, y) ->
              release (release failures ~parent:self x) ~parent:self y
            | App  (x, y) ->
              release (release failures ~parent:self x) ~parent:self y
            | Join ({ child; intermediate } as t) ->
              let failures = release failures ~parent:self child in
              begin match intermediate with
                | None -> failures
                | Some child' ->
                  t.intermediate <- None;
                  release failures ~parent:self child'
              end
            | Var  _ -> failures
            | Prim t ->
              begin match value with
                | Eval_none  | Eval_progress -> failures
                | Eval_some x ->
                  begin match t.release self x with
                    | () -> failures
                    | exception exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      (exn, bt) :: failures
                  end
              end
          end
        | _ -> failures

  (* [acquire] cannot raise *)
  let rec acquire : type a b . parent:a t_ -> b t_ -> unit = fun ~parent ->
    function
    | Root _ -> assert false
    | Pure _ -> ()
    | Operator t as self ->
      (* [acquire] is true if this is the first time this operator
         is used, in which case we need to acquire its children *)
      let need_acquire = Trace.is_empty t.trace_parent in
      let trace = Trace.add_parent ~self ~parent t.trace_parent in
      t.trace_parent <- trace;
      if need_acquire then (
        (* acquire immediate children, and so on recursively *)
        match t.desc with
        | Map  (x, _) -> acquire ~parent:self x
        | Map2 (x, y, _) ->
          acquire ~parent:self x;
          acquire ~parent:self y
        | Pair (x, y) ->
          acquire ~parent:self x;
          acquire ~parent:self y
        | App  (x, y) ->
          acquire ~parent:self x;
          acquire ~parent:self y
        | Join { child; intermediate } ->
          acquire ~parent:self child;
          begin match intermediate with
            | None -> ()
            | Some _ ->
              assert false (* this can't initialized already, first-time acquire *)
          end
        | Var  _ -> ()
        | Prim _ -> ()
      )

  (* [sample origin self] computes a value for [self].

     [sample] raise if any user-provided computation raises.
     Graph will be left in a coherent state but exception will be propagated
     to the observer. *)
  let sample queue ~parent =
    let rec aux : type a b . a t_ -> b t_ -> b = fun parent ->
      function
      | Root _ -> assert false
      | Pure x -> x
      | Operator t as self ->
        (* try to use cached value, if present *)
        match t.value with
        | Eval_some value ->
          Trace.activate ~self ~parent t.trace_parent;
          value
        | _ ->
          t.value <- Eval_progress;
          let result : b = match t.desc with
            | Map  (x, f) -> f (aux self x)
            | Map2 (x, y, f) -> f (aux self x) (aux self y)
            | Pair (x, y) -> (aux self x, aux self y)
            | App  (f, x) -> (aux self f) (aux self x)
            | Join x ->
              let intermediate =
                (* We haven't touched any state yet,
                   it is safe for [aux] to raise *)
                aux self x.child
              in
              begin match x.intermediate with
                | None ->
                  x.intermediate <- Some intermediate;
                  acquire ~parent:self intermediate;
                | Some x' when x' != intermediate ->
                  queue := Release_more {
                      origin = self;
                      element = x';
                      next = !queue;
                    };
                  x.intermediate <- Some intermediate;
                  acquire ~parent:self intermediate;
                | Some _ -> ()
              end;
              aux self intermediate
            | Var  x -> x.binding
            | Prim t -> t.acquire self
          in
          begin match t.value with
            | Eval_progress -> t.value <- Eval_some result;
            | Eval_none | Eval_some _ -> ()
          end;
          (* [self] just became active, so it may invalidate [origin] in case its
             value changes because of [t.desc], like if it's a variable and gets
             mutated, or if it's a primitive that gets invalidated.
             We need to put [origin] into [self.trace_parent] in case it isn't
             there yet. *)
          Trace.activate ~self ~parent t.trace_parent;
          result
    in
    aux parent

end

(** {1 Public combinators} *)

(* The type system cannot see that t is covariant in its parameter.
   Use the Force to convince it. *)
type +'a t
external inj : 'a t_ -> 'a t = "%identity"
external prj : 'a t -> 'a t_ = "%identity"
external prj2 : 'a t t -> 'a t_ t_ = "%identity"

type ('a, 'b) func = 'a -> 'b
external prj3 : ('a t, 'b) func -> ('a t_, 'b) func = "%identity"

type 'a root = 'a t

let observe ?(on_invalidate=ignore) child : _ root =
  let root = Root {
      child = prj child;
      value = Eval_none;
      on_invalidate;
      trace_children = TC0;
      acquired = false;
    } in
  inj root

let flush_release_queue queue =
  let queue' = !queue in
  queue := Release_done;
  let rec aux failures = function
    | Release_done -> failures
    | Release_more t ->
      let failures = Eval.release failures ~parent:t.origin t.element in
      aux failures t.next
  in
  aux [] queue'

let sample queue x = match prj x with
  | Pure _ | Operator _ -> assert false
  | Root t as self ->
    match t.value with
    | Eval_some value -> value
    | _ ->
      (* no cached value, compute it now *)
      if not t.acquired then (
        t.acquired <- true;
        Eval.acquire ~parent:self t.child;
      );
      t.value <- Eval_progress;
      let value = Eval.sample queue ~parent:self t.child in
      begin match t.value with
        | Eval_progress -> t.value <- Eval_some value; (* cache value *)
        | Eval_none | Eval_some _ -> ()
      end;
      value

let is_damaged x = match prj x with
  | Pure _ | Operator _ -> assert false
  | Root {value = Eval_some _; _} -> false
  | Root {value = Eval_none | Eval_progress; _} -> true

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

exception Release_failure of exn option * release_failure list

let flush_or_fail main_exn queue =
  match flush_release_queue queue with
  | [] -> ()
  | failures -> raise (Release_failure (main_exn, failures))

let quick_sample root =
  let queue = ref Release_done in
  match sample queue root with
  | result -> flush_or_fail None queue; result
  | exception exn -> flush_or_fail (Some exn) queue; raise exn

let quick_release root =
  let queue = ref Release_done in
  release queue root;
  flush_or_fail None queue

(* Basic combinators *)
let return x = inj (Pure x)
let pure x = inj (Pure x)

let is_pure x = match prj x with
  | Pure x -> Some x
  | _ -> None

let operator desc =
  Operator { value = Eval_none; trace_parent = TP0; desc; trace_children = TC0 }

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

(* Variables *)
type 'a var = 'a t_
let var x = operator (Var {binding = x})
let get x = inj x

let set (vx:_ var) x : unit =
  match vx with
  | Operator ({desc = Var v; _}) ->
    (* set the variable, and invalidate all observers *)
    Trace.invalidate_node vx;
    v.binding <- x
  | _ -> assert false

let peek = function
  | Operator ({desc = Var v; _}) -> v.binding
  | _ -> assert false

(* Primitives *)
type 'a prim = 'a t
let prim ~acquire ~release =
  let acquire = prj3 acquire and release = prj3 release in
  inj (operator (Prim { acquire; release }))

let get_prim x = x

let invalidate x = match prj x with
  | Operator ({ desc = Prim p; _ } as t) ->
    let value = t.value in
    t.value <- Eval_none;
    (* the value is invalidated, be sure to invalidate all parents as well *)
    Trace.invalidate t.trace_parent;
    begin match value with
      | Eval_none | Eval_progress -> ()
      | Eval_some v -> p.release (prj x) v
    end
  | _ -> assert false

let dump_trace x =
  let addr oc obj =
    Printf.fprintf oc "0x%08x" (Obj.magic obj : int)
  in
  let rec dump_trace : type a. a t_ -> unit =
    fun obj -> match obj with
      | Pure _ -> Printf.eprintf "%a: Pure _\n%!" addr obj
      | Operator t ->
        Printf.eprintf "%a: Operator _ -> %a\n%!"
          addr obj dump_trace_aux t.trace_parent;
        begin match t.trace_parent with
          | TP0 -> ()
          | TP1 a -> dump_trace a
          | TP2 (a,b) -> dump_trace a; dump_trace b
          | TP3 (a,b,c) -> dump_trace a; dump_trace b; dump_trace c
          | TP4 (a,b,c,d) -> dump_trace a; dump_trace b; dump_trace c; dump_trace d
          | TPn t -> Array.iter dump_trace t.entries
        end
      | Root _ -> Printf.eprintf "%a: Root _\n%!" addr obj
  and dump_trace_aux oc = function
    | TP0 -> Printf.fprintf oc "TP0"
    | TP1 a -> Printf.fprintf oc "TP1 %a" addr a
    | TP2 (a,b) ->
      Printf.fprintf oc "TP2 (%a, %a)" addr a addr b
    | TP3 (a,b,c) ->
      Printf.fprintf oc "TP3 (%a, %a, %a)" addr a addr b addr c
    | TP4 (a,b,c,d) ->
      Printf.fprintf oc "TP4 (%a, %a, %a, %a)" addr a addr b addr c addr d
    | TPn t ->
      Printf.fprintf oc "TPn {active = %d; count = %d; entries = "
        t.active t.count;
      Array.iter (Printf.fprintf oc "(%a)" addr) t.entries;
      Printf.fprintf oc "}"
  in
  dump_trace (prj x)

module Infix = struct
  let (>>=) x f = bind x ~f
  let (>|=) x f = map x ~f
  let (<*>) = app
end

(*$R
  let x = var 0 in
  let y = map succ (get x) in
  let o_y = Lwd.observe y in
  assert_equal 1 (quick_sample o_y);
  set x 10;
  assert_equal 11 (quick_sample o_y);
*)
