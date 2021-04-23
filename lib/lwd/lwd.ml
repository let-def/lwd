type t_node = private T_node [@@warning "-37"]
type t_root = private T_root [@@warning "-37"]

let clock = ref 1

let evaluating = max_int - 1

type ('a, _) t__ =
  | Pure : 'a -> ('a, t_node) t__
  | Operator : {
      mutable value : 'a; (* last known value *)
      mutable validity : int;
      mutable trace : trace; (* list of parents this can invalidate *)
      mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
      desc: 'a desc;
    } -> ('a, t_node) t__
  | Root : {
      mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
      mutable on_invalidate : unit -> unit;
      mutable acquired : bool;
      child : 'a t_;
    } -> ('a, t_root) t__

and 'a t_ = ('a, t_node) t__

and _ desc =
  | Map  : 'a t_ * ('a -> 'b) -> 'b desc
  | Map2 : 'a t_ * 'b t_ * ('a -> 'b -> 'c) -> 'c desc
  | Pair : 'a t_ * 'b t_ -> ('a * 'b) desc
  | App  : ('a -> 'b) t_ * 'a t_ -> 'b desc
  | Join : { child : 'a t_ t_;
             mutable intermediate : 'a t_;
             mutable acquired : bool;
           } -> 'a desc
  | Var  : 'a desc
  | Prim : { acquire : 'a t -> 'a -> 'a;
             invalidate : 'a t -> 'a -> 'a;
             release : 'a t -> 'a -> 'a } -> 'a desc

(* a set of (active) parents for a ['a t], used during invalidation *)
and trace =
  | T0
  | T1 : _ t__ -> trace
  | T2 : _ t__ * _ t__ -> trace
  | T3 : _ t__ * _ t__ * _ t__ -> trace
  | T4 : _ t__ * _ t__ * _ t__ * _ t__ -> trace
  | Tn : { mutable active : int; mutable count : int;
           mutable entries : (Obj.t, Obj.t) t__ array } -> trace

(* a set of direct children for a composite document *)
and trace_idx =
  | I0
  | I1 : { mutable idx : int ;
           obj : 'a t_;
           mutable next : trace_idx } -> trace_idx

(* The type system cannot see that t is covariant in its parameter.
   Use the Force to convince it. *)
and +'a t
external inj : 'a t_ -> 'a t = "%identity"
external prj : 'a t -> 'a t_ = "%identity"
external prj2 : 'a t t -> 'a t_ t_ = "%identity"
external t_equal : _ t__ -> _ t__ -> bool = "%eq"
external obj_t : _ t__ -> (Obj.t, Obj.t) t__ = "%identity"

(* Basic combinators *)
let return x = inj (Pure x)
let pure x = inj (Pure x)

let is_pure x = match prj x with
  | Pure x -> Some x
  | _ -> None

let dummy = obj_t (Pure ())

let join_validity a b : int =
  if a < b then b else a

let operator validity value desc =
  Operator { value; validity; trace = T0; desc; trace_idx = I0 }

let unpack = function
  | Pure x -> 0, x
  | Operator ox -> ox.validity, ox.value

let map x ~f = inj (
    match prj x with
    | Pure vx -> Pure (f vx)
    | Operator ox as x ->
      operator ox.validity (f ox.value) (Map (x, f))
  )

let map2 x y ~f = inj (
    match prj x, prj y with
    | Pure vx, Pure vy -> Pure (f vx vy)
    | x, y ->
      let bx, vx = unpack x in
      let by, vy = unpack y in
      operator (join_validity bx by) (f vx vy) (Map2 (x, y, f))
  )

let pair x y = inj (
    match prj x, prj y with
    | Pure vx, Pure vy -> Pure (vx, vy)
    | x, y ->
      let bx, vx = unpack x in
      let by, vy = unpack y in
      operator (join_validity bx by) (vx, vy) (Pair (x, y))
  )

let app f x = inj (
    match prj f, prj x with
    | Pure vf, Pure vx -> Pure (vf vx)
    | f, x ->
      let bf, vf = unpack f in
      let bx, vx = unpack x in
      operator (join_validity bf bx) (vf vx) (App (f, x))
  )

let join child = inj (
    match prj2 child with
    | Pure v -> v
    | child ->
      let bc, vc = unpack child in
      let bcc, vcc = unpack vc in
      operator (join_validity bc bcc) vcc
        (Join { child; intermediate = vc; acquired = false })
  )

let bind x ~f = join (map ~f x)

(* Management of trace indices *)

let addr oc obj =
  Printf.fprintf oc "0x%08x" (Obj.magic obj : int)

let rec dump_trace : type a b. (a, b) t__ -> unit =
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

let add_idx (type a) obj idx : (_, a) t__ -> unit = function
  | Pure _ -> assert false
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
let rem_idx (type a) obj : (_, a) t__ -> int = function
  | Pure _ -> assert false
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

let mov_idx (type a) obj oldidx newidx : (_, a) t__ -> unit = function
  | Pure _ -> assert false
  | Root t' -> mov_idx_rec obj oldidx newidx t'.trace_idx
  | Operator t' -> mov_idx_rec obj oldidx newidx t'.trace_idx

let rec get_idx_rec obj = function
  | I0 -> assert false
  | I1 t ->
    if t_equal t.obj obj
    then t.idx
    else get_idx_rec obj t.next

(* find index of [obj] in the given lwd *)
let get_idx (type a) obj : (_, a) t__ -> int = function
  | Pure _ -> assert false
  | Root t' -> get_idx_rec obj t'.trace_idx
  | Operator t' -> get_idx_rec obj t'.trace_idx

(* Propagating invalidation recursively.
   Each document is invalidated at most once,
   and only if it has [t.value = Some _]. *)
let rec invalidate_node : type a b. (a, b) t__ -> unit = function
  | Pure _ -> assert false
  | Root t ->
    t.on_invalidate () (* user callback that {i observes} this root. *)
  | Operator t ->
    if t.validity <> max_int then (
      t.validity <- max_int;
      invalidate_trace t.trace; (* invalidate parents recursively *)
    )

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
type 'a var = 'a t_
let var x = operator 0 x Var
let get x = inj x

let set (vx : _ var) x : unit =
  match vx with
  | Operator ({desc = Var; _} as op) ->
    (* set the variable, and invalidate all observers *)
    op.value <- x;
    invalidate_node vx;
  | _ -> assert false

let peek = function
  | Pure x -> x
  | Operator op -> op.value

let peek_any x = peek (prj x)

(* Primitives *)

type 'a prim = 'a t

let prim ~acquire ~release ~invalidate value =
  inj (operator 0 value (Prim { acquire; release; invalidate }))

let get_prim x = x

let invalidate x = match prj x with
  | Operator ({ desc = Prim p; _ } as t) ->
    (* the value is invalidated, be sure to invalidate all parents as well *)
    invalidate_trace t.trace;
    t.value <- p.invalidate x t.value;
    t.validity <- max_int;
  | _ -> assert false

type release_list =
  | Release_done
  | Release_more :
      { origin : _ t__; element : _ t_; next : release_list } -> release_list

type release_queue = release_list ref
let make_release_queue () = ref Release_done

type release_failure = exn * Printexc.raw_backtrace

(* [sub_release [] origin self] is called when [origin] is released,
   where [origin] is reachable from [self]'s trace.
   We're going to remove [origin] from that trace as [origin] is now dead.

   [sub_release] cannot raise.
   If a primitive raises, the exception is caught and a warning is emitted. *)
let rec sub_release
  : type a a' b . release_failure list -> (a, a') t__ -> b t_ -> release_failure list
  = fun failures origin -> function
    | Pure _ -> failures
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
          else if t_equal w origin then T3 (x, y, z)
          else assert false
        | Tn tn as trace ->
          let revidx = rem_idx self origin in
          assert (t_equal tn.entries.(revidx) origin);
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
        begin match t.desc with
          | Map  (x, _) -> sub_release failures self x
          | Map2 (x, y, _) ->
            sub_release (sub_release failures self x) self y
          | Pair (x, y) ->
            sub_release (sub_release failures self x) self y
          | App  (x, y) ->
            sub_release (sub_release failures self x) self y
          | Join t ->
            let failures = sub_release failures self t.child in
            if t.acquired then (
              t.acquired <- false;
              sub_release failures self t.intermediate
            ) else failures
          | Var -> failures
          | Prim p ->
            begin match p.release (inj self) t.value with
              | value' ->
                t.value <- value';
                failures
              | exception exn ->
                let bt = Printexc.get_raw_backtrace () in
                (exn, bt) :: failures
            end
        end
      | _ -> failures

(* make sure that [origin] is in [self.trace], passed as last arg. *)
let activate_tracing (self : _ t_) (origin : _ t__) = function
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

(* [sub_acquire] cannot raise *)
let rec sub_acquire : type a a' b . (a, a') t__ -> b t_ -> int =
  fun origin ->
  function
  | Pure _ -> 0
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
        activate_tracing self origin trace;
        trace
    in
    t.trace <- trace;
    if acquire then (
      (* acquire immediate children, and so on recursively *)
      let validity' =
        match t.desc with
        | Map  (x, _) ->
          sub_acquire self x
        | Map2 (x, y, _) ->
          join_validity
            (sub_acquire self x)
            (sub_acquire self y)
        | Pair (x, y) ->
          join_validity
            (sub_acquire self x)
            (sub_acquire self y)
        | App  (x, y) ->
          join_validity
            (sub_acquire self x)
            (sub_acquire self y)
        | Join x ->
          assert (not x.acquired);
          let v = sub_acquire self x.child in
          if v < max_int then (
            x.acquired <- true;
            join_validity v (sub_acquire self x.intermediate)
          ) else
            max_int
        | Var -> t.validity
        | Prim p ->
          t.value <- p.acquire (inj self) t.value;
          t.validity
      in
      if validity' > t.validity then
        t.validity <- max_int
    );
    t.validity

(* [sub_sample origin self] computes a value for [self].

   [sub_sample] raise if any user-provided computation raises.
   Graph will be left in a coherent state but exception will be propagated
   to the observer. *)
let sub_sample queue =
  let rec aux : type a a' b . (a, a') t__ -> b t_ -> b = fun origin ->
    function
    | Pure x -> x
    | Operator t as self ->
      (* try to use cached value, if present *)
      if t.validity < evaluating then (
        activate_tracing self origin t.trace;
        t.value
      ) else (
        t.validity <- evaluating;
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
            if intermediate != x.intermediate then (
              if x.acquired then (
                queue := Release_more {
                    origin = self;
                    element = x.intermediate;
                    next = !queue;
                  };
                x.acquired <- false;
              );
              x.intermediate <- intermediate
            );
            if not x.acquired then (
              x.acquired <- true;
              ignore (sub_acquire self intermediate : int)
            );
            aux self intermediate
          | Var -> t.value
          | Prim _ -> t.value
        in
        if t.validity = evaluating then (
          t.value <- result;
          t.validity <- !clock;
        );
        (* [self] just became active, so it may invalidate [origin] in case its
           value changes because of [t.desc], like if it's a variable and gets
           mutated, or if it's a primitive that gets invalidated.
           We need to put [origin] into [self.trace] in case it isn't there yet. *)
        activate_tracing self origin t.trace;
        result
      )
  in
  aux

type +'a root
external inj_root : ('a, t_root) t__ -> 'a root = "%identity"
external prj_root : 'a root -> ('a, t_root) t__ = "%identity"

let observe ?(on_invalidate=ignore) child : _ root =
  let child = prj child in
  inj_root (Root {
    child; on_invalidate;
    trace_idx = I0;
    acquired = false;
  })

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

let sample queue x =
  let Root t as self = prj_root x in
  (* no cached value, compute it now *)
  if not t.acquired then (
    t.acquired <- true;
    ignore (sub_acquire self t.child : int);
  );
  let validity, _ = unpack t.child in
  if validity = max_int then incr clock;
  sub_sample queue self t.child

let is_damaged x =
  let Root {child; _} = prj_root x in
  let validity, _ = unpack child in
  validity = max_int

let release queue x =
  let Root t as self = prj_root x in
  if t.acquired then (
    (* release subtree *)
    t.acquired <- false;
    queue := Release_more { origin = self; element = t.child; next = !queue }
  )

let set_on_invalidate x f =
  let Root t = prj_root x in
  t.on_invalidate <- f

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
