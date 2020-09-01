type +'a t =
  | Nil
  | Leaf of { mutable mark: int; v: 'a; }
  | Join of { mutable mark: int; l: 'a t; r: 'a t; }

type 'a seq = 'a t

let empty = Nil

let element v = Leaf { mark = 0; v }

let mask_bits = 2
let old_mask = 1
let new_mask = 2
let both_mask = 3

let rank = function
  | Nil | Leaf _ -> 0
  | Join t -> t.mark lsr mask_bits

let concat a b = match a, b with
  | Nil, x | x, Nil -> x
  | l, r -> Join { mark = (max (rank l) (rank r) + 1) lsl mask_bits; l; r }

type ('a, 'b) view =
  | Empty
  | Element of 'a
  | Concat of 'b * 'b

let view = function
  | Nil    -> Empty
  | Leaf t -> Element t.v
  | Join t -> Concat (t.l, t.r)

module Balanced : sig
  type 'a t = private 'a seq
  val empty : 'a t
  val element : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t

  val view : 'a t -> ('a, 'a t) view
end = struct
  type 'a t = 'a seq

  let empty = empty
  let element = element

  let check l r = abs (l - r) <= 1

  let rec node_left l r =
    let ml = rank l in
    let mr = rank r in
    if check ml mr then concat l r else match l with
      | Nil | Leaf _ -> assert false
      | Join t ->
        if check (rank t.l) ml
        then concat t.l (node_left t.r r)
        else match t.r with
          | Nil | Leaf _ -> assert false
          | Join tr ->
            let trr = node_left tr.r r in
            if check (1 + max (rank t.l) (rank tr.l)) (rank trr)
            then concat (concat t.l tr.l) trr
            else concat t.l (concat tr.l trr)

  let rec node_right l r =
    let ml = rank l in
    let mr = rank r in
    if check mr ml then concat l r else match r with
      | Nil | Leaf _ -> assert false
      | Join t ->
        if check (rank t.r) mr
        then concat (node_right l t.l) t.r
        else match t.l with
          | Nil | Leaf _ -> assert false
          | Join tl ->
            let tll = node_right l tl.l in
            if check (1 + max (rank tl.r) (rank t.r)) (rank tll)
            then concat tll (concat tl.r t.r)
            else concat (concat tll tl.r) t.r

  let concat l r =
    let ml = rank l in
    let mr = rank r in
    if check ml mr
    then concat l r
    else if ml <= mr
    then node_right l r
    else node_left l r

  let view = view
end

module Reducer = struct
  type (+'a, 'b) xform =
    | XEmpty
    | XLeaf of { a: 'a t; mutable b: 'b option; }
    | XJoin of { a: 'a t; mutable b: 'b option;
                 l: ('a, 'b) xform; r: ('a, 'b) xform; }

  type stats = {
    mutable marked: int;
    mutable shared: int;
    mutable blocked: int;
  }
  let mk_stats () = { marked = 0; shared = 0; blocked = 0 }

  let new_marked stats = stats.marked <- stats.marked + 1
  let new_shared stats = stats.shared <- stats.shared + 1
  let new_blocked stats = stats.blocked <- stats.blocked + 1

  let rec block stats = function
    | Nil -> ()
    | Leaf t' ->
      let mark = t'.mark in
      if mark land both_mask <> both_mask && mark land both_mask <> 0
      then (
        new_blocked stats;
        t'.mark <- mark lor both_mask
      )
    | Join t' ->
      let mark = t'.mark in
      if mark land both_mask <> both_mask && mark land both_mask <> 0
      then (
        new_blocked stats;
        t'.mark <- mark lor both_mask;
        block stats t'.l;
        block stats t'.r;
      )

  let enqueue stats q mask = function
    | Nil -> ()
    | Leaf t' ->
      let mark = t'.mark in
      if mark land mask = 0 then (
        (* Not yet seen *)
        new_marked stats;
        if mark land both_mask <> 0 then (
          (* Newly shared, clear mask *)
          t'.mark <- -1;
          new_blocked stats;
          new_shared stats;
        ) else
          t'.mark <- mark lor mask;
      );
      if mark <> -1 && mark land both_mask = both_mask then (
        t'.mark <- -1;
        new_shared stats
      )
    | Join t' as t ->
      let mark = t'.mark in
      if mark land mask = 0 then (
        (* Not yet seen *)
        new_marked stats;
        if mark land both_mask <> 0 then (
          (* Newly shared, clear mask *)
          t'.mark <- -1;
          new_blocked stats;
          new_shared stats;
          block stats t'.l;
          block stats t'.r;
        ) else (
          (* First mark *)
          t'.mark <- mark lor mask;
          Queue.push t q
        )
      );
      if mark <> -1 && mark land both_mask = both_mask then (
        t'.mark <- -1;
        new_shared stats
      )

  let dequeue stats q mask =
    match Queue.pop q with
    | Join t ->
      if t.mark land both_mask = mask then (
        enqueue stats q mask t.l;
        enqueue stats q mask t.r;
      )
    | _ -> assert false

  let traverse1 stats q mask =
    while not (Queue.is_empty q) do
      dequeue stats q mask
    done

  let rec traverse sold snew qold qnew =
    if Queue.is_empty qold then
      traverse1 snew qnew new_mask
    else if Queue.is_empty qnew then
      traverse1 sold qold old_mask
    else (
      dequeue sold qold old_mask;
      dequeue snew qnew new_mask;
      traverse sold snew qold qnew
    )

  type ('a, 'b) unmark_state = {
    dropped : 'b option array;
    mutable dropped_leaf : int;
    mutable dropped_join : int;
    shared : 'a seq array;
    shared_x : ('a, 'b) xform list array;
    mutable shared_index: int;
  }

  let next_shared_index st =
    let result = st.shared_index in
    st.shared_index <- result + 1;
    result

  let rec unblock = function
    | XEmpty -> ()
    | XLeaf {a = Nil | Join _; _} -> assert false
    | XJoin {a = Nil | Leaf _; _} -> assert false
    | XLeaf {a = Leaf t'; _} ->
      let mark = t'.mark in
      if mark <> -1 && mark land both_mask = both_mask then
        t'.mark <- mark land lnot both_mask;
    | XJoin {a = Join t'; l; r; _} ->
      let mark = t'.mark in
      if mark <> -1 && mark land both_mask = both_mask then (
        t'.mark <- mark land lnot both_mask;
        unblock l;
        unblock r
      )

  let rec unmark_old st = function
    | XEmpty -> ()
    | XLeaf {a = Nil | Join _; _} -> assert false
    | XJoin {a = Nil | Leaf _; _} -> assert false
    | XLeaf {a = Leaf t' as a; b} as t ->
      let mark = t'.mark in
      if mark land both_mask = old_mask then (
        let dropped_leaf = st.dropped_leaf in
        if dropped_leaf > -1 then (
          st.dropped.(dropped_leaf) <- b;
          st.dropped_leaf <- dropped_leaf + 1;
        );
        t'.mark <- mark land lnot both_mask
      ) else if mark = -1 then (
        let index = next_shared_index st in
        st.shared.(index) <- a;
        st.shared_x.(index) <- [t];
        t'.mark <- (index lsl mask_bits) lor new_mask;
      ) else if mark land both_mask = new_mask then (
        let index = mark lsr mask_bits in
        st.shared_x.(index) <- t :: st.shared_x.(index);
      ) else if mark land both_mask = both_mask then (
        assert false
        (*t'.mark <- mark land lnot both_mask*)
      )
    | XJoin {a = Join t' as a; l; r; b} as t ->
      let mark = t'.mark in
      if mark land both_mask = old_mask then (
        let dropped_leaf = st.dropped_leaf in
        if dropped_leaf > -1 then (
          st.dropped.(dropped_leaf) <- b;
          st.dropped_leaf <- dropped_leaf + 1;
        );
        t'.mark <- mark land lnot both_mask;
        unmark_old st l;
        unmark_old st r;
      ) else if mark = -1 then (
        let index = next_shared_index st in
        st.shared.(index) <- a;
        st.shared_x.(index) <- [t];
        t'.mark <- (index lsl mask_bits) lor new_mask;
        unblock l;
        unblock r;
      ) else if mark land both_mask = new_mask then (
        let index = mark lsr mask_bits in
        st.shared_x.(index) <- t :: st.shared_x.(index);
      ) else if mark land both_mask = both_mask then (
        assert false
      )

  let prepare_shared st =
    for i = 0 to st.shared_index - 1 do
      begin match st.shared.(i) with
        | Nil -> ()
        | Leaf t -> t.mark <- t.mark lor both_mask
        | Join t -> t.mark <- t.mark lor both_mask
      end;
      match st.shared_x.(i) with
      | [] -> assert false
      | [_] -> ()
      | xs -> st.shared_x.(i) <- List.rev xs
    done

  let rec unmark_new st = function
    | Nil -> XEmpty
    | Leaf t' as t ->
      let mark = t'.mark in
      if mark <> -1 && mark land both_mask = both_mask then (
        let index = mark lsr mask_bits in
        match st.shared_x.(index) with
        | [] -> XLeaf {a = t; b = None}
        | x :: xs -> st.shared_x.(index) <- xs; x
      ) else (
        t'.mark <- 0;
        XLeaf {a = t; b = None}
      )
    | Join t' as t ->
      let mark = t'.mark in
      if mark = -1 then (
        let index = next_shared_index st in
        t'.mark <- 0;
        st.shared.(index) <- t;
        let l = unmark_new st t'.l in
        let r = unmark_new st t'.r in
        XJoin {a = t; b = None; l; r}
      ) else if mark land both_mask = both_mask then (
        let index = mark lsr mask_bits in
        match st.shared_x.(index) with
        | [] -> assert false
        | x :: xs ->
          st.shared_x.(index) <- xs;
          if xs == [] then t'.mark <- 0;
          x
      ) else (
        t'.mark <- t'.mark land lnot both_mask;
        let l = unmark_new st t'.l in
        let r = unmark_new st t'.r in
        XJoin {a = t; b = None; l; r}
      )

  type 'b dropped = {
    leaves: int;
    table: 'b option array;
    extra_leaf: 'b list;
    extra_join: 'b list;
  }

  let no_dropped =
    { leaves = 0; table = [||]; extra_leaf = []; extra_join = [] }

  let diff get_dropped xold tnew = match xold, tnew with
    | XEmpty, Nil -> no_dropped, XEmpty
    | (XLeaf {a; _} | XJoin {a; _}), _ when a == tnew -> no_dropped, xold
    | _ ->
      (* Cost: 16 words *)
      let qold = Queue.create () and sold = mk_stats () in
      let qnew = Queue.create () and snew = mk_stats () in
      begin match xold with
        | XEmpty -> ()
        | (XLeaf {a; _} | XJoin {a; _}) ->
          enqueue sold qold old_mask a
      end;
      enqueue snew qnew new_mask tnew;
      traverse sold snew qold qnew;
      let nb_dropped = sold.marked - (sold.blocked + snew.blocked) in
      let st = {
        dropped = if get_dropped then Array.make nb_dropped None else [||];
        dropped_leaf = if get_dropped then 0 else -1;
        dropped_join = if get_dropped then nb_dropped else -1;
        shared = Array.make (sold.shared + snew.shared) Nil;
        shared_x = Array.make (sold.shared + snew.shared) [];
        shared_index = 0;
      } in
      (*Printf.eprintf "sold.shared:%d sold.marked:%d sold.blocked:%d\n%!"
        sold.shared sold.marked sold.blocked;
      Printf.eprintf "snew.shared:%d snew.marked:%d snew.blocked:%d\n%!"
        snew.shared snew.marked snew.blocked;*)
      unmark_old st xold;
      assert (st.dropped_leaf = st.dropped_join);
      prepare_shared st;
      let result = unmark_new st tnew in
      (*Printf.eprintf "new_computed:%d%!\n" !new_computed;*)
      let restore_rank = function
        | Nil -> assert false
        | Leaf t -> t.mark <- 0
        | Join t ->
          t.mark <- (max (rank t.l) (rank t.r) + 1) lsl mask_bits
      in
      for i = st.shared_index - 1 downto 0 do
        restore_rank st.shared.(i)
      done;
      if get_dropped then (
        let xleaf = ref [] in
        let xjoin = ref [] in
        for i = 0 to st.shared_index - 1 do
          List.iter (function
              | XLeaf { b = Some b; _} -> xleaf := b :: !xleaf
              | XJoin { b = Some b; _} -> xjoin := b :: !xjoin
              | _ -> ()
            ) st.shared_x.(i)
        done;
        ({ leaves = st.dropped_leaf;
           table = st.dropped;
           extra_leaf = !xleaf;
           extra_join = !xjoin }, result)
      ) else
        no_dropped, result

  type ('a, 'b) map_reduce = ('a -> 'b) * ('b -> 'b -> 'b)
  let map (f, _) x = f x
  let reduce (_, f) x y = f x y

  let eval map_reduce = function
    | XEmpty -> None
    | other ->
      let rec aux = function
        | XEmpty | XLeaf {a = Nil | Join _; _} -> assert false
        | XLeaf {b = Some b; _} | XJoin {b = Some b; _} -> b
        | XLeaf ({a = Leaf t';_ } as t) ->
          let result = map map_reduce t'.v in
          t.b <- Some result;
          result
        | XJoin t ->
          let l = aux t.l and r = aux t.r in
          let result = reduce map_reduce l r in
          t.b <- Some result;
          result
      in
      Some (aux other)

  type ('a, 'b) reducer = ('a, 'b) map_reduce * ('a, 'b) xform

  let make ~map ~reduce = ((map, reduce), XEmpty)

  let reduce (map_reduce, tree : _ reducer) =
    eval map_reduce tree

  let update (map_reduce, old_tree : _ reducer) new_tree : _ reducer =
    let _, tree = diff false old_tree new_tree in
    (map_reduce, tree)

  let update_and_get_dropped (map_reduce, old_tree : _ reducer) new_tree
    : _ dropped * _ reducer =
    let dropped, tree = diff true old_tree new_tree in
    (dropped, (map_reduce, tree))

  let fold_dropped kind f dropped acc =
    let acc = ref acc in
    let start, bound = match kind with
      | `All    -> 0, Array.length dropped.table
      | `Map    -> 0, dropped.leaves
      | `Reduce -> dropped.leaves, Array.length dropped.table
    in
    for i = start to bound - 1 do
      match dropped.table.(i) with
      | None -> ()
      | Some x -> acc := f x !acc
    done;
    !acc
end

(* Lwd interface *)

let fold ~map ~reduce seq =
  let reducer = ref (Reducer.make ~map ~reduce) in
  Lwd.map' seq @@ fun seq ->
  let reducer' = Reducer.update !reducer seq in
  reducer := reducer';
  Reducer.reduce reducer'

let fold_monoid map (zero, reduce) seq =
  let reducer = ref (Reducer.make ~map ~reduce) in
  Lwd.map' seq @@ fun seq ->
  let reducer' = Reducer.update !reducer seq in
  reducer := reducer';
  match Reducer.reduce reducer' with
  | None -> zero
  | Some x -> x

let monoid = (empty, concat)

let lwd_empty : 'a t Lwd.t = Lwd.pure Nil
let lwd_monoid : 'a. 'a t Lwd.t Lwd_utils.monoid =
  (lwd_empty, fun x y -> Lwd.map2 concat x y)

let map f seq =
  fold_monoid (fun x -> element (f x)) monoid seq

let filter f seq =
  fold_monoid (fun x -> if f x then element x else empty) monoid seq

let filter_map f seq =
  let select x = match f x with
    | Some y -> element y
    | None -> empty
  in
  fold_monoid select monoid seq

let lift (seq : 'a Lwd.t seq Lwd.t) : 'a seq Lwd.t =
  Lwd.join (fold_monoid (Lwd.map element) (Lwd_utils.lift_monoid monoid) seq)
