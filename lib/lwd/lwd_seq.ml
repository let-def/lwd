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
  }
  let mk_stats () = { marked = 0; shared = 0 }

  let rec discard stats mask = function
    | Nil -> ()
    | Leaf t ->
      let mark = t.mark in
      if mark land both_mask <> 0 && mark land mask = 0 then (
        stats.marked <- stats.marked + 1;
        stats.shared <- stats.shared + 1;
        t.mark <- mark lor mask;
      )
    | Join t ->
      let mark = t.mark in
      if mark land both_mask <> 0 && mark land mask = 0 then (
        stats.marked <- stats.marked + 1;
        stats.shared <- stats.shared + 1;
        t.mark <- mark lor mask;
        discard stats mask t.l;
        discard stats mask t.r;
      )

  let enqueue stats q mask = function
    | Nil -> ()
    | Leaf t ->
      let mark = t.mark in
      stats.marked <- stats.marked + 1;
      if mark land mask = 0 then (
        t.mark <- mark lor mask;
        if mark land both_mask <> 0 then
          stats.shared <- stats.shared + 1;
      )
    | Join t as node ->
      let mark = t.mark in
      if mark land mask = 0 then (
        stats.marked <- stats.marked + 1;
        t.mark <- mark lor mask;
        if mark land both_mask = 0 then (
          Queue.push node q
        ) else (
          stats.shared <- stats.shared + 1;
          discard stats mask t.l;
          discard stats mask t.r;
        )
      )

  let dequeue stats q mask =
    match Queue.pop q with
    | Join t ->
      if t.mark land mask <> 0 then (
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
    shared : ('a, 'b) xform array;
    mutable shared_index: int;
  }

  let rec unmark_old st = function
    | XEmpty -> ()
    | XLeaf {a = Nil | Join _; _} -> assert false
    | XJoin {a = Nil | Leaf _; _} -> assert false
    | XLeaf {a = Leaf t'; b}  as t ->
      let mark = t'.mark land both_mask in
      if mark = old_mask then (
        let dropped_leaf = st.dropped_leaf in
        if dropped_leaf > -1 then (
          st.dropped.(dropped_leaf) <- b;
          st.dropped_leaf <- dropped_leaf + 1;
        );
        t'.mark <- mark land lnot both_mask
      ) else if mark = both_mask then (
        let shared_index = st.shared_index in
        st.shared.(shared_index) <- t;
        st.shared_index <- shared_index + 1;
        t'.mark <- shared_index lsl mask_bits;
      )
    | XJoin {a = Join t'; l; r; b} as t ->
      let mark = t'.mark land both_mask in
      if mark <> 0 then (
        if mark = old_mask then (
          let dropped_join = st.dropped_join - 1 in
          if dropped_join > -1 then (
            st.dropped.(dropped_join) <- b;
            st.dropped_join <- dropped_join;
          );
          t'.mark <- mark land lnot both_mask
        )
        else if mark = both_mask then (
          let shared_index = st.shared_index in
          st.shared.(shared_index) <- t;
          st.shared_index <- shared_index + 1;
          t'.mark <- shared_index lsl mask_bits;
        );
        unmark_old st l;
        unmark_old st r;
      )

  let rec unmark_new st = function
    | Nil -> XEmpty
    | Leaf t' as t ->
      let mark = t'.mark in
      if mark land both_mask = new_mask then (
        let shared_index = st.shared_index in
        let x = XLeaf {a = t; b = None} in
        st.shared.(shared_index) <- x;
        st.shared_index <- shared_index + 1;
        t'.mark <- shared_index lsl mask_bits;
        x
      ) else (
        assert (mark land both_mask = 0);
        st.shared.(mark lsr mask_bits)
      )
    | Join t' as t ->
      let mark = t'.mark in
      if mark land both_mask = new_mask then (
        let shared_index = st.shared_index in
        st.shared_index <- shared_index + 1;
        let l = unmark_new st t'.l in
        let r = unmark_new st t'.r in
        let x = XJoin {a = t; b = None; l; r} in
        st.shared.(shared_index) <- x;
        t'.mark <- shared_index lsl mask_bits;
        x
      ) else (
        assert (mark land both_mask = 0);
        st.shared.(mark lsr mask_bits)
      )

  let rec check_ranks = function
    | Nil    -> 0
    | Leaf t -> assert (t.mark = 0); 0
    | Join t ->
      let l = check_ranks t.l in
      let r = check_ranks t.r in
      let rank = max l r + 1 in
      assert (t.mark = rank lsl mask_bits);
      rank

  let diff get_dropped xold tnew = match xold, tnew with
    | XEmpty, Nil -> 0, [||], XEmpty
    | (XLeaf {a; _} | XJoin {a; _}), _ when a == tnew -> 0, [||], xold
    | _ ->
      let qold = Queue.create () in
      let sold = mk_stats () in
      let qnew = Queue.create () in
      let snew = mk_stats () in
      begin match xold with
        | XEmpty -> ()
        | (XLeaf {a; _} | XJoin {a; _}) ->
          enqueue sold qold old_mask a
      end;
      enqueue snew qnew new_mask tnew;
      traverse sold snew qold qnew;
      let nb_dropped = sold.marked - (sold.shared + snew.shared) in
      let st = {
        dropped = if get_dropped then Array.make nb_dropped None else [||];
        dropped_leaf = if get_dropped then 0 else -1;
        dropped_join = if get_dropped then nb_dropped else -1;
        shared = Array.make snew.marked XEmpty;
        shared_index = 0;
      } in
      unmark_old st xold;
      assert (st.dropped_leaf = st.dropped_join);
      let result = unmark_new st tnew in
      for i = st.shared_index - 1 downto 0 do
        match st.shared.(i) with
        | XEmpty
        | XLeaf {a = Nil | Join _; _}
        | XJoin {a = Nil | Leaf _; _} -> assert false
        | XLeaf {a = Leaf t; _} -> t.mark <- 0
        | XJoin {a = Join t; _} ->
            t.mark <- (max (rank t.l) (rank t.r) + 1) lsl mask_bits
      done;
      ignore (check_ranks tnew);
      st.dropped_leaf, st.dropped, result

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
    let _leaves, _dropped, tree = diff false old_tree new_tree in
    (map_reduce, tree)

  type 'b dropped = {
    leaves: int;
    table: 'b option array;
  }

  let update_and_get_dropped (map_reduce, old_tree : _ reducer) new_tree
    : _ dropped * _ reducer =
    let leaves, table, tree = diff true old_tree new_tree in
    { leaves; table }, (map_reduce, tree)

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
