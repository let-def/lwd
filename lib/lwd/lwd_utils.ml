
type 'a monoid = 'a * ('a -> 'a -> 'a)

let lift_monoid (zero, plus) =
  (Lwd.return zero, Lwd.map2 plus)

let pure_pack (zero, plus) items =
  let rec cons_monoid c xs v =
    match xs with
    | (c', v') :: xs when c = c' ->
      cons_monoid (c + 1) xs (plus v' v)
    | xs -> (c, v) :: xs
  in
  match List.fold_left (cons_monoid 0) [] items with
  | [] -> zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> plus v acc) x xs

let rec cons_lwd_monoid plus c xs v =
  match xs with
  | (c', v') :: xs when c = c' ->
    cons_lwd_monoid plus (c + 1) xs (Lwd.map2 plus v' v)
  | xs -> (c, v) :: xs

let pack (zero, plus) items =
  match List.fold_left (cons_lwd_monoid plus 0) [] items with
  | [] -> Lwd.return zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> Lwd.map2 plus v acc) x xs

let pack_seq (zero, plus) items =
  match Seq.fold_left (cons_lwd_monoid plus 0) [] items with
  | [] -> Lwd.return zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> Lwd.map2 plus v acc) x xs

let local_state f =
  let r = ref None in
  let acquire () = match !r with
    | None -> invalid_arg "Lwd_utils.trace: cyclic evaluation"
    | Some v -> v
  in
  let prim = Lwd.prim ~acquire ~release:ignore in
  let update v =
    r := Some v;
    Lwd.invalidate prim
  in
  let v, result = f (Lwd.get_prim prim) update in
  r := Some v;
  result

let rec map_l (f:'a -> 'b Lwd.t) (l:'a list) : 'b list Lwd.t =
  match l with
  | [] -> Lwd.return []
  | x :: tl -> Lwd.map2 List.cons (f x) (map_l f tl)

let flatten_l (l:'a Lwd.t list) : 'a list Lwd.t =
  map_l (fun x->x) l
