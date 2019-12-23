type request = int * bool Lwd.var

type t =
  | Empty
  | Handle of {
      request: request Lwd.var;
      focused: bool Lwd.var;
    }
  | Merge of {
      request: request Lwd.t;
      focused: bool Lwd.t;
      left: t;
      right : t;
    }

let make_handle () =
  let focused = Lwd.var false in
  Handle { request = Lwd.var (0, focused); focused = focused }

let empty = Empty

(* Ticket *)

let get_request = function
  | Merge t -> t.request
  | Handle t -> Lwd.get t.request
  | Empty -> assert false

let lwd_false = Lwd.pure false
let has_focus = function
  | Empty -> lwd_false
  | Merge t -> t.focused
  | Handle t -> Lwd.get t.focused

let merge t1 t2 =
  match t1, t2 with
  | Empty, x | x, Empty -> x
  | _ ->
    let argmax (a1, _ as h1) (a2, _ as h2) = if a1 > a2 then h1 else h2 in
    let request1 = get_request t1 and request2 = get_request t2 in
    let request = Lwd.map2 argmax request1 request2 in
    let focused1 = has_focus t1 and focused2 = has_focus t2 in
    let focused = Lwd.map2 (||) focused1 focused2 in
    Merge { request; focused; left = t1; right = t2 }

let clock = ref 0

let rec request_focus : t -> unit = function
  | Handle { request; focused } ->
    incr clock;
    Lwd.set request (!clock, focused)
  | Merge { left; _ }  ->
    request_focus left
  | Empty -> ()

type root = {
  mutable last_focus : bool Lwd.var;
  var : t Lwd.var;
  root : request Lwd.root;
  focused : bool Lwd.root;
}

let make_root ?on_invalidate () =
  let var = Lwd.var empty in
  let default = (0, Lwd.var false) in
  let on_invalidate = match on_invalidate with
    | None -> None
    | Some f -> Some (fun (n, _) -> f n)
  in
  let root = Lwd.observe ?on_invalidate (Lwd.bind (Lwd.get var) (function
      | Empty -> Lwd.pure default
      | other -> get_request other
    )) in
  let focused = Lwd.observe (Lwd.bind (Lwd.get var) has_focus) in
  { last_focus = snd default; var; root; focused }

let update root t =
  let last_focus = root.last_focus in
  let _last_tree = Lwd.peek root.var in
  Lwd.set root.var t;
  let (new_time, new_focus) = Lwd.sample root.root in
  root.last_focus <- new_focus;
  if (last_focus != new_focus) && Lwd.peek last_focus then
    Lwd.set last_focus false;
  if new_time > 0 && not (Lwd.peek new_focus) then
    Lwd.set new_focus true

let peek_focus t = Lwd.unsafe_peek (has_focus t)

let focused root = Lwd.sample (root.focused)
