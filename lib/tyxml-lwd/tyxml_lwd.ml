open Js_of_ocaml

let js_string_of_float f = (Js.number_of_float f)##toString

let js_string_of_int i = (Js.number_of_float (float_of_int i))##toString

module Elt = struct
  type 'a t = 'a Lwd_seq.t Lwd.t
  type 'a child = 'a t
  let inject x = x
end

module Child = struct
  type 'a t = 'a Elt.t
  let return x = Lwd.pure (Lwd_seq.element x)

  type 'a list = 'a t
  let nil = let nil = Lwd.pure Lwd_seq.empty in fun () -> nil
  let singleton x = x
  let append l1 l2 = Lwd.map2 Lwd_seq.concat l1 l2
  let cons x xs = append (singleton x) xs
end

module Attr = struct
  type 'a t = 'a option Lwd.t
  type (-'a,'b) ft = 'a -> 'b
  let return x = Lwd.return (Some x)
  let fmap f x = Lwd.map (function None -> None | Some x -> Some (f x)) x
end

module Xml
  : Xml_sigs.T
    with module Elt = Elt
     and module Child = Child
     and module Attr = Attr
     and type data = Dom.node Js.t
     and type event_handler          = (Dom_html.event Js.t -> bool) Attr.t
     and type mouse_event_handler    = (Dom_html.mouseEvent Js.t -> bool) Attr.t
     and type keyboard_event_handler = (Dom_html.keyboardEvent Js.t -> bool) Attr.t
     and type touch_event_handler    = (Dom_html.touchEvent Js.t -> bool) Attr.t
= struct

  module Elt = Elt
  module Attr = Attr
  type 'a attr = 'a Attr.t
  module Child = Child

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s

  type aname = string

  type event_handler          = (Dom_html.event Js.t -> bool) attr
  type mouse_event_handler    = (Dom_html.mouseEvent Js.t -> bool) attr
  type keyboard_event_handler = (Dom_html.keyboardEvent Js.t -> bool) attr
  type touch_event_handler    = (Dom_html.touchEvent Js.t -> bool) attr

  type 'a attrib_k =
    | Event          : (Dom_html.event Js.t -> bool) attrib_k
    | Event_mouse    : (Dom_html.mouseEvent Js.t -> bool) attrib_k
    | Event_keyboard : (Dom_html.keyboardEvent Js.t -> bool) attrib_k
    | Event_touch    : (Dom_html.touchEvent Js.t -> bool) attrib_k
    | Attr_float     : float attrib_k
    | Attr_int       : int attrib_k
    | Attr_string    : string attrib_k
    | Attr_space_sep : string list attrib_k
    | Attr_comma_sep : string list attrib_k
    | Attr_uri       : string attrib_k
    | Attr_uris      : string list attrib_k

  type 'a attrib_v = {name: string; kind : 'a attrib_k; value: 'a attr}
  type attrib = Attrib : 'a attrib_v -> attrib [@@ocaml.unboxed]

  let attrib kind name value = Attrib {name; kind; value}
  let float_attrib                  n v = attrib Attr_float n v
  let int_attrib                    n v = attrib Attr_int n v
  let string_attrib                 n v = attrib Attr_string n v
  let space_sep_attrib              n v = attrib Attr_space_sep n v
  let comma_sep_attrib              n v = attrib Attr_comma_sep n v
  let event_handler_attrib          n v = attrib Event n v
  let mouse_event_handler_attrib    n v = attrib Event_mouse n v
  let keyboard_event_handler_attrib n v = attrib Event_keyboard n v
  let touch_event_handler_attrib    n v = attrib Event_touch n v
  let uri_attrib                    n v = attrib Attr_uri n v
  let uris_attrib                   n v = attrib Attr_uris n v

  let attach
      (type a) (node: #Dom.element Js.t) (k: a attrib_v) (v : a option) : unit =
    let name_js = Js.string k.name in
    match v with
    | None -> begin match k.kind with
        | Event | Event_mouse | Event_keyboard | Event_touch ->
          Js.Unsafe.set node name_js Js.null
        | Attr_float | Attr_int | Attr_string | Attr_space_sep
        | Attr_comma_sep | Attr_uri | Attr_uris ->
          Js.Unsafe.delete node name_js
      end
    | Some v -> begin match k.kind with
        | Event          -> Js.Unsafe.set node name_js (fun ev -> Js.bool (v ev))
        | Event_mouse    -> Js.Unsafe.set node name_js (fun ev -> Js.bool (v ev))
        | Event_keyboard -> Js.Unsafe.set node name_js (fun ev -> Js.bool (v ev))
        | Event_touch    -> Js.Unsafe.set node name_js (fun ev -> Js.bool (v ev))
        | Attr_float     -> Js.Unsafe.set node name_js (Js.float v)
        | Attr_int       -> Js.Unsafe.set node name_js v
        | Attr_string    -> Js.Unsafe.set node name_js (Js.string v)
        | Attr_space_sep -> Js.Unsafe.set node name_js (Js.string (String.concat " " v))
        | Attr_comma_sep -> Js.Unsafe.set node name_js (Js.string (String.concat "," v))
        | Attr_uri       -> Js.Unsafe.set node name_js (Js.string v)
        | Attr_uris      -> Js.Unsafe.set node name_js (Js.string (String.concat " " v))
      end

  (** Element *)

  type data = Dom.node Js.t
  type elt = data Elt.t
  type children = data Child.list

  type ename = string

  let as_node (x : #Dom.node Js.t) = (x :> Dom.node Js.t)
  let pure_node x = Child.return (as_node x)

  let empty () = pure_node Dom_html.document##createDocumentFragment

  let comment c = pure_node (Dom_html.document##createComment (Js.string c))

  let string_monoid =
    let cat a b = match a, b with "", x | x, "" -> x | a, b -> a ^ b in
    ("", cat)

  let pcdata input =
    let node =
      Lwd_seq.element (Dom_html.document##createTextNode (Js.string ""))
    in
    let text = Lwd_seq.fold_monoid (fun x -> x) string_monoid input in
    Lwd.map (fun text ->
        begin match Lwd_seq.view node with
          | Lwd_seq.Element elt -> elt##.data := Js.string text;
          | _ -> assert false
        end;
        (node : Dom.text Js.t Lwd_seq.t :> data Lwd_seq.t)
      ) text

  let encodedpcdata = pcdata

  let entity =
    let string_fold s ~pos ~init ~f =
      let r = ref init in
      for i = pos to String.length s - 1 do
        let c = s.[i] in
        r := f !r c
      done;
      !r
    in
    let invalid_entity e = failwith (Printf.sprintf "Invalid entity %S" e) in
    let int_of_char = function
      | '0' .. '9' as x -> Some (Char.code x - Char.code '0')
      | 'a' .. 'f' as x -> Some (Char.code x - Char.code 'a' + 10)
      | 'A' .. 'F' as x -> Some (Char.code x - Char.code 'A' + 10)
      | _ -> None
    in
    let parse_int ~pos ~base e =
      string_fold e ~pos ~init:0 ~f:(fun acc x ->
          match int_of_char x with
          | Some d when d < base -> (acc * base) + d
          | Some _ | None -> invalid_entity e)
    in
    let is_alpha_num = function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    in
    fun e ->
      let len = String.length e in
      let str =
        if len >= 1 && Char.equal e.[0] '#'
        then
          let i =
            if len >= 2 && (Char.equal e.[1] 'x' || Char.equal e.[1] 'X')
            then parse_int ~pos:2 ~base:16 e
            else parse_int ~pos:1 ~base:10 e
          in
          Js.string_constr##fromCharCode i
        else if string_fold e ~pos:0 ~init:true ~f:(fun acc x ->
                    (* This is not quite right according to
                       https://www.xml.com/axml/target.html#NT-Name.
                       but it seems to cover all html5 entities
                       https://dev.w3.org/html5/html-author/charref *)
                    acc && is_alpha_num x)
        then
          match e with
          | "quot" -> Js.string "\""
          | "amp" -> Js.string "&"
          | "apos" -> Js.string "'"
          | "lt" -> Js.string "<"
          | "gt" -> Js.string ">"
          | "" -> invalid_entity e
          | _ -> Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";"))
        else invalid_entity e
      in
      pure_node (Dom_html.document##createTextNode str)

  let attach_attribs node l =
    Lwd_utils.pack ((), fun () () -> ())
      (List.map (fun (Attrib a) -> Lwd.map (attach node a) a.value) l)

  let leaf ?(a = []) name : elt =
    let e = Dom_html.document##createElement (Js.string name) in
    let e' = Lwd_seq.element (e : Dom_html.element Js.t :> data) in
    Lwd.map' (attach_attribs e a) (fun () -> e')

  type child_tree =
    | Leaf of data
    | Inner of { mutable bound: data Js.opt;
                 left: child_tree; right: child_tree; }

  let child_node node = Leaf node

  let child_join left right = Inner { bound = Js.null; left; right }

  let update_children (self : data) (children : children) : unit Lwd.t =
    let reducer =
      ref (Lwd_seq.Reducer.make ~map:child_node ~reduce:child_join)
    in
    Lwd.map' children @@ fun children ->
    let dropped, reducer' =
      Lwd_seq.Reducer.update_and_get_dropped !reducer children in
    let remove_child child () = match child with
      | Leaf node -> ignore (self##removeChild node)
      | Inner _ -> ()
    in
    Lwd_seq.Reducer.fold_dropped `Map remove_child dropped ();
    begin match Lwd_seq.Reducer.reduce reducer' with
      | None -> ()
      | Some tree ->
        let rec update acc = function
          | Leaf x ->
            ignore (self##insertBefore x acc);
            Js.some x
          | Inner t ->
            if Js.Opt.test t.bound then t.bound else (
              let acc = update acc t.right in
              let acc = update acc t.left in
              t.bound <- acc;
              acc
            )
        in
        ignore (update Js.null tree)
    end

  let node ?(a = []) name children : elt =
    let e = Dom_html.document##createElement (Js.string name) in
    let e' = Lwd_seq.element e in
    Lwd.map2'
      (update_children (e :> data) children)
      (attach_attribs e a)
      (fun () () -> (e' :> data Lwd_seq.t))

  let cdata s = pure_node (Dom_html.document##createTextNode (Js.string s))

  let cdata_script s = cdata s

  let cdata_style s = cdata s
end
