open Js_of_ocaml

let js_string_of_float f = (Js.number_of_float f)##toString

let js_string_of_int i = (Js.number_of_float (float_of_int i))##toString

module Elt = struct
  type 'a t = 'a Lwd.t
  type 'a child = 'a Lwd.t
  let inject x = x
end

module Child = struct
  type 'a t = 'a Lwd.t
  let return = Lwd.return

  type 'a list = 'a Lwd.t Lwd_seq.t
  let nil () = Lwd_seq.empty
  let singleton x = Lwd_seq.element x
  let cons x xs = Lwd_seq.concat (singleton x) xs
  let append l1 l2 = Lwd_seq.concat l1 l2
end

module Attr = struct
  type 'a t = 'a Lwd.t
  type (-'a,'b) ft = 'a -> 'b
  let return = Lwd.return
  let fmap = Lwd.map
end

module Xml
  : Xml_sigs.T
    with module Elt = Elt
     and module Child = Child
     and module Attr = Attr
     and type data = Dom.node Js.t

= struct

  module Elt = Elt
  module Attr = Attr
  module Child = Child
  
  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s

  type aname = string

  type event_handler = Dom_html.event Js.t -> bool
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
  type touch_event_handler = Dom_html.touchEvent Js.t -> bool

  type attrib_k =
    | Event of event_handler
    | MouseEvent of mouse_event_handler
    | KeyboardEvent of keyboard_event_handler
    | TouchEvent of touch_event_handler
    | Attr of Js.js_string Js.t option Lwd.t

  type attrib = aname * attrib_k

  let attr name v = name, Attr (Lwd.pure (Some v))

  let float_attrib name value : attrib = attr name (js_string_of_float value)

  let int_attrib name value = attr name (js_string_of_int value)

  let string_attrib name value = attr name (Js.string value)

  let space_sep_attrib name values = attr name (Js.string (String.concat " " values))

  let comma_sep_attrib name values = attr name (Js.string (String.concat "," values))

  let event_handler_attrib name (value : event_handler) = name, Event value

  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    name, MouseEvent value

  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    name, KeyboardEvent value

  let touch_event_handler_attrib name (value : touch_event_handler) =
    name, TouchEvent value

  let uri_attrib name value = attr name (Js.string value)

  let uris_attrib name values = attr name (Js.string (String.concat " " values))

  (** Element *)

  type data = Dom.node Js.t
  type elt = data Lwd.t
  type children = data Lwd_seq.t
  
  type ename = string

  let empty () = (Dom_html.document##createDocumentFragment :> Dom.node Js.t)

  let comment c = (Dom_html.document##createComment (Js.string c) :> Dom.node Js.t)

  let pcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)

  let encodedpcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)

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
      (Dom_html.document##createTextNode str :> Dom.node Js.t)

  (* TODO: fix get_prop
     it only work when html attribute and dom property names correspond.
     find a way to get dom property name corresponding to html attribute
  *)

  let get_prop node name =
    if Js.Optdef.test (Js.Unsafe.get node name) then Some name else None

  let iter_prop_protected node name f =
    match get_prop node name with
    | Some n -> ( try f n with _ -> ())
    | None -> ()

  let attach_attribs node l =
    List.fold_left
      (fun (acc : unit Lwd.t) (name, att) ->
         let name_js = Js.string name in
         match att with
         | Attr a ->
           (* Note that once we have weak pointers working, we'll need to React.S.retain *)
           Lwd.map2
             (fun () -> function
                | Some v -> (
                    ignore (node##setAttribute name_js v);
                    match name with
                    | "style" -> node##.style##.cssText := v
                    | _ ->
                      iter_prop_protected node name_js (fun name ->
                          Js.Unsafe.set node name v))
                | None -> (
                    ignore (node##removeAttribute name_js);
                    match name with
                    | "style" -> node##.style##.cssText := Js.string ""
                    | _ ->
                      iter_prop_protected node name_js (fun name ->
                          Js.Unsafe.set node name Js.null)))
             acc a
         | Event h ->
           Js.Unsafe.set node name_js (fun ev -> Js.bool (h ev));
           acc
         | MouseEvent h ->
           Js.Unsafe.set node name_js (fun ev -> Js.bool (h ev));
           acc
         | KeyboardEvent h ->
           Js.Unsafe.set node name_js (fun ev -> Js.bool (h ev));
           acc
         | TouchEvent h ->
           Js.Unsafe.set node name_js (fun ev -> Js.bool (h ev));
           acc
      )
      (Lwd.pure ()) l

  let leaf ?(a = []) name : elt =
    let e = Dom_html.document##createElement (Js.string name) in
    Lwd.map (fun () -> (e :> data)) @@ attach_attribs e a

  let update_children (_ : data) (_ : children) : unit = assert false
  
  let node ?(a = []) name children : elt =
    let e = Dom_html.document##createElement (Js.string name) in
    update_children (e :> data) children;
    Lwd.map (fun () -> (e :> data)) @@ attach_attribs e a
    
  let cdata s = pcdata s

  let cdata_script s = cdata s

  let cdata_style s = cdata s
end
