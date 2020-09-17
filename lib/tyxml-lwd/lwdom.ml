module Xml = Tyxml_lwd.Xml
module Svg = Svg_f.Make(Tyxml_lwd.Xml)
module Html = Html_f.Make(Tyxml_lwd.Xml)(Svg)

type 'a t = 'a Xml.Elt.t
type 'a attr = 'a Xml.Attr.t

let elt x = Lwd.pure (Lwd_seq.element x)
let attr x : _ Xml.Attr.t = Lwd.pure (Some x)
let lwd_attr x : _ Xml.Attr.t = Lwd.map (fun x -> Some x) x

let dom_nodes x =
  let rec fold x acc = match Lwd_seq.view x with
    | Lwd_seq.Empty -> acc
    | Lwd_seq.Element x -> x :: acc
    | Lwd_seq.Concat (l, r) -> fold l (fold r acc)
  in
  fold x []

let children = function
  | [] -> Xml.Child.nil ()
  | [x] -> x
  | [x; y] -> Lwd.map2 Lwd_seq.concat x y
  | xs -> Lwd_utils.reduce Lwd_seq.lwd_monoid xs

let children_array = function
  | [||] -> Xml.Child.nil ()
  | [|x|] -> x
  | [|x; y|] -> Lwd.map2 Lwd_seq.concat x y
  | xs ->
    Lwd_seq.bind (Lwd_seq.lift (Lwd.pure (Lwd_seq.of_array xs))) (fun x -> x)
