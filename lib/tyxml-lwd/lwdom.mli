open Js_of_ocaml
open Tyxml_lwd

module Xml = Xml
module Svg : Svg_sigs.Make(Xml).T
module Html : Html_sigs.Make(Xml)(Svg).T

(** FIXME
    - Explain that in this DOM binding, element is a monoid.
    - Explain how to manage roots, connect to Browser document *)

type 'a t = 'a Xml.Elt.t
type 'a attr = 'a Xml.Attr.t

val elt : 'a -> 'a t
(** Create an element from a value *)

val children : 'a t list -> 'a t
(** Flatten a list of elements *)

val children_array : 'a t array -> 'a t
(** Flatten an array of elements *)

val attr : 'a -> 'a attr
(** Make a constant attribute *)

val lwd_attr : 'a Lwd.t -> 'a attr
(** Make a reactive attribute *)

(** FIXME: Expose higher-level interface, especially for managing roots and
    connecting to the document  *)
val dom_nodes : 'a Html.data Lwd_seq.t -> Dom.node Js.t list
