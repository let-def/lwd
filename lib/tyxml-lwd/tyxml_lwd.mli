open Js_of_ocaml

(** {1 Representation of attributes} *)

module Attr : sig

  type 'a t = 'a option Lwd.t
  (** Attributes are valued with reactive and optional value.

      Reactive because the binding can change over time.
      Optional because the binding can be disabled or enabled.
  *)

  val return : 'a -> 'a t
  (** Constant attribute *)

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping attribute *)

  type ('a, 'b) ft = 'a -> 'b
  (** Attribute transformers are simple functions.
      (Required by TyXML's Xml_sigs) *)

end

type 'a attr = 'a Attr.t

(** {1 Representation of elements} *)

module Elt : sig

  type 'a t = 'a Lwd_seq.t Lwd.t
  (** An element of type ['a].

      [_ Lwd.t] because of reactivity (element can change, dependencies are
      tracked).

      [_ Lwd_seq.t] because we identify element and element sequences: an
      element is a singleton sequence.
      This simplifies most the API, no need to deal with sequence wrapping.
  *)

  type 'a child = 'a t
  (** (Required by TyXML) *)

  val inject : 'a t -> 'a t
  (** (Required by TyXML) *)

end

type 'a elt = 'a Elt.t

(** {1 Representation of children / element list} *)

module Child : sig
  type 'a t = 'a elt
  type 'a list = 'a elt
  (** Elements and element lists are the same. *)

  val empty : 'a elt
  (** Empty list *)

  val return : 'a -> 'a elt
  (** Make a single element *)

  val append : 'a elt -> 'a elt -> 'a elt
  (** Concatenate two lists *)

  (** Definitions required by TyXML *)

  val nil : unit -> 'a elt
  val singleton : 'a elt -> 'a elt
  val cons : 'a elt -> 'a elt -> 'a elt
end

(** {1 TyXML compatible representation of XML documents} *)

module Xml : Xml_sigs.T
  with module Elt = Elt
   and module Child = Child
   and module Attr = Attr
   and type data = Dom.node Js.t
   and type event_handler          = (Dom_html.event Js.t -> bool) attr
   and type mouse_event_handler    = (Dom_html.mouseEvent Js.t -> bool) attr
   and type keyboard_event_handler = (Dom_html.keyboardEvent Js.t -> bool) attr
   and type touch_event_handler    = (Dom_html.touchEvent Js.t -> bool) attr

(** {1 TyXML produced Svg and Html} *)

module Svg : Svg_sigs.Make(Xml).T
module Html : Html_sigs.Make(Xml)(Svg).T

(** {1 Running an Lwd-driven DOM in the browser} *)

module Lwdom : sig

  val elt : 'a -> 'a elt
  (** Create an element from a value *)

  val children : 'a elt list -> 'a elt
  (** Flatten a list of elements *)

  val children_array : 'a elt array -> 'a elt
  (** Flatten an array of elements *)

  val attr : 'a -> 'a attr
  (** Make a constant attribute *)

  val rattr : 'a Lwd.t -> 'a attr
  (** Make a reactive attribute *)

  val dom_nodes : 'a Html.data Lwd_seq.t -> Dom.node Js.t list
  (** FIXME: Expose higher-level interface, especially for managing roots and
      connecting to the document  *)

end
