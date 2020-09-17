open Notty
open Nottui

val empty_lwd : ui Lwd.t

(* Primitive printing *)

val string : ?attr:attr -> string -> ui
val int : ?attr:attr -> int -> ui
val bool : ?attr:attr -> bool -> ui
val float_ : ?attr:attr -> float -> ui

(* Printf support *)
val printf : ?attr:attr -> ('a, unit, string, ui) format4 -> 'a
val kprintf : (ui -> 'a) -> ?attr:attr -> ('b, unit, string, 'a) format4 -> 'b

val fmt : ?attr:attr -> ('a, Format.formatter, unit, ui) format4 -> 'a
val kfmt : (ui -> 'a) -> ?attr:attr -> ('b, Format.formatter, unit, 'a) format4 -> 'b

(* FIXME Menu *)
(*val menu_overlay : ?dx:'a -> ?dy:'b -> 'c -> 'd -> 'e*)
val main_menu_item : string -> (unit -> 'a) -> ui Lwd.t
val sub_menu_item : string -> (unit -> 'a) -> ui Lwd.t
val sub_entry : string -> (unit -> unit) -> ui

(* FIXME Explain how scrolling works *)
val scroll_step : int
type scroll_state = { position : int; bound : int; visible : int; total : int }
val default_scroll_state : scroll_state
val vscroll_area :
  state:scroll_state Lwd.t ->
  change:([> `Action | `Content ] -> scroll_state -> unit) ->
  ui Lwd.t -> ui Lwd.t

val scroll_area :
  ?offset:int * int -> ui Lwd.t -> ui Lwd.t

(* FIXME Explain panes *)
val v_pane : ui Lwd.t -> ui Lwd.t -> ui Lwd.t
val h_pane : ui Lwd.t -> ui Lwd.t -> ui Lwd.t

(* FIXME Edit field *)

val edit_field :
  ?focus:Focus.handle ->
  (string * int) Lwd.t ->
  on_change:(string * int -> unit) ->
  on_submit:(string * int -> unit) -> ui Lwd.t

(* FIXME Tabs *)

val tabs : (string * (unit -> ui Lwd.t)) list -> ui Lwd.t

(* FIXME Flex box *)

val flex_box : ?w:int Lwd.t -> ui Lwd.t list -> ui Lwd.t

(* FIXME Unfoldable *)

val unfoldable :
  ?folded_by_default:bool ->
  ui Lwd.t -> (unit -> ui Lwd.t) -> ui Lwd.t

(* FIXME Boxes *)
val hbox : ui Lwd.t list -> ui Lwd.t
val vbox : ui Lwd.t list -> ui Lwd.t
val zbox : ui Lwd.t list -> ui Lwd.t

(* FIXME List *)
val vlist : ?bullet:string -> ui Lwd.t list -> ui Lwd.t

val vlist_with :
  ?bullet:string ->
  ?filter:('a -> bool) Lwd.t ->
  ('a -> ui Lwd.t) -> 'a list Lwd.t -> ui Lwd.t

val grid :
  ?max_h:int -> ?max_w:int ->
  ?pad:gravity -> ?crop:gravity -> ?bg:attr ->
  ?h_space:int -> ?v_space:int ->
  ?headers:ui Lwd.t list ->
  ui Lwd.t list list -> ui Lwd.t

val button : ?attr:attr -> string -> (unit -> unit) -> ui

val file_select :
  ?abs:bool ->
  ?filter:(String.t -> bool) ->
  on_select:(string -> unit) -> unit -> ui Lwd.t

val toggle : ?init:bool -> string Lwd.t -> (bool -> unit) -> ui Lwd.t

val toggle' : string Lwd.t -> bool Lwd.var -> ui Lwd.t
