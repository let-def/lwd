open Notty

module Focus :
sig
  type handle
  val void : handle
  val make : unit -> handle
  val has_focus : handle -> bool Lwd.t
  val peek_focus : handle -> bool option
  val request : handle -> unit
end

module Gravity :
sig
  type direction = [
    | `Negative
    | `Neutral
    | `Positive
  ]
  val pp_direction : Format.formatter -> direction -> unit
  type t
  val pp : Format.formatter -> t -> unit
  val make : h:direction -> v:direction -> t
  val default : t
  val h : t -> direction
  val v : t -> direction
end
type gravity = Gravity.t

module Ui :
sig
  type may_handle = [ `Unhandled | `Handled ]

  type mouse_handler = x:int -> y:int -> Unescape.button -> [
      | may_handle
      | `Grab of (x:int -> y:int -> unit) * (x:int -> y:int -> unit)
    ]

  type layout_spec = { w : int; h : int; sw : int; sh : int; }
  val pp_layout_spec : Format.formatter -> layout_spec -> unit

  type t
  val pp : Format.formatter -> t -> unit

  val empty : t
  val atom : image -> t
  val mouse_area : mouse_handler -> t -> t
  val has_focus : t -> bool Lwd.t
  val keyboard_area : ?handle:Focus.handle ->
    (Unescape.key -> may_handle) -> t -> t
  val scroll_area : int -> int -> t -> t
  val size_sensor : (int -> int -> unit) -> t -> t
  val resize :
    ?w:int -> ?h:int -> ?sw:int -> ?sh:int ->
    ?fill:Gravity.t -> ?crop:Gravity.t -> ?bg:attr -> t -> t
  val overlay :
    ?dx:int -> ?dy:int ->
    ?handler:mouse_handler -> ?origin:gravity -> ?direction:gravity ->
    t -> t
  val event_filter :
    ?handle:Focus.handle ->
    ([`Key of Unescape.key | `Mouse of Unescape.mouse] -> may_handle) -> t -> t

  val join_x : t -> t -> t
  val join_y : t -> t -> t
  val join_z : t -> t -> t
  val pack_x : t Lwd_utils.monoid
  val pack_y : t Lwd_utils.monoid
  val pack_z : t Lwd_utils.monoid
  val hcat : t list -> t
  val vcat : t list -> t
  val zcat : t list -> t

  val layout_spec : t -> layout_spec
end

type ui = Ui.t

module Renderer :
sig
  type size = int * int

  type t
  val make : unit -> t
  val size : t -> size
  val update : t -> size -> Ui.t -> unit
  val image : t -> image
  val dispatch_mouse : t -> Unescape.mouse -> Ui.may_handle
  val dispatch_key   : t -> Unescape.key   -> Ui.may_handle
  val dispatch_event : t -> Unescape.event -> Ui.may_handle
end

module Ui_loop :
sig
  open Notty_unix
  val step : ?process_event:bool -> ?timeout:float -> renderer:Renderer.t ->
    Term.t -> ui Lwd.root -> unit
  val run :
    ?tick_period:float -> ?tick:(unit -> unit) ->
    ?term:Term.t -> ?renderer:Renderer.t ->
    ?quit:bool Lwd.t -> ui Lwd.t -> unit
end
