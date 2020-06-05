open Notty

module Focus :
sig
  type handle
  val make : unit -> handle
  val request : handle -> unit
  val release : handle -> unit

  type status
  val empty : status
  val status : handle -> status Lwd.t
  val has_focus : status -> bool
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

  type semantic_key = [
    (* Clipboard *)
    | `Copy
    | `Paste
    (* Focus management *)
    | `Focus of [`Next | `Prev | `Left | `Right | `Up | `Down]
  ]

  type key = [
    | Unescape.special | `Uchar of Uchar.t | `ASCII of char | semantic_key
  ] * Unescape.mods

  type mouse = Unescape.mouse

  type event = [ `Key of key | `Mouse of mouse | `Paste of Unescape.paste ]

  type layout_spec = { w : int; h : int; sw : int; sh : int; }
  val pp_layout_spec : Format.formatter -> layout_spec -> unit

  type t
  val pp : Format.formatter -> t -> unit

  val empty : t
  val atom : image -> t
  val mouse_area : mouse_handler -> t -> t
  val has_focus : t -> bool
  val keyboard_area : ?focus:Focus.status -> (key -> may_handle) -> t -> t
  val scroll_area : int -> int -> t -> t
  val size_sensor : (int -> int -> unit) -> t -> t
  val full_sensor :
    ?before:(int -> int -> int -> int -> unit) ->
    ?after:(int -> int -> int -> int -> unit) -> t -> t
  val resize :
    ?w:int -> ?h:int -> ?sw:int -> ?sh:int ->
    ?fill:Gravity.t -> ?crop:Gravity.t -> ?bg:attr -> t -> t
  val overlay :
    ?dx:int -> ?dy:int ->
    ?handler:mouse_handler -> ?origin:gravity -> ?direction:gravity ->
    t -> t
  val event_filter :
    ?focus:Focus.status ->
    ([`Key of key | `Mouse of mouse] -> may_handle) -> t -> t

  val join_x : t -> t -> t
  val join_y : t -> t -> t
  val join_z : t -> t -> t
  val pack_x : t Lwd_utils.monoid
  val pack_y : t Lwd_utils.monoid
  val pack_z : t Lwd_utils.monoid
  val hcat : t list -> t
  val vcat : t list -> t
  val zcat : t list -> t

  val void : int -> int -> t
  (** Void space of dimensions [x,y]. Useful for padding and interstitial
      space. *)

  val layout_spec : t -> layout_spec
  val layout_width : t -> int
  val layout_stretch_width : t -> int
  val layout_height : t -> int
  val layout_stretch_height : t -> int
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
  val dispatch_mouse : t -> Ui.mouse -> Ui.may_handle
  val dispatch_key   : t -> Ui.key   -> Ui.may_handle
  val dispatch_event : t -> Ui.event -> Ui.may_handle
end

module Ui_loop :
sig
  open Notty_unix
  val step : ?process_event:bool -> ?timeout:float -> renderer:Renderer.t ->
    Term.t -> ui Lwd.root -> unit
  val run :
    ?tick_period:float -> ?tick:(unit -> unit) ->
    ?term:Term.t -> ?renderer:Renderer.t ->
    ?quit:bool Lwd.var -> ui Lwd.t -> unit
end
