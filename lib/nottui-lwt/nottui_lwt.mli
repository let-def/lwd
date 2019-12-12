open Notty
open Nottui

type event = [
  | `Key of Unescape.key
  | `Mouse of Unescape.mouse
  | `Paste of Unescape.paste
  | `Resize of int * int
]

val render : ?quit:unit Lwt.t -> size:int * int -> event Lwt_stream.t -> ui Lwd.t -> image Lwt_stream.t

val run : (*?term:Term.t ->*) ?quit:unit Lwt.t -> ui Lwd.t -> unit Lwt.t
