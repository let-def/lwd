type t

val make_handle : unit -> t
val empty : t
val merge : t -> t -> t
val request_focus : t -> unit
val has_focus : t -> bool Lwd.t
val peek_focus : t -> bool option

type root
val make_root : ?on_invalidate:(int -> unit) -> unit -> root
val update : root -> t -> unit
val focused : root -> bool
