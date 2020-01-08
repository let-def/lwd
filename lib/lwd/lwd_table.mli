type 'a t
type 'a row

val make : unit -> 'a t
val clear : 'a t -> unit

val prepend : ?set:'a -> 'a t -> 'a row
val append : ?set:'a -> 'a t -> 'a row
val prepend' : 'a t -> 'a -> unit
val append' : 'a t -> 'a -> unit
val before : ?set:'a -> 'a row -> 'a row
val after : ?set:'a -> 'a row -> 'a row

val first : 'a t -> 'a row option
val last : 'a t -> 'a row option
val next : 'a row -> 'a row option
val prev : 'a row -> 'a row option

val get : 'a row -> 'a option
val set : 'a row -> 'a -> unit
val unset : 'a row -> unit

val is_bound : 'a row -> bool
val remove : 'a row -> unit

val reduce : 'a Lwd_utils.monoid -> 'a t -> 'a Lwd.t
val map_reduce : ('a row -> 'a -> 'b) -> 'b Lwd_utils.monoid -> 'a t -> 'b Lwd.t

val iter : ('a -> unit) -> 'a t -> unit

module Infix : sig
  val ($<-) : 'a row -> 'a -> unit
end
