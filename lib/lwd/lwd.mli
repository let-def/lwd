type 'a t
val return : 'a -> 'a t
val pure : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val map' : 'a t -> ('a -> 'b) -> 'b t
val map2' : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
val join : 'a t t -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val app : ('a -> 'b) t -> 'a t -> 'b t
val pair : 'a t -> 'b t -> ('a * 'b) t

val impure : 'a t -> 'a t

type 'a var
val var : 'a -> 'a var
val get : 'a var -> 'a t
val set : 'a var -> 'a -> unit
val peek : 'a  var -> 'a

type 'a prim
val prim : acquire:(unit -> 'a) -> release:('a -> unit) -> 'a prim
val get_prim : 'a prim -> 'a t
val invalidate : 'a prim -> unit

type release_failure = exn * Printexc.raw_backtrace
exception Release_failure of release_failure list

type 'a root
val observe : ?on_invalidate:('a -> unit) -> 'a t -> 'a root
val set_on_invalidate : 'a root -> ('a -> unit) -> unit

val sample : 'a root -> 'a
val is_damaged : 'a root -> bool
val release : 'a root -> unit

(*val unsafe_peek : 'a t -> 'a option*)

module Infix : sig
  val (let$) : 'a t -> ('a -> 'b t) -> 'b t
  val (and$) : 'a t -> 'b t -> ('a * 'b) t
  val ($=) : 'a var -> 'a -> unit
end
