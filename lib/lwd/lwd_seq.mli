(* Sequence construction *)

type +'a t
type +'a seq = 'a t

val empty : 'a seq
val element : 'a -> 'a seq
val concat : 'a seq -> 'a seq -> 'a seq

type ('a, 'b) view =
  | Empty
  | Element of 'a
  | Concat of 'b * 'b

val view : 'a seq -> ('a, 'a seq) view

(* TODO: Balanced sequence construction
   module Balanced : sig
     type nonrec 'a t = private 'a seq
     val empty : 'a t
     val element : 'a -> 'a t
     val concat : 'a t -> 'a t -> 'a t

     val view : 'a t -> ('a, 'a t) view
   end
*)

(* Lwd interface *)

val fold : map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'a seq Lwd.t -> 'b option Lwd.t
val fold_monoid : ('a -> 'b) -> 'b Lwd_utils.monoid -> 'a seq Lwd.t -> 'b Lwd.t
val map : ('a -> 'b) -> 'a seq Lwd.t -> 'b seq Lwd.t
val filter : ('a -> bool) -> 'a seq Lwd.t -> 'a seq Lwd.t
val filter_map : ('a -> 'b option) -> 'a seq Lwd.t -> 'b seq Lwd.t

val lift : 'a Lwd.t seq Lwd.t -> 'a seq Lwd.t

(* Low-level interface *)

module Reducer : sig
  type ('a, 'b) reducer

  val make : map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> ('a, 'b) reducer
  val update : ('a, 'b) reducer -> 'a seq -> ('a, 'b) reducer

  val reduce : ('a, 'b) reducer -> 'b option

  type 'b dropped
  val update_and_get_dropped :
    ('a, 'b) reducer -> 'a seq -> 'b dropped * ('a, 'b) reducer

  val fold_dropped :
    [<`All|`Map|`Reduce] -> ('a -> 'b -> 'b) -> 'a dropped -> 'b -> 'b
end

