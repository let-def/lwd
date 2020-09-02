type 'a monoid = 'a * ('a -> 'a -> 'a)
val lift_monoid : 'a monoid -> 'a Lwd.t monoid

val pack : 'a monoid -> 'a Lwd.t list -> 'a Lwd.t
val pack_seq : 'a monoid -> 'a Lwd.t Seq.t -> 'a Lwd.t
val pure_pack : 'a monoid -> 'a list -> 'a

val map_reduce : ('a -> 'b) -> 'b monoid -> 'a list -> 'b

val local_state : ('a Lwd.t -> ('a -> unit) -> 'a * 'b) -> 'b

val map_l : ('a -> 'b Lwd.t) -> 'a list -> 'b list Lwd.t

val flatten_l : 'a Lwd.t list -> 'a list Lwd.t
