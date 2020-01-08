val (let$) : 'a Lwd.t -> ('a -> 'b) -> 'b Lwd.t
val (let$*) : 'a Lwd.t -> ('a -> 'b Lwd.t) -> 'b Lwd.t
val (and$) : 'a Lwd.t -> 'b Lwd.t -> ('a * 'b) Lwd.t

val ($=) : 'a Lwd.var -> 'a -> unit
val ($<-) : 'a Lwd_table.row -> 'a -> unit
