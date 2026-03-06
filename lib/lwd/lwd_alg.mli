(** {0 Incremental transformation on custom data types}

    This library provides the logic to memoize custom transformations on custom
    datatypes, generalizing `Lwd_seq` to your own algebraic data types.
*)

(** {1 Recursively foldable types}

    To incrementalize a custom data type, the library needs to be able to peek
    into its structure and store custom information.

    [Lwd_alg.t] extends a user type with the necessary information: if [u] is
    some algebraic data type, [u Lwd_alg.t] is the "incrementalized" version.

    [!]: the type is injective in its parameter,
         if [u == v] then [u t == t v]
*)
type !'a t

(** To incrementalize transformations on a value of type [a], we need to peek
    into the incrementalized sub-values of [a].

    This is done by asking the user to provide a function that feeds all
    sub-values to a [folder].
*)

type folder
val accumulate : 'a t -> folder -> folder

(**
    We get an [a t] by equipping [a] with [a foldable]
   (the function that accumulates all sub-values to a folder).
*)
type 'a foldable = 'a -> folder -> folder

(** Lift an [a] to an [a t] *)
val make : 'a -> 'a foldable -> 'a t

(** Access the [a] that was lifted *)
val peek : 'a t -> 'a

(** Map and traces: constructing incremental computations *)

(** A trace is an opaque type we use to track the sub-computations. *)
type trace

(** An [('a, 'b) map] represents an incremental computation transformation from
    ['a t] to ['b t]. *)
type ('a, 'b) map

(** A map is function that receives a trace that can be applied when decomposing
    the transformation. *)
val map : ?finalize:('a -> 'b -> unit) -> (trace -> 'a -> 'b) -> ('a, 'b) map

(** Given a trace, one can apply a map on a sub-value. *)
val apply : trace -> ('a, 'b) map -> 'a t -> 'b

(** Resumptions

    A function ['a -> 'b] transforms values of type ['a] to ['b].
    To incrementalize this transformation we need to remember the intermediate
    steps that could be re-used when transforming an updated version of the data
    structure.

    This "function application with memory" is represented by a resumption: a
    function that produces a result and an updated version of the
    transformation.

    Let's assume that
      [f : 'a -> b] is a transformation, and
      [r : ('a, 'b) resumption] it's incrementalized version.
    We can apply both:
    - [y = f x]
    - [R (y, r') = r x]
    Now if we have an [x'], then
    - [y' = f x'] will recompute everything, but
    - [R (y', r'') = r' x'] recompute only the delta between [x] and [x'].
*)
type ('a, 'b) resumption = R of ('a -> 'b * ('a, 'b) resumption) [@@ocaml.unboxed]

(** Instantiating an incremental computation yields a resumption consumming
    incremental values. *)
val transform : ('a, 'b) map -> ('a t, 'b) resumption
