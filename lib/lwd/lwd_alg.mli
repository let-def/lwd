(** {0 Incremental transformation on custom data types}:

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

type folder_kind
type folder

(**
    We get an [a t] by equipping [a] with [a foldable]
   (the function that accumulates all sub-values to a folder).
*)
type 'a foldable = folder_kind -> folder -> 'a -> folder
val add : 'a t foldable


(** Lift an [a] to an [a t] *)
val make : 'a foldable -> 'a -> 'a t

(** Access the [a] that was lifted *)
val peek : 'a t -> 'a

(** Map and tapes: constructing incremental computations *)

(** A tape is an opaque type we use to track the sub-computations. *)
type tape

(** An [('a, 'b) map] represents an incremental computation transformation from
    ['a t] to ['b t]. *)
type ('a, 'b) map

(** A map is function that receives a tape to record sub-transformations. *)
val map : ?finalize:('a -> 'b -> unit) -> (tape -> 'a -> 'b) -> ('a, 'b) map

(** Creates a placeholder map that has no implementation.
    Calling [apply] or [transform] on a map created by [undefined] before it has
    been filled by [define] raises [Failure]. *)
val undefined : unit -> ('a, 'b) map

(** Binds a concrete implementation to an existing [undefined] map.
    Used to construct recursive maps by providing the transformation logic.
    If the map has already been defined, [define] raises [Invalid_argument]. *)
val define : ('a, 'b) map -> ?finalize:('a -> 'b -> unit) -> (tape -> 'a -> 'b) -> unit

(** Given a tape, one can apply a map on a sub-value. *)
val apply : tape -> ('a, 'b) map -> 'a t -> 'b

(** An incrementalized applicationt of a map.
    if [f = transform map], then running sequentially:
    1. first [y = f x] computes a complete transformation,
    2. then [y' = f x'] recomputes just the delta between [y'] and [y]
*)
val transform : ('a, 'b) map -> 'a t -> 'b

module Resumption : sig
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
        [r : 'a -> ('a, 'b) r] it's incrementalized version.
      We can apply both:
      - [y = f x]
      - [ry = r x] where [ry.result = y]
      Now if we have an [x'], then:
      - [y' = f x'] will recompute everything, but
      - [ry' = ry.next x'] recompute only the delta between [x] and [x'].
  *)
  type ('a, 'b) r = {result: 'b; next: 'a -> ('a, 'b) r }

  (** Instantiating an incremental computation yields a resumption consumming
      incremental values. *)
  val of_map : ('a, 'b) map -> 'a t -> ('a t, 'b) r
end
