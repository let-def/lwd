(** A set of abstract elements annotated with values of type ['a] that can be
   efficiently diffed. *)
type 'a t

(** The empty set *)
val empty : 'a t

(** [element x] creates a new set element tagged with metadata [x] (O(1)).
    It is the physical identity of the element that is considered when
    computing set difference, not the tag.
    Therefore [diff (element x) (element x) = { added = [x]; removed = [x]; }]
    But [(let e = element x in diff e e) = { added = []; removed = []; }]
*)
val element : 'a -> 'a t

(** The union of two set of resources (O(1)) *)
val union : 'a t -> 'a t -> 'a t

(** Compute the difference between two sets.

    [diff old_set new_set = { added; removed }]
    where [removed] lists the tag of elements only present in [old_set]
     and [added] lists the tag of elements only present in [new_set]

    _Conjecture_: the algorithm is linear in the number of changes between
    [old_set] and [new_set].

    When used in a linear fashion (you have a sequence of sets [s_i] and only
    compare [s_i] and [s_i+1], at most once for each [i]), it should not affect
    the complexity of the program.
*)
type 'a diff = { added : 'a list; removed : 'a list }
val diff : 'a t -> 'a t -> 'a diff
