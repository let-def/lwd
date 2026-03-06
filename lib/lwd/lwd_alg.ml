type ('a, 'b) resumption = R of ('a -> 'b * ('a, 'b) resumption) [@@ocaml.unboxed]

type 'acc folder = {folder: 'b. 'b t -> 'acc -> 'acc} [@@ocaml.unboxed]
type 'a foldable = {fold: 'acc. 'a -> 'acc folder -> 'acc -> 'acc} [@@ocaml.unboxed]

type !'a t = {
  mutable mark: int;
  value: 'a;
  foldable: 'a foldable;
  mutable cache :
}

and tape = unit list ref

and ('a, 'b) map = {
  func: tape -> 'a -> 'b;
  mutable result: ('b, exn) result;
}

and 'a transformation = T : ('a, 'b) map * 'b -> 'a transformation

let make foldable value = {foldable; value; mark = 0}
let peek t = t.value

exception Internal_error
let undefined = Result.Error Internal_error

let map func = { func; result = undefined }

let apply tape map t =


val transform : ('a, 'b) map -> ('a t, 'b t) resumption
