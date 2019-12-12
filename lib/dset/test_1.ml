open Dset

let check ?(removed=[]) ?(added=[]) diff =
  List.for_all (fun x -> List.mem x added) diff.added &&
  List.for_all (fun x -> List.mem x removed) diff.removed

let e1 = element 1 and e2 = element 2

let () =
  assert (check (diff empty empty));
  assert (check (diff (element 1) (element 2)) ~removed:[1] ~added:[2]);
  assert (check (diff (element 1) (element 1)) ~removed:[1] ~added:[1]);
  assert (check (diff e1 e1));
  assert (check (diff e1 e2) ~removed:[1] ~added:[2]);
  assert (check (diff e2 e1) ~removed:[2] ~added:[1]);
  assert (check (diff e2 e2));
  assert (check (diff (union e1 e1) (union e1 e1)));
  assert (check (diff (union e1 e2) (union e1 e1)) ~removed:[2]);
  assert (check (diff (union e1 e1) (union e1 e2)) ~added:[2]);
  assert (check (diff (union e1 e2) (union e1 e2)));
  assert (check (diff (union e1 e2) empty) ~removed:[2; 1]);
  assert (check (diff empty (union e1 e2)) ~added:[1; 2]);
