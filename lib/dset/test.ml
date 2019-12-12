open Rtree.Rtree

let x = leaf 1
let y = leaf 2

let _ = diff empty empty
let _ = diff (leaf 1) (leaf 2)
let _ = diff (join x x) (join x x)
let _ = diff (join x y) (join x x)
let _ = diff (join x x) (join x y)
let _ = diff (join x y) (join x y)
let t0 = join x y
let _ = diff t0 t0

let _ =
  let k1 = ref 0 in
  let k2 = ref 1_000_000 in
  let t1 = join t0 (leaf 3) in
  let t2 = join (leaf 0) t0 in
  while true do
    incr k1;
    decr k2;
    ignore (diff t1 t2);
    if !k2 = 0 then (
      k2 := 1_000_000;
      Printf.printf "% 8d iterations in %fs\n%!" !k1 (Sys.time ());
    )
  done
