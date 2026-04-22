let cache_changes computation ~equal k =
  let cache = ref None in
  Lwd.bind computation ~f:(fun value ->
      match !cache with
      | None ->
        let var = Lwd.var value in
        let result = k (Lwd.get var) in
        cache := Some (var, result);
        result
      | Some (var, result) ->
        let value' = Lwd.peek var in
        if not (equal value value') then
          Lwd.set var value;
        result
    )

let time = Lwd.var 0

let max_time = 1000
let big_list = List.init max_time Fun.id

let compute time =
  Printf.printf "Let's go with a costly computation with time = %d\n" time;
  List.filter ((>=) time) big_list

(* Without caching *)
let () =
  Printf.printf "Without caching\n";
  let l =
    let open Lwd.Infix in
    (Lwd.get time >|= fun time -> time >= max_time)
    >>= fun very_big ->
    if very_big then
      Lwd.return @@ compute max_time
    else
      (Lwd.get time) >|= fun time ->
      compute time
  in
  let root = Lwd.observe l in
  let () = Lwd.set time (max_time + 1) in
  let _sample1 = Lwd.quick_sample root in
  let () = Lwd.set time (max_time + 2) in
  let _sample2 = Lwd.quick_sample root in
  Lwd.quick_release root

(* With caching *)
let () =
  let l =
    Printf.printf "With caching\n";
    let open Lwd.Infix in
    cache_changes
      ~equal:Bool.equal
      (Lwd.get time >|= fun time -> time >= max_time)
      (fun very_big ->
         very_big >>= fun very_big ->
         if very_big then
           Lwd.return @@ compute max_time
         else
           (Lwd.get time) >|= fun time ->
           compute time
      )
  in
  let root = Lwd.observe l in
  let () = Lwd.set time (max_time + 1) in
  let _sample1 = Lwd.quick_sample root in
  let () = Lwd.set time (max_time + 2) in
  let _sample2 = Lwd.quick_sample root in
  Lwd.quick_release root
