open Brr
open Brr_lwd

type square = On | Off

let flip = function On -> Off | Off -> On

let class_of_state =
  function
  | On -> Jstr.v "square-on"
  | Off -> Jstr.v "square-off"

let lwd_table_row_map ~f row =
  Lwd_table.get row |> Option.iter (fun v -> Lwd.set v (f (Lwd.peek v)))

let ui =
  let squares = Lwd_table.make () in
  let add_square () =
    let row = Lwd_table.append squares in
    Lwd_table.set row (Lwd.var Off)
  in
  for _ = 1 to 20 * 25 do
    add_square ()
  done;
  let board =
    Lwd_table.map_reduce
      (fun row state ->
         Lwd_seq.element @@
         Elwd.div
           ~at:[
             `P (At.class' (Jstr.v "square"));
             `R ((Lwd.map ~f:(fun x -> At.class' (class_of_state x)) (Lwd.get state)));
           ]
           ~ev:[
             `P (Elwd.handler Ev.click
                   (fun _ -> lwd_table_row_map row ~f:(fun state -> flip state)))
           ]
           []
      )
      Lwd_seq.monoid
      squares
  in
  [
    `S (Lwd_seq.lift board)
  ]

let defer_after_dom_loading f =
  let on_load _ = f () in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window));
  ()

let () =
  defer_after_dom_loading @@ fun () ->
  match El.find_first_by_selector (Jstr.v ".game-board") with
  | None -> failwith ".game-board could not be found, check your html"
  | Some main ->
    let _remove_token = Elwd.set_children main ui in
    ()
