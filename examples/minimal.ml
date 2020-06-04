open Nottui

(* Put the UI here *)

(*let node title ~f =
  let vopened = Lwd.var false in
  let label =
    Lwd.map' (Lwd.get vopened) @@ fun opened ->
    let text = if opened then "[-]" else "[+]" in
    Ui.mouse_area (fun ~x:_ ~y:_ -> function
        | `Left -> Lwd.set vopened (not opened); `Handled
        | _ -> `Unhandled
      ) (Ui.atom Notty.(I.string A.empty text))
  in
  let content = Lwd.bind (Lwd.get vopened) @@ function
    | true -> f ()
    | false -> Lwd.pure Ui.empty
  in
  Lwd.map2' label content (fun lbl content ->
      Ui.join_x lbl
        (Ui.join_y (Ui.atom Notty.(I.string A.empty title)) content)
    )

let rec count_to_10 () =
  Lwd_utils.pack Ui.pack_y (
    List.map
      (fun i -> node (string_of_int i) ~f:count_to_10)
      [1;2;3;4;5;6;7;8;9;10]
  )

let root = count_to_10 ()*)

let f_to_c x = (x -. 32.0) *. 5.0/.9.0
let c_to_f x = x *. 9.0/.5.0 +. 32.0

let degrees = Lwd.var 0.0

let farenheit = Lwd.var (nan, ("", 0))

let farenheit_text =
  Lwd.map2' (Lwd.get degrees) (Lwd.get farenheit)
    (fun d (d', f) ->
       if d = d' then f else (string_of_float (c_to_f d), 0))

let farenheit_edit =
  Nottui_widgets.edit_field
    farenheit_text
    ~on_change:(fun (text, _ as state) ->
        let d = match float_of_string_opt text with
          | None -> Lwd.peek degrees
          | Some d -> let d = f_to_c d in Lwd.set degrees d; d
        in
        Lwd.set farenheit (d, state)
      )
    ~on_submit:ignore

let celsius = Lwd.var (nan, ("", 0))

let celsius_text =
  Lwd.map2' (Lwd.get degrees) (Lwd.get celsius)
    (fun d (d', f) -> if d = d' then f else (string_of_float d, 0))

let celsius_edit =
  Nottui_widgets.edit_field
    celsius_text
    ~on_change:(fun (text, _ as state) ->
        let d = match float_of_string_opt text with
          | None -> Lwd.peek degrees
          | Some d -> Lwd.set degrees d; d
        in
        Lwd.set celsius (d, state)
      )
    ~on_submit:ignore

let root =
  let base =
    Lwd_utils.pack Ui.pack_y [
      Lwd.pure (Nottui_widgets.string "Celsius:");
      celsius_edit;
      Lwd.pure (Nottui_widgets.string "Farenheight:");
      farenheit_edit;
    ]
  in
  let base = Lwd.map2 Ui.join_y base base in
  let base = Lwd.map2 Ui.join_y base base in
  let base = Lwd.map2 Ui.join_y base base in
  let base = Lwd.map2 Ui.join_y base base in
  let base = Lwd.map2 Ui.join_y base base in
  base

(*let () = Statmemprof_emacs.start 1E-4 30 5*)

let () = Ui_loop.run ~tick_period:0.2 root
