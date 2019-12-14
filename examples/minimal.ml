open Nottui

(* Put the UI here *)

let bind x f = Lwd.join (Lwd.map f x)

let node title ~f =
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

let root = count_to_10 ()

(*let () = Statmemprof_emacs.start 1E-4 30 5*)

let () = Ui_loop.run ~tick_period:0.2 root
