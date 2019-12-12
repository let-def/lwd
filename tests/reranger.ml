open Nottui
open Nottui_widgets
open Lwd.Infix

let is_double_click =
  let k = ref 0 in
  let last = ref (0, 0.0) in
  fun () ->
    let k' = !k in
    incr k;
    fun () ->
      let time = Unix.time () in
      let result =
        let k, t = !last in
        k = k' && t +. 0.4 >= time
      in
      last := (k', time);
      result

let remember_width ~wref ui =
  wref := max (Ui.layout_spec ui).Ui.w !wref;
  Ui.resize ~w:!wref ui

let rec dir ?(initial_path = []) ?after_width:(wref = ref 0) path =
  let column = Lwd.var (Lwd.return Ui.empty) in
  let header = string ~attr:Notty.(A.bg A.green) (Filename.basename path) in
  let after = Lwd.var (Lwd.return Ui.empty) in
  let directories = Lwd_table.make () in
  let files = Lwd_table.make () in
  let body =
    Nottui_widgets.scroll_area
      (Lwd_utils.pack Ui.pack_y
         [
           Lwd_table.reduce Ui.pack_y directories;
           Lwd_table.reduce Ui.pack_y files;
         ])
  in
  let rec set_constrain constrain =
    let header =
      Ui.mouse_area
        (fun ~x:_ ~y:_ -> function
          | `Left ->
              set_constrain false;
              Lwd.set after (Lwd.return Ui.empty);
              `Handled | _ -> `Unhandled)
        header
    in
    let t = Lwd_utils.pack Ui.pack_y [ Lwd.return header; body ] in
    let t =
      if constrain then Lwd.map (Ui.resize ~w:12) t
      else Lwd.map (remember_width ~wref) t
    in
    column $= Lwd_utils.pack Ui.pack_x [ t; Lwd.join (Lwd.get after) ]
  in
  set_constrain false;
  let after_width = ref 0 in
  let goto ?initial_path name =
    set_constrain true;
    let t =
      try dir ?initial_path ~after_width (Filename.concat path name)
      with exn ->
        Lwd.return (string ~attr:Notty.(A.bg A.red) (Printexc.to_string exn))
    in
    after $= Lwd.map (Ui.join_x (string " ")) t
  in
  let highlighted_cell = ref None in
  let rec render_directory ?(highlight = false) cell name =
    if highlight then (
      ( match !highlighted_cell with
      | None -> ()
      | Some (cell, name) -> render_directory cell name );
      highlighted_cell := Some (cell, name) );
    Lwd_table.set cell
    @@ Ui.mouse_area
         (fun ~x:_ ~y:_ -> function
           | `Left ->
               render_directory ~highlight:true cell name;
               goto name;
               `Handled | _ -> `Unhandled)
         (string
            ~attr:Notty.(A.bg (if highlight then A.lightblue else A.blue))
            name)
  in
  let add_directory name =
    if name <> "" && name.[0] <> '.' then
      let highlight =
        match initial_path with x :: _ when x = name -> true | _ -> false
      in
      render_directory ~highlight (Lwd_table.append directories) name
  in
  let add_file name =
    let is_double_click = is_double_click () in
    Lwd_table.set (Lwd_table.append files)
    @@ Ui.mouse_area
         (fun ~x:_ ~y:_ -> function
           | `Left ->
               if is_double_click () then
                 ignore
                   ( Sys.command
                       ( "xdg-open "
                       ^ Filename.quote (Filename.concat path name) )
                     : int );
               `Handled | _ -> `Unhandled)
         (string name)
  in
  let entries = Sys.readdir path in
  Array.sort String.compare entries;
  Array.iter
    (fun name ->
      let path = Filename.concat path name in
      try if Sys.is_directory path then add_directory name else add_file name
      with exn ->
        let text =
          match exn with Sys_error _ -> name | exn -> Printexc.to_string exn
        in
        Lwd_table.append' files (string ~attr:Notty.(A.bg A.red) text))
    entries;
  (match initial_path with [] -> () | x :: xs -> goto ~initial_path:xs x);
  Lwd.join (Lwd.get column)

let gravity_fill = Gravity.make ~h:`Negative ~v:`Negative

let gravity_crop = Gravity.make ~h:`Positive ~v:`Negative

let () =
  let initial_path =
    let rec split path =
      let parent = Filename.dirname path in
      if parent = path then [] else Filename.basename path :: split parent
    in
    List.rev (split (Sys.getcwd ()))
  in
  Ui_loop.run
    (Lwd.map' (dir ~initial_path "/") (fun ui ->
         ui |> Ui.resize ~fill:gravity_fill ~crop:gravity_crop))
