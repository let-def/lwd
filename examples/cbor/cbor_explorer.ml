module Ui = Nottui
module W = Nottui_widgets
module C = CBOR.Simple
module A = Notty.A

let unfoldable summary (f: unit -> Ui.Ui.t Lwd.t) : Ui.Ui.t Lwd.t =
  let opened = ref false in
  let v = Lwd.var W.empty_lwd in
  let focus = Lwd.var Ui.Time.origin in
  let focused = Lwd.var `None in
  let refocus () = Lwd.set focus (Ui.Time.next ()) in
  let cursor ~x:_ ~y:_ = function
     | `Left when !opened ->
       opened := false;
       refocus ();
       Lwd.set v W.empty_lwd;
       `Handled
     | `Left ->
       opened := true;
       (* call [f] and pad a bit *)
       let inner =
         f()
         |> Lwd.map (fun x -> Ui.Ui.join_x (W.string "> ") x)
       in
       refocus ();
       Lwd.set v @@  inner;
       `Handled
     | _ -> `Unhandled
  in
  let cutoff_update v x =
    let x' = Lwd.peek v in if x <> x' then Lwd.set v x
  in
  let handler = { Ui.Ui.
    action = (fun _ _ -> `Unhandled);
    status = (fun direct status ->
        let d = match direct with
          | `Direct    -> "`Direct"
          | `Inherited -> "`Inherited"
        in
        let s = match status with
          | `Enter  -> "`Enter"
          | `Change -> "`Change"
          | `Leave  -> "`Leave"
        in
        prerr_endline (s ^ d);
        cutoff_update focused @@ match direct, status with
        | _, `Leave -> `None
        | `Direct , _ -> `Focused
        | `Inherited , _ -> `Sub_focused
      );
  } in
  let mouse =
    summary
    |> Lwd.map2 (fun focused ui ->
        match
          match focused with
          | `None -> None
          | `Focused -> Some Notty.(I.char A.(bg lightblue) '*' 1 1)
          | `Sub_focused -> Some Notty.(I.char A.(bg blue) '*' 1 1)
        with
        | None -> ui
        | Some img -> Ui.Ui.join_x (Ui.Ui.atom img) ui
      ) (Lwd.get focused)
    |> Lwd.map (fun m -> Ui.Ui.mouse_area cursor m)
    |> Lwd.map2 (fun focus ui -> Ui.Ui.focus_area focus handler ui) (Lwd.get focus)
  in
  Lwd_utils.pack Ui.Ui.pack_x [mouse; Lwd.join @@ Lwd.get v]

let ui_of_cbor (c:C.t) =
  let quit = Lwd.var false in
  let w_q = W.main_menu_item "[quit]" (fun () -> Lwd.set quit true; W.empty_lwd) in
  let rec traverse ?(fold=false) (c:C.t) : Ui.ui Lwd.t =
    match c with
    | `Bool b -> Lwd.return (W.printf ~attr:A.(fg blue) "%B" b)
    | `Bytes s -> Lwd.return (W.printf ~attr:A.(fg @@ gray 14) "<bytes(%d)>" (String.length s))
    | `Text s -> Lwd.return (W.string s)
    | `Int i -> Lwd.return @@ W.printf "%d" i
    | `Float f -> Lwd.return @@ W.printf "%f" f
    | `Null -> Lwd.return (W.string "null")
    | `Undefined -> Lwd.return (W.string "undefined")
    | `Simple i -> Lwd.return (W.printf "simple(%d)" i)
    | `Array [] -> Lwd.return (W.string "[]")
    | `Array l ->
      if fold then (
        let summary = Lwd.return @@ W.printf ~attr:A.(fg yellow) "<array(%d)>" (List.length l) in
        unfoldable summary
          (fun () ->
             let l = List.map (traverse ~fold:true) l in
             Lwd_utils.pack Ui.Ui.pack_y l)
      ) else (
        let l = List.map (traverse ~fold:true) l in
        Lwd_utils.pack Ui.Ui.pack_y l
      )
    | `Map [] -> Lwd.return (W.string "{}")
    | `Map [x,y] ->
      unfoldable (traverse x) (fun () -> traverse ~fold:false y)
    | `Map l ->
      let summary = Lwd.return @@ W.printf ~attr:A.(fg yellow) "<map(%d)>" (List.length l) in
      unfoldable summary
        (fun () ->
           let tbl = Lwd_table.make () in
           List.iter (fun (x,y) ->
               let row = Lwd_table.append tbl in
               let kv = unfoldable (traverse x) (fun () -> traverse ~fold:false y) in
               Lwd_table.set row kv)
             l;
           Lwd.join @@ Lwd_table.reduce (Lwd_utils.lift_monoid Ui.Ui.pack_y) tbl)
  in
  let w =
    Lwd.map2 Ui.Ui.join_y w_q
      (Nottui_widgets.scroll_area @@ traverse ~fold:true c)
  in
  Lwd.get quit, w

let show_file f =
  let cbor = CCIO.with_in f (fun ic -> CCIO.read_all ic |> C.decode) in
  let quit, ui = ui_of_cbor cbor in
  Ui.Ui_loop.run ~quit ~tick_period:0.2 ui

let () =
  let f = ref "" in
  Arg.parse (Arg.align [
    ]) (fun x -> f := x) "cbor_explorer <file>";
  if !f = "" then failwith "please provide a cbor file";
  show_file !f
