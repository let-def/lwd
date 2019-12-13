module Ui = Nottui
module W = Nottui_widgets
module C = CBOR.Simple
module A = Notty.A

let unfoldable summary (f: unit -> Ui.Ui.t Lwd.t) : Ui.Ui.t Lwd.t =
  let opened = ref false in
  let v = Lwd.var W.empty_lwd in
  let cursor ~x:_ ~y:_ = function
     | `Left when !opened ->
       opened := false;
       Lwd.set v W.empty_lwd;
       `Handled
     | `Left ->
       opened := true;
       (* call [f] and pad a bit *)
       let inner =
         f()
         |> Lwd.map (fun x -> Ui.Ui.join_x (W.string "> ") x)
       in
       Lwd.set v @@  inner;
       `Handled
     | _ -> `Unhandled
  in
  let mouse =
    Lwd.map (fun m -> Ui.Ui.mouse_area cursor m) summary
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
