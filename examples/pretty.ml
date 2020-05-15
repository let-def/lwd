module P = Nottui_pretty

let string ?attr text = P.ui (Nottui_widgets.string ?attr text)

let (^^) = P.(^^)
let (^/^) a b = P.(a ^^ break 1 ^^ b)


let doc =
  List.fold_left (^^) P.empty [
    P.group (string "This" ^/^ string "is" ^/^ string "pretty.");
    P.hardline; P.ui (Nottui.Ui.void 0 1); P.hardline;
    P.group (P.group (string "This" ^/^ string "is") ^/^ string "pretty.");
    P.hardline; P.ui (Nottui.Ui.void 0 1); P.hardline;
    P.group (string "This" ^/^ P.group (string "is" ^/^ string "pretty."));
  ]

let varying_width f =
  let width = Lwd.var 0 in
  Lwd.map'
    (f (Lwd.get width))
    (fun ui ->
       Nottui.Ui.size_sensor
         (fun w _ -> if Lwd.peek width <> w then Lwd.set width w)
         (Nottui.Ui.resize ~sw:1 ~sh:1 ~w:0 ui))

let () =
  Nottui.Ui_loop.run (
    Nottui_widgets.h_pane
      (varying_width (Lwd.map (fun width -> P.pretty width doc)))
      (Lwd.pure Nottui.Ui.empty)
  )
