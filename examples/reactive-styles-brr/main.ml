open Brr
open Brr_lwd
open Lwd_infix

let float var =
  let h =
   fun ev ->
    let el = ev |> Brr.Ev.target |> Brr.Ev.target_to_jv in
    let new_value = Jv.get el "value" |> Jv.to_string |> float_of_string in
    Brr.Console.(log [ new_value ]);
    Lwd.set var new_value
  in
  let ev = [ `P (Brr_lwd.Elwd.handler Brr.Ev.input h) ] in
  let at =
    let v =
      let$ v = Lwd.get var in
      Brr.At.value (Jstr.of_float v)
    in
    [ `R v; `P (Brr.At.type' (Jstr.v "number")) ]
  in
  Brr_lwd.Elwd.input ~at ~ev ()

let ui =
  let top = Lwd.var 1.0 in
  let set_top = float top in
  let left = Lwd.var 1.0 in
  let set_left = float left in
  let block =
    let top =
      let$ top = Lwd.get top in
      let top = Jstr.append (Jstr.of_float top) (Jstr.v "px") in
      (El.Style.top, top)
    in
    let left =
      let$ left = Lwd.get left in
      let left = Jstr.append (Jstr.of_float left) (Jstr.v "px") in
      (El.Style.left, left)
    in
    let st =
      [
        `R top;
        `R left;
        `P (El.Style.width, Jstr.v "50px");
        `P (El.Style.height, Jstr.v "50px");
        `P (El.Style.position, Jstr.v "absolute");
        `P (El.Style.background_color, Jstr.v "red");
      ]
    in
    Elwd.div ~st []
  in
  let playground =
    Elwd.div
      ~st:
        [
          `P (El.Style.width, Jstr.v "1000px");
          `P (El.Style.height, Jstr.v "1000px");
          `P (El.Style.position, Jstr.v "relative");
        ]
      [ `R block ]
  in
  Elwd.div [ `R set_top; `R set_left; `R playground ]

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    Console.(log [ str "on invalidate" ]);
    let _ : int =
      G.request_animation_frame @@ fun _ ->
      let _ui = Lwd.quick_sample ui in
      ()
    in
    ()
  in
  let on_load _ =
    Console.(log [ str "onload" ]);
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window));
  ()
