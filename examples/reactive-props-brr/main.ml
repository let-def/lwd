open Brr
open Brr_lwd
open Lwd_infix

(* A string input, whose value is synced with the given var *)
let string var =
  let h =
   fun ev ->
    let el = ev |> Brr.Ev.target |> Brr.Ev.target_to_jv in
    let new_value = Jv.get el "value" |> Jv.to_string in
    Brr.Console.(log [ new_value ]);
    Lwd.set var new_value
  in
  let ev = [ `P (Brr_lwd.Elwd.handler Brr.Ev.input h) ] in
  let at =
    let value =
      let$ v = Lwd.get var in
      Brr.At.value (Jstr.of_string v)
    in
    [ `R value; `P (Brr.At.type' (Jstr.v "text")) ]
  in
  Brr_lwd.Elwd.input ~at ~ev ()

(* A checkbox, whose checked prop is synced the reactive [check s] *)
let checkbox s check description =
  let checked =
    let$ s = s in
    (Jstr.v "checked", check s |> Jv.of_bool)
  in
  let checkbox_el =
    Elwd.input
      ~at:[ `P (At.type' (Jstr.v "checkbox")) ]
      ~st:[ `P (Jstr.v "pointer-events", Jstr.v "none") ]
      ~pr:[ `R checked ] ()
  in
  Elwd.div [ `R checkbox_el; `P (El.txt' description) ]

(* A string input, and checks whether it's long enough, contains uppercase,
   contains numbers *)
let ui =
  let s = Lwd.var "" in
  let string_input = string s in
  let s = Lwd.get s in
  let checkbox_length =
    let check s = String.length s >= 5 in
    checkbox s check "Has length at least 5"
  in
  let checkbox_uppercase =
    let check s = String.exists (function 'A' .. 'Z' -> true | _ -> false) s in
    checkbox s check "Has uppercase"
  in
  let checkbox_numbers =
    let check s = String.exists (function '0' .. '9' -> true | _ -> false) s in
    checkbox s check "Has numbers"
  in
  Elwd.div [ `R string_input ; `R checkbox_length ; `R checkbox_numbers ; `R checkbox_uppercase ]

(* Injection of reactive element to the DOM *)
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
