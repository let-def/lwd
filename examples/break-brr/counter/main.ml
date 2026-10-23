open Brr
open Brr_lwd

let table = Lwd_table.make ()
let count = Lwd_table.map_reduce (fun _row _v -> 1) (0, ( + )) table

let ui =
  Elwd.input
    ~at:
      [
        `P (At.type' (Jstr.v "button"));
        `R (Lwd.map count ~f:(fun i -> At.value (Jstr.of_int i)));
      ]
    ~ev:[ `P (Elwd.handler Ev.click (fun _ -> Lwd_table.append' table ())) ]
    ()

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    ignore @@ G.request_animation_frame
    @@ fun _ -> ignore @@ Lwd.quick_sample ui
  in
  let on_load _ =
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate
  in
  ignore @@ Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

let () =
  let f i = Console.log [ i ] in
  let root = Lwd.observe count in
  Lwd.set_on_invalidate root (fun _ -> f (Lwd.quick_sample root));
  let first_sample = Lwd.quick_sample root in
  f first_sample
