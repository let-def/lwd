open Brr
open Brr_lwd

let accordion ~name ~title content =
  (* TODO there might a bug in lwd: the open attribute disapears.
      This does not happen if we use an Elwd.details *)
  let at =[
    At.name (Jstr.v name);
    At.v (Jstr.v "open") (Jstr.v "true") ]
  in
  El.details ~at
    ((El.summary [ title ]) :: [ (El.section content) ])

let score = Lwd.var "Notes"

let ui =
  let e = Lwd.map (Lwd.get score) ~f:(fun score ->
    accordion ~name:"test-acc" ~title:(El.txt' "Bandonéon" )
      [El.txt' score])
  in
  Elwd.div [ `R e]

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    ignore @@ G.request_animation_frame
    @@ fun _ -> ignore @@ Lwd.quick_sample ui
  in
  let on_load _ =
    El.append_children (Document.body G.document) [ Lwd.quick_sample ui ];
    Lwd.set_on_invalidate ui on_invalidate;
    (* This triggers the bug, the text gets replaced but the <details> element
       loses it's "open" attirbute. *)
    Lwd.set score "Plus de notes"
  in
  ignore @@ Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)

