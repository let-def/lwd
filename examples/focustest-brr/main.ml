open Brr
open Brr_lwd

let ui =
  let items = Lwd.var Lwd_seq.empty in
  let shuffle () =
    let all = Lwd_seq.to_array (Lwd.peek items) in
    for i = Array.length all - 1 downto 1 do
      let i' = Random.int (i + 1) in
      let x = all.(i) in
      let x' = all.(i') in
      all.(i') <- x;
      all.(i) <- x';
    done;
    Lwd.set items (Lwd_seq.of_array all)
  in
  let edit _ =
    Lwd.map (Elwd.input ()) ~f:(fun el ->
        Ev.listen Ev.input
          (fun _ -> Console.log ["shuffle"]; shuffle ())
          (El.as_target el);
        el
      )
  in
  Lwd.set items (Lwd_seq.of_array (Array.init 10 edit));
  Elwd.div [
    `P (El.txt' "In this test, typing in one of the input field should \
                 shuffle them. The test succeeds if focus and selections are \
                 preserved after shuffling.");
    `P (El.br ());
    `S (Lwd_seq.lift (Lwd.get items))
  ]

let () =
  let ui = Lwd.observe ui in
  let on_invalidate _ =
    Console.(log [str "on invalidate"]);
    let _ : int =
      G.request_animation_frame @@ fun _ ->
      let _ui = Lwd.quick_sample ui in
      (*El.set_children (Document.body G.document) [ui]*)
      ()
    in
    ()
  in
  let on_load _ =
    Console.(log [str "onload"]);
    El.append_children (Document.body G.document) [Lwd.quick_sample ui];
    Lwd.set_on_invalidate ui on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window);
  ()
