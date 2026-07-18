open Brr
open Brr_lwd

let ui =
  let values = Lwd_table.make () in
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
    let row = Lwd_table.append values in
    Lwd.map (Elwd.input ()) ~f:(fun el ->
        ignore (
          Ev.listen Ev.input (fun _ ->
            let txt = Jstr.to_string (El.prop El.Prop.value el) in
            Console.log ["shuffle"; txt];
            Lwd_table.set row txt;
            shuffle ()
          ) (El.as_target el)
        );
        el
      )
  in
  Lwd.set items (Lwd_seq.of_array (Array.init 10 edit));
  let values =
    Lwd_table.map_reduce
      (fun _row txt -> Lwd_seq.element (txt ^ "\n"))
      (Lwd_seq.monoid)
      values
    |> Lwd_seq.sort_uniq String.compare
  in
  Elwd.div [
    `P (El.txt' "In this test, typing in one of the input field should \
                 shuffle them. The test succeeds if focus and selections are \
                 preserved after shuffling.");
    `P (El.br ());
    `S (Lwd_seq.lift (Lwd.get items));
    `S (Lwd_seq.map El.txt' values);
  ]

let defer_after_dom_loading f =
  let on_load _ = f () in
  ignore (Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window));
  ()

let () =
  defer_after_dom_loading @@ fun () ->
  match El.find_first_by_selector (Jstr.v "#main") with
  | None -> failwith "#main could not be found, check your html"
  | Some main ->
    let _remove_token = Elwd.insert_sibling `Replace main ui in
    ()
