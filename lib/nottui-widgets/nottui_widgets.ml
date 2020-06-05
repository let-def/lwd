open Lwd_infix
open Lwd.Infix
open Notty
open Nottui

let (!$) x = Lwd.join (Lwd.get x)
let empty_lwd = Lwd.return Ui.empty

let string ?(attr=A.empty) str =
  let control_character_index str i =
    let len = String.length str in
    let i = ref i in
    while let i = !i in i < len && str.[i] >= ' ' do
      incr i;
    done;
    if !i = len then raise Not_found;
    !i
  in
  let rec split str i =
    match control_character_index str i with
    | j ->
      let img = I.string attr (String.sub str i (j - i)) in
      img :: split str (j + 1)
    | exception Not_found ->
      [I.string attr
         (if i = 0 then str
          else String.sub str i (String.length str - i))]
  in
  Ui.atom (I.vcat (split str 0))

let int ?attr x = string ?attr (string_of_int x)
let bool ?attr x = string ?attr (string_of_bool x)
let float_ ?attr x = string ?attr (string_of_float x)

let printf ?attr fmt =
  Printf.ksprintf (string ?attr) fmt

let fmt ?attr fmt =
  Format.kasprintf (string ?attr) fmt

let kprintf k ?attr fmt =
  Printf.ksprintf (fun str -> k (string ?attr str)) fmt

let kfmt k ?attr fmt =
  Format.kasprintf (fun str -> k (string ?attr str)) fmt

let attr_menu_main = A.(bg green ++ fg black)
let attr_menu_sub = A.(bg lightgreen ++ fg black)

let menu_overlay ?dx ?dy handler t =
  let placeholder = Lwd.return (Ui.atom (I.void 1 0)) in
  let body = Lwd_utils.pack Ui.pack_x [placeholder; t; placeholder] in
  let bg = Lwd.map' body @@ fun t ->
    let {Ui. w; h; _} = Ui.layout_spec t in
    Ui.atom (I.char A.(bg lightgreen) ' ' w h)
  in
  Lwd.map (Ui.overlay ?dx ?dy ~handler) (Lwd_utils.pack Ui.pack_z [bg; body])

let scroll_step = 1

type scroll_state = {
  position: int;
  bound : int;
  visible : int;
  total : int;
}

let default_scroll_state = { position = 0; bound = 0; visible = 0; total = 0 }

let vscroll_area ~state ~change t =
  let visible = ref (-1) in
  let total = ref (-1) in
  let scroll state delta =
    let position = state.position + delta in
    let position = max 0 (min state.bound position) in
    if position <> state.position then
      change `Action {state with position};
    `Handled
  in
  let focus_handler state = function
    (*| `Arrow `Left , _ -> scroll (-scroll_step) 0*)
    (*| `Arrow `Right, _ -> scroll (+scroll_step) 0*)
    | `Arrow `Up   , [] -> scroll state (-scroll_step)
    | `Arrow `Down , [] -> scroll state (+scroll_step)
    | `Page `Up, [] -> scroll state ((-scroll_step) * 8)
    | `Page `Down, [] -> scroll state ((+scroll_step) * 8)
    | _ -> `Unhandled
  in
  let scroll_handler state ~x:_ ~y:_ = function
    | `Scroll `Up   -> scroll state (-scroll_step)
    | `Scroll `Down -> scroll state (+scroll_step)
    | _ -> `Unhandled
  in
  Lwd.map2' t state @@ fun t state ->
  t
  |> Ui.scroll_area 0 state.position
  |> Ui.resize ~h:0 ~sh:1
  |> Ui.size_sensor (fun _ h ->
      let tchange =
        if !total <> (Ui.layout_spec t).Ui.h
        then (total := (Ui.layout_spec t).Ui.h; true)
        else false
      in
      let vchange =
        if !visible <> h
        then (visible := h; true)
        else false
      in
      if tchange || vchange then
        change `Content {state with visible = !visible; total = !total;
                                    bound = max 0 (!total - !visible); }
    )
  |> Ui.mouse_area (scroll_handler state)
  |> Ui.keyboard_area (focus_handler state)

let scroll_area ?(offset=0,0) t =
  let offset = Lwd.var offset in
  let scroll d_x d_y =
    let s_x, s_y = Lwd.peek offset in
    let s_x = max 0 (s_x + d_x) in
    let s_y = max 0 (s_y + d_y) in
    Lwd.set offset (s_x, s_y);
    `Handled
  in
  let focus_handler = function
    | `Arrow `Left , [] -> scroll (-scroll_step) 0
    | `Arrow `Right, [] -> scroll (+scroll_step) 0
    | `Arrow `Up   , [] -> scroll 0 (-scroll_step)
    | `Arrow `Down , [] -> scroll 0 (+scroll_step)
    | `Page `Up, [] -> scroll 0 ((-scroll_step) * 8)
    | `Page `Down, [] -> scroll 0 ((+scroll_step) * 8)
    | _ -> `Unhandled
  in
  let scroll_handler ~x:_ ~y:_ = function
    | `Scroll `Up   -> scroll 0 (-scroll_step)
    | `Scroll `Down -> scroll 0 (+scroll_step)
    | _ -> `Unhandled
  in
  Lwd.map2' t (Lwd.get offset) @@ fun t (s_x, s_y) ->
  t
  |> Ui.scroll_area s_x s_y
  |> Ui.mouse_area scroll_handler
  |> Ui.keyboard_area focus_handler

let main_menu_item text f =
  let text = string ~attr:attr_menu_main (" " ^ text ^ " ") in
  let v = Lwd.var empty_lwd in
  let visible = ref false in
  let on_click ~x:_ ~y:_ = function
    | `Left ->
      visible := not !visible;
      if not !visible then (
        v $= Lwd.return Ui.empty
      ) else (
        let h ~x:_ ~y:_ = function
          | `Left ->
            visible := false; v $= Lwd.return Ui.empty; `Unhandled
          | _ -> `Unhandled
        in
        v $= menu_overlay h (f ())
      );
      `Handled
    | _ -> `Unhandled
  in
  Lwd_utils.pack Ui.pack_y [
    Lwd.return (Ui.mouse_area on_click text);
    Lwd.join (Lwd.get v)
  ]

let sub_menu_item text f =
  let text = string ~attr:attr_menu_sub text in
  let v = Lwd.var empty_lwd in
  let visible = ref false in
  let on_click ~x:_ ~y:_ = function
    | `Left ->
      visible := not !visible;
      if not !visible then (
        v $= Lwd.return Ui.empty
      ) else (
        let h ~x:_ ~y:_ = function
          | `Left ->
            visible := false; v $= Lwd.return Ui.empty; `Unhandled
          | _ -> `Unhandled
        in
        v $= menu_overlay h (f ())
      );
      `Handled
    | _ -> `Unhandled
  in
  Lwd_utils.pack Ui.pack_x [
    Lwd.return (Ui.mouse_area on_click text);
    Lwd.join (Lwd.get v)
  ]

let sub_entry text f =
  let text = string ~attr:attr_menu_sub text in
  let on_click ~x:_ ~y:_ = function
    | `Left -> f (); `Handled
    | _ -> `Unhandled
  in
  Ui.mouse_area on_click text

let v_pane left right =
  let w = ref 10 in
  let h = ref 10 in
  let split = ref 0.5 in
  let splitter = Lwd.var empty_lwd in
  let splitter_bg = Lwd.var Ui.empty in
  let left_pane = Lwd.var empty_lwd in
  let right_pane = Lwd.var empty_lwd in
  let node = Lwd_utils.pack Ui.pack_y [!$left_pane; !$splitter; !$right_pane] in
  let render () =
    let split = int_of_float (!split *. float !h) in
    let split = min (!h - 1) (max split 0) in
    left_pane $= Lwd.map' left
      (fun t -> Ui.resize ~w:!w ~h:split t);
    right_pane $= Lwd.map' right
      (fun t -> Ui.resize ~w:!w ~h:(!h - split - 1) t);
    splitter_bg $= Ui.atom (I.char A.(bg lightyellow) ' ' !w 1);
  in
  let action ~x:_ ~y:_ = function
    | `Left ->
      let y0 = int_of_float (!split *. float !h) in
      `Grab ((fun ~x:_ ~y ->
          let y0' = y0 + y in
          split := min 1.0 (max 0.0 (float y0' /. float !h));
          render ()
        ), (fun ~x:_ ~y:_ -> ()))
    | _ -> `Unhandled
  in
  splitter $= Lwd.map (Ui.mouse_area action) (Lwd.get splitter_bg);
  render ();
  let on_resize ew eh =
    if !w <> ew || !h <> eh then (
      w := ew; h := eh;
      render ()
    )
  in
  Lwd.map' node @@ fun t ->
  Ui.size_sensor on_resize (Ui.resize ~w:10 ~h:10 ~sw:1 ~sh:1 t)

let h_pane top bottom =
  let w = ref 10 in
  let h = ref 10 in
  let split = ref 0.5 in
  let splitter = Lwd.var empty_lwd in
  let splitter_bg = Lwd.var Ui.empty in
  let top_pane = Lwd.var empty_lwd in
  let bot_pane = Lwd.var empty_lwd in
  let node = Lwd_utils.pack Ui.pack_x [!$top_pane; !$splitter; !$bot_pane] in
  let render () =
    let split = int_of_float (!split *. float !w) in
    let split = min (!w - 1) (max split 0) in
    top_pane $= Lwd.map' top
      (fun t -> Ui.resize ~w:split ~h:!h t);
    bot_pane $= Lwd.map' bottom
      (fun t -> Ui.resize ~w:(!w - split - 1) ~h:!h t);
    splitter_bg $= Ui.atom (Notty.I.char Notty.A.(bg lightyellow) ' ' 1 !h);
  in
  let action ~x:_ ~y:_ = function
    | `Left ->
      let x0 = int_of_float (!split *. float !w) in
      `Grab ((fun ~x ~y:_ ->
          let x0' = x0 + x in
          split := min 1.0 (max 0.0 (float x0' /. float !w));
          render ()
        ), (fun ~x:_ ~y:_ -> ()))
    | _ -> `Unhandled
  in
  splitter $= Lwd.map (Ui.mouse_area action) (Lwd.get splitter_bg);
  render ();
  let on_resize ew eh =
    if !w <> ew || !h <> eh then (
      w := ew; h := eh;
      render ()
    )
  in
  Lwd.map' node @@ fun t ->
  Ui.size_sensor on_resize (Ui.resize ~w:10 ~h:10 ~sw:1 ~sh:1 t)

let sub' str p l =
  if p = 0 && l = String.length str
  then str
  else String.sub str p l

let edit_field ?(focus=Focus.make()) state ~on_change ~on_submit =
  let update focus_h focus (text, pos) =
    let pos = min (max 0 pos) (String.length text) in
    let content =
      Ui.atom @@ I.hcat @@
      if Focus.has_focus focus then (
        let attr = A.(bg lightblue) in
        let len = String.length text in
        (if pos >= len
         then [I.string attr text]
         else [I.string attr (sub' text 0 pos)])
        @
        (if pos < String.length text then
           [I.string A.(bg lightred) (sub' text pos 1);
            I.string attr (sub' text (pos + 1) (len - pos - 1))]
         else [I.string A.(bg lightred) " "]);
      ) else
        [I.string A.(st underline) (if text = "" then " " else text)]
    in
    let handler = function
      | `ASCII 'U', [`Ctrl] -> on_change ("", 0); `Handled (* clear *)
      | `Escape, [] -> Focus.release focus_h; `Handled
      | `ASCII k, _ ->
        let text =
          if pos < String.length text then (
            String.sub text 0 pos ^ String.make 1 k ^
            String.sub text pos (String.length text - pos)
          ) else (
            text ^ String.make 1 k
          )
        in
        on_change (text, (pos + 1));
        `Handled
      | `Backspace, _ ->
        let text =
          if pos > 0 then (
            if pos < String.length text then (
              String.sub text 0 (pos - 1) ^
              String.sub text pos (String.length text - pos)
            ) else if String.length text > 0 then (
              String.sub text 0 (String.length text - 1)
            ) else text
          ) else text
        in
        let pos = max 0 (pos - 1) in
        on_change (text, pos);
        `Handled
      | `Enter, _ -> on_submit (text, pos); `Handled
      | `Arrow `Left, [] ->
        let pos = min (String.length text) pos in
        if pos > 0 then (
          on_change (text, pos - 1);
          `Handled
        )
        else `Unhandled
      | `Arrow `Right, [] ->
        let pos = pos + 1 in
        if pos <= String.length text
        then (on_change (text, pos); `Handled)
        else `Unhandled
      | _ -> `Unhandled
    in
    Ui.keyboard_area ~focus handler content
  in
  let node =
    Lwd.map2 (update focus) (Focus.status focus) state
  in
  let mouse_grab (text, pos) ~x ~y:_ = function
    | `Left ->
      if x <> pos then on_change (text, x);
      Nottui.Focus.request focus;
      `Handled
    | _ -> `Unhandled
  in
  Lwd.map2' state node @@ fun state content ->
  Ui.mouse_area (mouse_grab state) content

(** Tab view, where exactly one element of [l] is shown at a time. *)
let tabs (tabs: (string * (unit -> Ui.t Lwd.t)) list) : Ui.t Lwd.t =
  match tabs with
  | [] -> Lwd.return Ui.empty
  | _ ->
    let cur = Lwd.var 0 in
    Lwd.get cur >>= fun idx_sel ->
    let _, f = List.nth tabs idx_sel in
    let tab_bar =
      tabs
      |> List.mapi
        (fun i (s,_) ->
           let attr = if i = idx_sel then A.(st underline) else A.empty in
           let tab_annot = printf ~attr "[%s]" s in
           Ui.mouse_area
             (fun ~x:_ ~y:_ l -> if l=`Left then (Lwd.set cur i; `Handled) else `Unhandled)
             tab_annot)
      |> Ui.hcat
    in
    f() >|= Ui.join_y tab_bar

(** Horizontal/vertical box. We fill lines until there is no room,
    and then go to the next ligne. All widgets in a line are considered to
    have the same height.
    @param width dynamic width  (default 80)
*)
let flex_box ?(w=Lwd.return 80) (l: Ui.t Lwd.t list) : Ui.t Lwd.t =
  Lwd_utils.flatten_l l >>= fun l ->
  w >|= fun w_limit ->
  let rec box_render (acc:Ui.t) (i:int) l : Ui.t =
    match l with
    | [] -> acc
    | ui0 :: tl ->
      let w0 = (Ui.layout_spec ui0).Ui.w in
      if i + w0 >= w_limit then (
        (* newline starting with ui0 *)
        Ui.join_y acc (box_render ui0 w0 tl)
      ) else (
        (* same line *)
        box_render (Ui.join_x acc ui0) (i+w0) tl
      )
  in
  box_render Ui.empty 0 l


(** Prints the summary, but calls [f()] to compute a sub-widget
    when clicked on. Useful for displaying deep trees. *)
let unfoldable ?(folded_by_default=true) summary (f: unit -> Ui.t Lwd.t) : Ui.t Lwd.t =
  let open Lwd.Infix in
  let opened = Lwd.var (not folded_by_default) in
  let fold_content =
    Lwd.get opened >>= function
    | true ->
      (* call [f] and pad a bit *)
      f() |> Lwd.map (Ui.join_x (string " "))
    | false -> empty_lwd
  in
  (* pad summary with a "> " when it's opened *)
  let summary =
    Lwd.get opened >>= fun op ->
    summary >|= fun s ->
    Ui.hcat [string ~attr:A.(bg blue) (if op then "v" else ">"); string " "; s]
  in
  let cursor ~x:_ ~y:_ = function
     | `Left when Lwd.peek opened -> Lwd.set opened false; `Handled
     | `Left -> Lwd.set opened true; `Handled
     | _ -> `Unhandled
  in
  let mouse = Lwd.map (fun m -> Ui.mouse_area cursor m) summary in
  Lwd.map2
    (fun summary fold ->
      (* TODO: make this configurable/optional *)
      (* newline if it's too big to fit on one line nicely *)
      let spec_sum = Ui.layout_spec summary in
      let spec_fold = Ui.layout_spec fold in
      (* TODO: somehow, probe for available width here? *)
      let too_big =
        spec_fold.Ui.h > 1 ||
        (spec_fold.Ui.h>0 && spec_sum.Ui.w + spec_fold.Ui.w > 60)
      in
      if too_big
      then Ui.join_y summary (Ui.join_x (string " ") fold)
      else Ui.join_x summary fold)
    mouse fold_content

let hbox l = Lwd_utils.pack Ui.pack_x l
let vbox l = Lwd_utils.pack Ui.pack_y l
let zbox l = Lwd_utils.pack Ui.pack_z l

let vlist ?(bullet="- ") (l: Ui.t Lwd.t list) : Ui.t Lwd.t =
  l
  |> List.map (fun ui -> Lwd.map (Ui.join_x (string bullet)) ui)
  |> Lwd_utils.pack Ui.pack_y

(** A list of items with a dynamic filter on the items *)
let vlist_with
    ?(bullet="- ")
    ?(filter=Lwd.return (fun _ -> true))
    (f:'a -> Ui.t Lwd.t)
    (l:'a list Lwd.t) : Ui.t Lwd.t =
  let open Lwd.Infix in
  let rec filter_map_ acc f l =
    match l with
    | [] -> List.rev acc
    | x::l' ->
      let acc' = match f x with | None -> acc | Some y -> y::acc in
      filter_map_ acc' f l'
  in
  let l = l >|= List.map (fun x -> x, Lwd.map (Ui.join_x (string bullet)) @@ f x) in
  let l_filter : _ list Lwd.t =
    filter >>= fun filter ->
    l >|=
    filter_map_ []
      (fun (x,ui) -> if filter x then Some ui else None)
  in
  l_filter >>= Lwd_utils.pack Ui.pack_y

let rec iterate n f x =
  if n=0 then x else iterate (n-1) f (f x)

(** A grid layout, with alignment in all rows/columns.
    @param max_h maximum height of a cell
    @param max_w maximum width of a cell
    @param bg attribute for controlling background style
    @param h_space horizontal space between each cell in a row
    @param v_space vertical space between each row
    @param fill used to control filling of cells
    @param crop used to control cropping of cells
    TODO: control padding/alignment, vertically and horizontally
    TODO: control align left/right in cells
    TODO: horizontal rule below headers
    TODO: headers *)
let grid
    ?max_h ?max_w
    ?fill ?crop ?bg
    ?(h_space=0)
    ?(v_space=0)
    ?(headers:Ui.t Lwd.t list option)
    (rows: Ui.t Lwd.t list list) : Ui.t Lwd.t =
  let rows = match headers with
    | None -> rows
    | Some r -> r :: rows
  in
  (* build a [ui list list Lwd.t] *)
  begin
    Lwd_utils.map_l (fun r -> Lwd_utils.flatten_l r) rows
  end >>= fun (rows:Ui.t list list) ->
  (* determine width of each column and height of each row *)
  let n_cols = List.fold_left (fun n r -> max n (List.length r)) 0 rows in
  let col_widths = Array.make n_cols 1 in
  List.iter
    (fun row ->
       List.iteri
         (fun col_j cell ->
           let w = (Ui.layout_spec cell).Ui.w in
           col_widths.(col_j) <- max col_widths.(col_j) w)
         row)
    rows;
  begin match max_w with
    | None -> ()
    | Some max_w ->
      (* limit width *)
      Array.iteri (fun i x -> col_widths.(i) <- min x max_w) col_widths
  end;
  (* now render, with some padding *)
  let pack_pad_x =
    if h_space<=0 then (Ui.empty, Ui.join_x)
    else (Ui.empty, (fun x y -> Ui.hcat [x; Ui.void h_space 0; y]))
  and pack_pad_y =
    if v_space =0 then (Ui.empty, Ui.join_y)
    else (Ui.empty, (fun x y -> Ui.vcat [x; Ui.void v_space 0; y]))
  in
  let rows =
    List.map
      (fun row ->
         let row_h =
           List.fold_left (fun n c -> max n (Ui.layout_spec c).Ui.h) 0 row
         in
         let row_h = match max_h with
           | None -> row_h
           | Some max_h -> min row_h max_h
         in
         let row =
           List.mapi
             (fun i c ->
                Ui.resize ~w:col_widths.(i) ~h:row_h ?crop ?fill ?bg c)
             row
         in
         Lwd_utils.pure_pack pack_pad_x row)
      rows
  in
  (* TODO: mouse and keyboard handling *)
  let ui = Lwd_utils.pure_pack pack_pad_y rows in
  Lwd.return ui

let button ?attr s f =
  Ui.mouse_area (fun ~x:_ ~y:_ _ -> f(); `Handled) (string ?attr s)


(* file explorer for selecting a file *)
let file_select
    ?(abs=false)
    ?filter
    ~(on_select:string -> unit) () : Ui.t Lwd.t =
  let rec aux ~fold path =
    try
      let p_rel = if path = "" then "." else path in
      if Sys.is_directory p_rel then (
        let ui() =
          let arr = Sys.readdir p_rel in
          let l = Array.to_list arr |> List.map (Filename.concat path) in
          (* apply potential filter *)
          let l = match filter with None -> l | Some f -> List.filter f l in
          let l = Lwd.return @@ List.sort String.compare l in
          vlist_with ~bullet:"" (aux ~fold:true) l
        in
        if fold then (
          unfoldable ~folded_by_default:true
            (Lwd.return @@ string @@ path ^ "/") ui
        ) else ui ()
      ) else (
        Lwd.return @@
        button ~attr:A.(st underline) path (fun () -> on_select path)
      )
    with e ->
      Lwd.return @@ Ui.vcat [
        printf ~attr:A.(bg red) "cannot list directory %s" path;
        string @@ Printexc.to_string e;
      ]
  in
  let start = if abs then Sys.getcwd () else "" in
  aux ~fold:false start

let toggle, toggle' =
  let toggle_ st (lbl:string Lwd.t) (f:bool -> unit) : Ui.t Lwd.t =
    let mk_but st_v lbl_v =
      let lbl = Printf.sprintf "[%s|%s]" lbl_v (if st_v then "✔" else "×");in
      button lbl (fun () ->
          let new_st = not st_v in
          Lwd.set st new_st; f new_st)
    in
    Lwd.map2 mk_but (Lwd.get st) lbl
  in
  (** Similar to {!toggle}, except it directly reflects the state of a variable. *)
  let toggle' (lbl:string Lwd.t) (v:bool Lwd.var) : Ui.t Lwd.t =
    toggle_ v lbl (Lwd.set v)
  (** a toggle, with a true/false state *)
  and toggle ?(init=false) (lbl:string Lwd.t) (f:bool -> unit) : Ui.t Lwd.t =
    let st = Lwd.var init in
    toggle_ st lbl f
  in
  toggle, toggle'

