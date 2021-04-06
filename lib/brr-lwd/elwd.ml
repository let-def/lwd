open Brr
open El

type t = El.t

type 'a col = [
  | `P of 'a
  (** Pure element *)
  | `R of 'a Lwd.t
  (** Reactive element *)
  | `S of 'a Lwd_seq.t Lwd.t
  (** Reactive sequence of elements *)
] list
(** Describing collections of elements *)

let is_pure_element = function
  | `P _ -> true
  | `R x -> Option.is_some (Lwd.is_pure x)
  | `S x -> Option.is_some (Lwd.is_pure x)

let extract_pure_element x = Option.get (Lwd.is_pure x)

let extract_pure_elements xs =
  List.flatten (
    List.map (function
        | `P x -> [x]
        | `R x -> [extract_pure_element x]
        | `S x -> Lwd_seq.to_list (extract_pure_element x)
      ) xs
  )

let consume_attribs : _ col -> _ = function
  | [] -> [], []
  | attribs ->
    let pure, impure = List.partition is_pure_element attribs in
    extract_pure_elements pure, impure

(** Reactive sequence of elements *)

let consume_children = function
  | [] -> [], None
  | [`P x] -> [x], None
  | [`S x] -> [], Some x
  | [`R x] -> [], Some (Lwd.map ~f:Lwd_seq.element x)
  | col ->
    if List.for_all is_pure_element col
    then
      List.flatten (
        List.map (function
            | `P x -> [x]
            | `R x -> [extract_pure_element x]
            | `S x -> Lwd_seq.to_list (extract_pure_element x)
          )
          col
      ), None
    else [], Some (
        Lwd_utils.map_reduce (function
            | `P x -> Lwd.pure (Lwd_seq.element x)
            | `R x -> Lwd.map ~f:Lwd_seq.element x
            | `S x -> x
          ) Lwd_seq.lwd_monoid
          col
      )

type child_tree =
  | Leaf of El.t
  | Inner of { mutable bound: Jv.t;
               left: child_tree; right: child_tree; }

let child_node node = Leaf node

let child_join left right = Inner { bound = Jv.null; left; right }

let jv_parentNode = Jstr.v "parentNode"
let jv_nextSibling = Jstr.v "nextSibling"
let jv_append = Jstr.v "append"
let jv_before = Jstr.v "before"
let jv_remove = Jstr.v "remove"
let jv_contains = Jstr.v "contains"

let jv_toRemove =
  Jstr.v "lwd-to-remove" (* HACK Could be turned into a Javascript symbol *)

let contains_focus node =
  match Brr.Document.active_el (Brr.El.document node) with
  | None -> false
  | Some el ->
    Jv.to_bool (Jv.call' (El.to_jv node) jv_contains [|El.to_jv el|])

let update_children
    (self : El.t)
    (children : El.t Lwd_seq.t Lwd.t) : El.t Lwd.t =
  let reducer =
    ref (Lwd_seq.Reducer.make ~map:child_node ~reduce:child_join)
  in
  Lwd.map children ~f:begin fun children ->
    let dropped, reducer' =
      Lwd_seq.Reducer.update_and_get_dropped !reducer children in
    reducer := reducer';
    let schedule_for_removal child () = match child with
      | Leaf node -> Jv.set' (El.to_jv node) jv_toRemove Jv.true';
      | Inner _ -> ()
    in
    Lwd_seq.Reducer.fold_dropped `Map schedule_for_removal dropped ();
    let preserve_focus = contains_focus self in
    begin match Lwd_seq.Reducer.reduce reducer' with
      | None -> ()
      | Some tree ->
        let rec update acc = function
          | Leaf node ->
            let node' = El.to_jv node in
            Jv.delete' node' jv_toRemove;
            (*Brr.Console.log ["Updating "; node];*)
            if Jv.get' node' jv_parentNode != El.to_jv self then (
              if Jv.is_null acc
              then ignore (Jv.call' (El.to_jv self) jv_append [|node'|])
              else ignore (Jv.call' acc jv_before [|node'|])
            ) else if preserve_focus && contains_focus node then (
              let rec shift_siblings () =
                let sibling = Jv.get' node' jv_nextSibling in
                if sibling == acc then true
                else if Jv.is_null sibling then false
                else (
                  ignore (Jv.call' node' jv_before [|sibling|]);
                  shift_siblings ()
                )
              in
              if not (shift_siblings ()) then
                ignore (Jv.call' acc jv_before [|node'|]);
            ) else if Jv.get' node' jv_nextSibling != acc then
              ignore (Jv.call' acc jv_before [|node'|]);
            node'
          | Inner t ->
            if Jv.is_null t.bound then (
              let acc = update acc t.right in
              let acc = update acc t.left in
              t.bound <- acc;
              acc
            ) else
              t.bound
        in
        ignore (update Jv.null tree)
    end;
    let remove_child child () = match child with
      | Leaf node ->
        let node = El.to_jv node in
        if Jv.is_some (Jv.get' node jv_toRemove) then (
          (*Brr.Console.log ["Removing "; node];*)
          Jv.delete' node jv_toRemove;
          ignore (Jv.call' node jv_remove [||])
        )
      | Inner _ -> ()
    in
    Lwd_seq.Reducer.fold_dropped `Map remove_child dropped ();
    self
  end

let pure_unit = Lwd.pure ()

let attach_attribs el attribs =
  let set_at at =
    let k, v = At.to_pair at in
    El.set_at k (Some v) el
  in
  Lwd_utils.map_reduce (function
      | `P at -> set_at at; pure_unit
      | `R at -> Lwd.map ~f:set_at at
      | `S ats -> Lwd.map ~f:ignore (Lwd_seq.map set_at ats)
    ) (pure_unit, fun _ _ -> pure_unit)
    attribs

let v ?d ?(at=[]) tag children =
  let at, impure_at = consume_attribs at in
  let children, impure_children = consume_children children in
  let el = El.v ?d ~at tag children in
  match impure_at, impure_children with
  | [], None -> Lwd.pure el
  | [], Some children ->
    update_children el children
  | at, None ->
    Lwd.map ~f:(fun () -> el) (attach_attribs el at)
  | at, Some children ->
    Lwd.map2 ~f:(fun () el -> el)
      (attach_attribs el at)
      (update_children el children)

(** {1:els Element constructors} *)

type cons =  ?d:El.document -> ?at:At.t col -> t col -> t Lwd.t
(** The type for element constructors. This is simply {!v} with a
    pre-applied element name. *)

type void_cons = ?d:El.document -> ?at:At.t col -> unit -> t Lwd.t
(** The type for void element constructors. This is simply {!v}
    with a pre-applied element name and without children. *)

let cons name ?d ?at cs = v ?d ?at name cs
let void_cons name ?d ?at () = v ?d ?at name []

let a = cons Name.a
let abbr = cons Name.abbr
let address = cons Name.address
let area = void_cons Name.area
let article = cons Name.article
let aside = cons Name.aside
let audio = cons Name.audio
let b = cons Name.b
let base = void_cons Name.base
let bdi = cons Name.bdi
let bdo = cons Name.bdo
let blockquote = cons Name.blockquote
let body = cons Name.body
let br = void_cons Name.br
let button = cons Name.button
let canvas = cons Name.canvas
let caption = cons Name.caption
let cite = cons Name.cite
let code = cons Name.code
let col = void_cons Name.col
let colgroup = cons Name.colgroup
let command = cons Name.command
let datalist = cons Name.datalist
let dd = cons Name.dd
let del = cons Name.del
let details = cons Name.details
let dfn = cons Name.dfn
let div = cons Name.div
let dl = cons Name.dl
let dt = cons Name.dt
let em = cons Name.em
let embed = void_cons Name.embed
let fieldset = cons Name.fieldset
let figcaption = cons Name.figcaption
let figure = cons Name.figure
let footer = cons Name.footer
let form = cons Name.form
let h1 = cons Name.h1
let h2 = cons Name.h2
let h3 = cons Name.h3
let h4 = cons Name.h4
let h5 = cons Name.h5
let h6 = cons Name.h6
let head = cons Name.head
let header = cons Name.header
let hgroup = cons Name.hgroup
let hr = void_cons Name.hr
let html = cons Name.html
let i = cons Name.i
let iframe = cons Name.iframe
let img = void_cons Name.img
let input = void_cons Name.input
let ins = cons Name.ins
let kbd = cons Name.kbd
let keygen = cons Name.keygen
let label = cons Name.label
let legend = cons Name.legend
let li = cons Name.li
let link = void_cons Name.link
let map = cons Name.map
let mark = cons Name.mark
let menu = cons Name.menu
let meta = void_cons Name.meta
let meter = cons Name.meter
let nav = cons Name.nav
let noscript = cons Name.noscript
let object' = cons Name.object'
let ol = cons Name.ol
let optgroup = cons Name.optgroup
let option = cons Name.option
let output = cons Name.output
let p = cons Name.p
let param = void_cons Name.param
let pre = cons Name.pre
let progress = cons Name.progress
let q = cons Name.q
let rp = cons Name.rp
let rt = cons Name.rt
let ruby = cons Name.ruby
let s = cons Name.s
let samp = cons Name.samp
let script = cons Name.script
let section = cons Name.section
let select = cons Name.select
let small = cons Name.small
let source = void_cons Name.source
let span = cons Name.span
let strong = cons Name.strong
let style = cons Name.style
let sub = cons Name.sub
let summary = cons Name.summary
let sup = cons Name.sup
let table = cons Name.table
let tbody = cons Name.tbody
let td = cons Name.td
let textarea = cons Name.textarea
let tfoot = cons Name.tfoot
let th = cons Name.th
let thead = cons Name.thead
let time = cons Name.time
let title = cons Name.title
let tr = cons Name.tr
let track = void_cons Name.track
let u = cons Name.u
let ul = cons Name.ul
let var = cons Name.var
let video = cons Name.video
let wbr = void_cons Name.wbr
