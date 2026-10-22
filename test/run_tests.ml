(* Entry point: force-link every test module (each registers its tests as a
   side effect of module initialization -- see the [init] markers) then
   report a summary. *)

let () = ignore Test_lwd.init
let () = ignore Test_lwd_alg.init
let () = ignore Test_lwd_table.init
let () = ignore Test_lwd_seq.init
let () = ignore Test_lwd_utils.init
let () = exit (Framework.run_all ())
