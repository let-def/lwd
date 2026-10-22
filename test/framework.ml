(* Minimal, dependency-light test harness shared by all test_*.ml files.

   Each test is a name + thunk. Failures are collected (not fatal), so one
   broken test doesn't hide the results of the others. [run_all] prints a
   summary and returns a suitable process exit code. *)

type outcome =
  | Pass
  | Fail of string

let results : (string * outcome) list ref = ref []

let test name f =
  let outcome =
    try f (); Pass with
    | Assert_failure (file, line, col) ->
      Fail (Printf.sprintf "assertion failed at %s:%d:%d" file line col)
    | Failure msg -> Fail msg
    | exn -> Fail (Printexc.to_string exn)
  in
  results := (name, outcome) :: !results

(* Run [f ()] under a wall-clock deadline using SIGALRM. Returns [`Timeout]
   if the deadline is hit, [`Result r] otherwise. Used to pin down behaviors
   that are expected to hang or run away (e.g. unbounded [fix] loops, or
   cyclic graphs) without ever letting the test suite itself hang. *)
let with_timeout seconds f =
  let timed_out = ref false in
  let old_handler =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> timed_out := true; raise Exit))
  in
  let old_alarm = Unix.alarm seconds in
  let finish () =
    ignore (Unix.alarm old_alarm);
    Sys.set_signal Sys.sigalrm old_handler
  in
  match f () with
  | result -> finish (); `Result result
  | exception Exit when !timed_out -> finish (); `Timeout
  | exception exn -> finish (); raise exn

let check ?(msg = "") name cond =
  test name (fun () -> if not cond then failwith (if msg = "" then "condition false" else msg))

let check_eq (type a) name (pp : a -> string) (expected : a) (actual : a) =
  test name (fun () ->
      if expected <> actual then
        failwith (Printf.sprintf "expected %s, got %s" (pp expected) (pp actual)))

let check_raises name f =
  test name (fun () ->
      match f () with
      | _ -> failwith "expected an exception, none was raised"
      | exception _ -> ())

let qcheck_test ?(seed = 1234) name t =
  test name (fun () ->
      QCheck.Test.check_exn ~rand:(Random.State.make [| seed |]) t)

let section name f =
  Printf.printf "--- %s ---\n%!" name;
  f ()

let run_all () =
  let entries = List.rev !results in
  let failed = List.filter (fun (_, o) -> o <> Pass) entries in
  List.iter
    (fun (name, outcome) ->
       match outcome with
       | Pass -> Printf.printf "PASS  %s\n" name
       | Fail msg -> Printf.printf "FAIL  %s\n      %s\n" name msg)
    entries;
  Printf.printf "\n%d/%d tests passed\n%!"
    (List.length entries - List.length failed)
    (List.length entries);
  if failed = [] then 0 else 1
