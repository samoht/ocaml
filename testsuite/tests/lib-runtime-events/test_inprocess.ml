(* TEST
 include runtime_events;
 set OCAML_RUNTIME_EVENTS_INPROCESS = "1";
 set OCAML_RUNTIME_EVENTS_START = "1";
*)

(* The in-process ring (OCAML_RUNTIME_EVENTS_INPROCESS) has no backing file; the
   in-process consumer must still read events from it. *)

let got_start = ref false

let lifecycle _domain_id _ts lifecycle_event _data =
  match lifecycle_event with
  | Runtime_events.EV_RING_START -> got_start := true
  | _ -> ()

let () =
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create ~lifecycle () in
  let _ = Runtime_events.read_poll cursor callbacks None in
  assert !got_start;
  (* no <pid>.events backing file should exist for the in-process ring *)
  Array.iter
    (fun f -> assert (not (Filename.check_suffix f ".events")))
    (Sys.readdir ".")
