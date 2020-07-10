open Lwt
open Lwt.Syntax
open TrackOClock.Toggl

(* Create the authenticated client using the token in environment *)
let username = Sys.getenv "toggl_token"
let password = "api_token"

module Client = TrackOClock.Toggl.Client(struct let auth = Auth.Basic {username ; password} end)
open Api(Client)

let get_or_failwith = function
  | Ok x -> x
  | Error e -> Piaf.Error.to_string e |> failwith

let client = Client.create @@ Uri.of_string "https://api.toggl.com" >|= get_or_failwith

(* Utility functions for writing tests *)
let wait value =
  let* _ = Lwt_unix.sleep 0.5 in
  return value

let get_workspace _switch () =
  client
  >>= Workspace.list
  >|= get_or_failwith
  >|= List.filter (fun ({name ; _}: Types.workspace) -> name = "Personal")
  >|= List.hd

let get_project _switch ({id; _}: Types.workspace) =
  client
  >>= Project.list id
  >|= get_or_failwith
  >|= List.hd

let delete_time_entry _switch (time_entry: Types.time_entry) =
  client
  >>= TimeEntry.delete time_entry.id
  >>= wait

let create_time_entry switch ({id; _}: Types.project) =
  let time_entry = client
    >>= TimeEntry.create (Types.create_time_entry
                            ~pid:id
                            ~description:"Test time entry"
                            ~tags:["foo"]
                            ~billable:false
                            ~start:(Types.datetime_of_string "\"2020-01-01T00:00:00Z\"")
                            ~duration:(Some 5)
                            ())
    >|= get_or_failwith
    >>= wait
  in
  Lwt_switch.add_hook (Some switch) (fun () -> time_entry >>= delete_time_entry switch >|= ignore);
  time_entry

let stop_time_entry _switch (time_entry: Types.time_entry) =
  client
  >>= TimeEntry.stop time_entry.id
  >|= get_or_failwith
  >>= wait

let start_time_entry switch ({id; wid; _}: Types.project) =
  let time_entry = client
    >>= TimeEntry.start (Types.create_time_entry ~wid ~pid:id ~description:"Test time entry" ())
    >|= get_or_failwith
    >>= wait
  in
  Lwt_switch.add_hook (Some switch) (fun () -> time_entry >>= delete_time_entry switch >|= ignore);
  time_entry

let get_time_entry _switch (time_entry: Types.time_entry) =
  client
  >>= TimeEntry.details time_entry.id
  >|= get_or_failwith
  >>= wait

let get_current_time_entry _switch () =
  client
  >>= TimeEntry.current
  >|= get_or_failwith
  >>= wait

let list_time_entries ?start_date ?end_date switch () =
  client
  >>= TimeEntry.list ?start_date ?end_date
  >|= get_or_failwith
  |> both (get_workspace switch ())
  >|= (function workspace, time_entries -> List.filter (fun (te: Types.time_entry) -> te.wid = workspace.id) time_entries)
  >>= wait

(* Tests *)
let test_start_get_stop_delete switch () =
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch () in
  let* time_entry =  stop_time_entry switch time_entry in
  let* _ =  delete_time_entry switch time_entry in
  return ()

let test_start_stop_delete switch () =
  let* workspace = get_workspace switch () in
  let* project =  get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_start_get_delete switch () =
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch () in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_start_delete switch () =
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch () in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_create_get_delete switch () =
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* time_entry = create_time_entry switch project in
  let* time_entry = get_time_entry switch time_entry in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_create_and_list_start_date_before switch () =
  let start_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let start_date = Ptime.sub_span start_date one_h in
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?start_date switch () in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "One issue" [time_entry] time_entries in
  return ()

let test_create_and_list_end_date_after switch () =
  let end_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let end_date = Ptime.add_span end_date one_h in
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?end_date switch () in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "One issue" [time_entry] time_entries in
  return ()

let test_create_and_list_start_date_after switch () =
  let start_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let start_date = Ptime.add_span start_date one_h in
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* _ = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?start_date switch () in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "No issue" [] time_entries in
  return ()

let test_create_and_list_end_date_before switch () =
  let end_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let end_date = Ptime.sub_span end_date one_h in
  let* workspace = get_workspace switch () in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* _ = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?end_date switch () in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "No issue" [] time_entries in
  return ()

let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ Alcotest_lwt.run "Toggl integration tests." [
    "No exception raised", [
      test_case "Start, get current, stop and delete time entry work without exception" `Slow test_start_get_stop_delete ;
      test_case "Start, get current and delete time entry work without exception" `Slow test_start_get_delete;
      test_case "Start, stop and delete time entry work without exception" `Slow test_start_stop_delete ;
      test_case "Start and delete time entry work without exception" `Slow test_start_delete;
      test_case "Create, get by id and delete a time entry without exception" `Slow test_create_get_delete;
      test_case "Create time entry then list all with start_date before should return it" `Slow test_create_and_list_start_date_before ;
      test_case "Create time entry then list all with end_date after should return it" `Slow test_create_and_list_end_date_after;
      test_case "Create time entry then list all with start_date after should return nothing" `Slow test_create_and_list_start_date_after;
      test_case "Create time entry then list all with end_date before should return nothing" `Slow test_create_and_list_end_date_before;
    ]
  ]
