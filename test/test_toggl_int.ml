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

let get_workspace _switch =
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

let create_time_entry ~pid ?(description="Test time entry") ?(start=Types.datetime_of_string "\"2020-01-01T00:00:00Z\"") ?stop ?(duration=3600) ?tags ?duronly ?billable switch =
  let time_entry = client
    >>= TimeEntry.create (Types.create_time_entry ~pid ~description ?tags ~start ?stop ~duration ?duronly ?billable ())
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

let get_current_time_entry _switch =
  client
  >>= TimeEntry.current
  >|= get_or_failwith
  >>= wait

let list_time_entries ?start_date ?end_date switch =
  client
  >>= TimeEntry.list ?start_date ?end_date
  >|= get_or_failwith
  |> both (get_workspace switch)
  >|= (function workspace, time_entries -> List.filter (fun (te: Types.time_entry) -> te.wid = workspace.id) time_entries)
  >>= wait

let update_time_entry t ?description ?start ?stop ?duration ?tags ?project ?workspace ?duronly ?billable _switch =
  client
  >>= TimeEntry.update t ?description ?start ?stop ?duration ?tags ?project ?workspace ?duronly ?billable
  >|= get_or_failwith
  >>= wait

(* Tests *)
let test_start_get_stop_delete switch () =
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch in
  let* time_entry =  stop_time_entry switch time_entry in
  let* _ =  delete_time_entry switch time_entry in
  return ()

let test_start_stop_delete switch () =
  let* workspace = get_workspace switch in
  let* project =  get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_start_get_delete switch () =
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_start_delete switch () =
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* _ = start_time_entry switch project in
  let* time_entry = get_current_time_entry switch in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_create_get_delete switch () =
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* time_entry = create_time_entry switch ~pid:project.id in
  let* time_entry = get_time_entry switch time_entry in
  let* _ = delete_time_entry switch time_entry in
  return ()

let test_create_and_list_start_date_before switch () =
  let start_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let start_date = Ptime.sub_span start_date one_h in
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?start_date switch in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "One issue" [time_entry] time_entries in
  return ()

let test_create_and_list_end_date_after switch () =
  let end_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let end_date = Ptime.add_span end_date one_h in
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* time_entry = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?end_date switch in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "One issue" [time_entry] time_entries in
  return ()

let test_create_and_list_start_date_after switch () =
  let start_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let start_date = Ptime.add_span start_date one_h in
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* _ = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?start_date switch in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "No issue" [] time_entries in
  return ()

let test_create_and_list_end_date_before switch () =
  let end_date = Ptime_clock.now () in
  let one_h = Ptime.Span.of_int_s 3600 in
  let end_date = Ptime.sub_span end_date one_h in
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* time_entry = start_time_entry switch project in
  let* _ = stop_time_entry switch time_entry in
  let* time_entries = list_time_entries ?end_date switch in
  let* _ = return @@ Alcotest.(check (list Testables.Toggl.time_entry)) "No issue" [] time_entries in
  return ()

let test_modify_time_entry switch () =
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* te0 = create_time_entry ~pid:project.id switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Expected initial state" {
      id=te0.id;
      wid=4436316;
      pid=Some 161895933;
      tags=[];
      billable=false;
      start=CCOpt.get_exn@@Ptime.of_date_time((2020,1,1),((0,0,0),0));
      stop=Some (CCOpt.get_exn@@Ptime.of_date_time((2020,1,1),((1,0,0),0)));
      duration=3600;
      description="Test time entry";
      duronly=false;
      uid=4179541;
      at = te0.at} te0 in
  let* te1 = update_time_entry te0.id ~stop:(Ptime.add_span te0.start (Ptime.Span.of_int_s 1800)) switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Stop time modified" {
      te0 with stop = Ptime.add_span te0.start (Ptime.Span.of_int_s 1800);
               at = te1.at
    } te1 in
  let* te2 = update_time_entry te1.id ~duration:7200 switch in (* 2h *)
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Duration and stop time modified" {
      te1 with duration = 7200;
               stop = Ptime.add_span te1.start (Ptime.Span.of_int_s 7200);
               at = te2.at
    } te2 in
  let* te3 = update_time_entry te2.id ~description:"New description" switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Description modified" {
      te2 with description = "New description";
               at = te3.at
    } te3 in
  let* te4 = update_time_entry te3.id ~start:(Ptime.add_span te3.start (Ptime.Span.of_int_s 60) |> CCOpt.get_exn) switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Start and stop times modified" {
      te3 with start = CCOpt.get_exn @@ Ptime.add_span te3.start (Ptime.Span.of_int_s 60);
               stop = Ptime.add_span te4.start (Ptime.Span.of_int_s te3.duration);
               at = te4.at
    } te4 in
  let* te5 = update_time_entry te4.id ~tags:["foo"] switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Tags modified" {
      te4 with tags=["foo"];
               at = te5.at
    } te5 in
  let* te6 = update_time_entry te5.id ~project:None switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Project nullified" {
      te5 with pid=None;
               at = te6.at
    } te6 in
  let* workspace = get_workspace switch in
  let* project = get_project switch workspace in
  let* te6' = update_time_entry te5.id ~project:(Some project) switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "Project added" {
      te5 with pid=te0.pid;
               at = te6'.at
    } te6' in
  (* TODO test modifying workspace after creating a new one maybe *)
  let* te7 = update_time_entry te6'.id ~duronly:true switch in
  let* _ = return @@ Alcotest.check Testables.Toggl.time_entry "duronly set to true" {
      te6' with duronly=true;
               at = te7.at
    } te7 in
  (* Cannot test billable without a premium workspace *)
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
      test_case "Create time entry then modify it" `Slow test_modify_time_entry;
    ]
  ]
