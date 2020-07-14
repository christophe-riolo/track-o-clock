open Alcotest
open TrackOClock.Toggl.Types

let client = TogglClient.create (Uri.of_string "https://api.toggl.com")
let error_client = TogglErrorClient.create (Uri.of_string "https://api.toggl.com")
let raise_error result =
  let open Lwt_result in
  result
  |> map_err (fun err -> Failure (Piaf.Error.to_string err))
  |> get_exn


module TestNormalBehaviour = struct

  module Api = TrackOClock.Toggl.Api(TogglClient)

  let time_entry = TrackOClock.Toggl_v.create_time_entry
      ~id:436694100
      ~pid:123
      ~wid:777
      ~billable:false
      ~start:(datetime_of_string "\"2013-03-05T07:58:58.000Z\"")
      ~duration:1200
      ~description:"Meeting with possible clients"
      ~tags:[
        "billed"
      ]
      ~uid:1
      ~at:(datetime_of_string "\"2013-03-05T07:58:58.000Z\"")
      ()

  let time_entry_request = create_time_entry
      ~pid:123
      ~wid:777
      ~billable:false
      ~start:(datetime_of_string "\"2013-03-05T07:58:58.000Z\"")
      ~duration:1200
      ~description:"Meeting with possible clients"
      ~tags:[
        "billed"
      ]
      ()

  let projects = [
    create_project
      ~id:123
      ~wid:777
      ~name:"Very lucrative project"
      ~billable:false
      ~is_private:true
      ~active:true
      ~at:(datetime_of_string "\"2013-03-06T09:15:18+00:00\"")
      ~created_at:(datetime_of_string "\"2013-03-06T09:15:18+00:00\"")
      ();
    create_project
      ~id:32123
      ~wid:777
      ~name:"Factory server infrastructure"
      ~billable:true
      ~is_private:true
      ~active:true
      ~at:(datetime_of_string "\"2013-03-06T09:16:06+00:00\"")
      ~created_at:(datetime_of_string "\"2013-03-06T09:16:06+00:00\"")
      ();
  ]

  let workspaces = [
    create_workspace
      ~id:3134975
      ~name:"John's personal ws"
      ~premium:true
      ~admin:true
      ~default_hourly_rate:50.
      ~default_currency:"USD"
      ~only_admins_may_create_projects:false
      ~only_admins_see_billable_rates:true
      ~rounding:1
      ~rounding_minutes:15
      ~at:(datetime_of_string "\"2013-08-28T16:22:21+00:00\"")
      ~logo_url:"my_logo.png"
      ();
    create_workspace
      ~id:777
      ~name:"My Company Inc"
      ~premium:true
      ~admin:true
      ~default_hourly_rate:40.
      ~default_currency:"EUR"
      ~only_admins_may_create_projects:false
      ~only_admins_see_billable_rates:true
      ~rounding:1
      ~rounding_minutes:15
      ~at:(datetime_of_string "\"2013-08-28T16:22:21+00:00\"")
      ()
  ]

  open Lwt_result
  let test_start_time_entry _switch () =
    client
    >>= Api.TimeEntry.start time_entry_request
    >|= check Testables.Toggl.time_entry "Same time entry" time_entry
    |> raise_error

  let test_stop_time_entry _switch () =
    client
    >>= Api.TimeEntry.stop 436694100
    >|= check Testables.Toggl.time_entry "Same time entry" time_entry
    |> raise_error

  let test_create_time_entry _switch () =
    client
    >>= Api.TimeEntry.create time_entry_request
    >|= check Testables.Toggl.time_entry "Same time entry" time_entry
    |> raise_error

  let test_current_time_entry _switch () =
    client
    >>= Api.TimeEntry.current
    >|= check Testables.Toggl.time_entry "Same time entry" time_entry
    |> raise_error

  let test_time_entry_details _switch () =
    client
    >>= Api.TimeEntry.details 436694100
    >|= check Testables.Toggl.time_entry "Same time entry" time_entry
    |> raise_error

  let test_delete_time_entry _switch () =
    client
    >>= Api.TimeEntry.delete 436694100
    >|= check string "Same workspaces" ""
    |> raise_error

  let test_list_time_entries_no_query _switch () =
    client
    >>= Api.TimeEntry.list
    >|= check (list Testables.Toggl.time_entry) "Same projects" []
    |> raise_error

  let test_list_time_entries_query _switch () =
    client
    >>= Api.TimeEntry.list
      ~start_date:(CCOpt.get_exn @@ Ptime.of_date (2020, 1, 1))
      ~end_date:(CCOpt.get_exn @@ Ptime.of_date (2020, 1, 2))
    >|= check (list Testables.Toggl.time_entry) "Same projects" [time_entry]
    |> raise_error

  let test_list_time_entries_future _switch () =
    client
    >>= Api.TimeEntry.list
      ~start_date:(CCOpt.get_exn @@ Ptime.of_date (4020, 1, 1))
      ~end_date:(CCOpt.get_exn @@ Ptime.of_date (4020, 1, 2))
    >|= check (list Testables.Toggl.time_entry) "Same projects" []
    |> raise_error

  let test_list_workspaces _switch () =
    client
    >>= Api.Workspace.list
    >|= check (list Testables.Toggl.workspace) "Same workspaces" workspaces
    |> raise_error

  let test_list_projects _switch () =
    client
    >>= Api.Project.list 777
    >|= check (list Testables.Toggl.project) "Same projects" projects
    |> raise_error

end

module TestNotFound = struct

  module Api = TrackOClock.Toggl.Api(TogglClient)
  open Lwt_result

  let test_stop_time_entry _switch () =
    client
    >>= Api.TimeEntry.stop 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Says that url is not found" "not_found")
    |> Lwt.map Result.get_error

  let test_list_projects _switch () =
    client
    >>= Api.Project.list 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Says that url is not found" "not_found")
    |> Lwt.map Result.get_error

  let test_time_entry_details _switch () =
    client
    >>= Api.TimeEntry.details 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Says that url is not found" "not_found")
    |> Lwt.map Result.get_error

  let test_delete_time_entry _switch () =
    client
    >>= Api.TimeEntry.delete 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Says that url is not found" "not_found")
    |> Lwt.map Result.get_error

end

module TestConnectionError = struct

  module Api = TrackOClock.Toggl.Api(TogglErrorClient)
  open Lwt_result

  let test_stop_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.stop 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_list_projects _switch () =
    error_client
    >>= Api.Project.list 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_start_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.start (create_time_entry ~description:"" ())
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_create_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.create (create_time_entry ~description:"" ())
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_current_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.current
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_time_entry_details _switch () =
    error_client
    >>= Api.TimeEntry.stop 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_delete_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.delete 0
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_list_workspaces _switch () =
    error_client
    >>= Api.Workspace.list
    |> map_err Piaf.Error.to_string
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

end

let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ Alcotest_lwt.run "Toggl unit tests" [
    "Normal behaviour", TestNormalBehaviour.[
        test_case "Creating time entry response is parsed" `Quick test_create_time_entry;
        test_case "Starting time entry response is parsed" `Quick test_start_time_entry;
        test_case "Stopping time entry response is parsed" `Quick test_stop_time_entry;
        test_case "Getting current time entry response is parsed" `Quick test_current_time_entry;
        test_case "Getting specified time entry response is parsed" `Quick test_time_entry_details;
        test_case "Deleting specified time entry response is parsed" `Quick test_delete_time_entry;
        test_case "Getting all time entries without query response is parsed" `Quick test_list_time_entries_no_query;
        test_case "Getting all time entries with query response is parsed" `Quick test_list_time_entries_query;
        test_case "Getting no time entries with query is parsed" `Quick test_list_time_entries_future;
        test_case "Getting all workspaces response is parsed" `Quick test_list_workspaces;
        test_case "Getting all projects response is parsed" `Quick test_list_projects;
      ];
    "Page not found", TestNotFound.[
        test_case "Stopping time entry response is parsed" `Quick test_stop_time_entry;
        test_case "Getting all projects response is parsed" `Quick test_list_projects;
        test_case "Getting specified time entry response is parsed" `Quick test_time_entry_details;
        test_case "Deleting specified time entry response is parsed" `Quick test_delete_time_entry;
      ];
    "Error case", TestConnectionError.[
        test_case "Creating time entry response returns error" `Quick test_create_time_entry;
        test_case "Starting time entry response returns error" `Quick test_start_time_entry;
        test_case "Stopping time entry response returns error" `Quick test_stop_time_entry;
        test_case "Getting current time entry response returns error" `Quick test_current_time_entry;
        test_case "Getting specified time entry response returns error" `Quick test_time_entry_details;
        test_case "Deleting specified time entry response returns error" `Quick test_delete_time_entry;
        test_case "Getting all workspaces response returns error" `Quick test_list_workspaces;
        test_case "Getting all projects response returns error" `Quick test_list_projects;
      ];
  ]
