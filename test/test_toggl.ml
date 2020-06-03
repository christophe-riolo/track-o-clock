open Alcotest
open TrackOClock.Toggl.Types

module Testables = struct
  open TrackOClock.Toggl.Types
  let time_entry: time_entry testable = (module struct
    type t = [%import: TrackOClock.Toggl.Types.time_entry] [@@deriving show, eq]
  end)

  let project: project testable = (module struct
    type t = [%import: TrackOClock.Toggl.Types.project] [@@deriving show, eq]
  end)

  let workspace: workspace testable = (module struct
    type t = [%import: TrackOClock.Toggl.Types.workspace] [@@deriving show, eq]
  end)
end

let client = TogglClient.create (Uri.of_string "http://toggle.com")
let error_client = TogglErrorClient.create (Uri.of_string "http://toggle.com")
let raise_error result =
  let open Lwt_result in
  result
  |> map_err (fun err -> Failure (Piaf.Error.to_string err))
  |> get_exn


module TestNormalBehaviour = struct

  module Api = TrackOClock.Toggl.Api(TogglClient)

  let time_entry = create_time_entry
      ~id:436694100
      ~pid:123
      ~wid:777
      ~billable:false
      ~start:"2013-03-05T07:58:58.000Z"
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
      ~cid:987
      ~name:"Very lucrative project"
      ~billable:false
      ~is_private:true
      ~active:true
      ~at:"2013-03-06T09:15:18+00:00"
      ();
    create_project
      ~id:32123
      ~wid:777
      ~cid:123
      ~name:"Factory server infrastructure"
      ~billable:true
      ~is_private:true
      ~active:true
      ~at:"2013-03-06T09:16:06+00:00"
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
      ~at:"2013-08-28T16:22:21+00:00"
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
      ~at:"2013-08-28T16:22:21+00:00"
      ()
  ]

  open Lwt_result
  let test_start_time_entry _switch () =
    client
    >>= Api.TimeEntry.start time_entry
    >|= check Testables.time_entry "Same time entry" time_entry
    |> raise_error

  let test_stop_time_entry _switch () =
    client
    >>= Api.TimeEntry.stop 436694100
    >|= check Testables.time_entry "Same time entry" time_entry
    |> raise_error

  let test_create_time_entry _switch () =
    client
    >>= Api.TimeEntry.create time_entry
    >|= check Testables.time_entry "Same time entry" time_entry
    |> raise_error

  let test_current_time_entry _switch () =
    client
    >>= Api.TimeEntry.current
    >|= check Testables.time_entry "Same time entry" time_entry
    |> raise_error

  let test_list_workspaces _switch () =
    client
    >>= Api.Workspace.list
    >|= check (list Testables.workspace) "Same workspaces" workspaces
    |> raise_error

  let test_list_projects _switch () =
    client
    >>= Api.Project.list 777
    >|= check (list Testables.project) "Same projects" projects
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

end

module TestConnectionError = struct

  module Api = TrackOClock.Toggl.Api(TogglErrorClient)
  open Lwt_result

  let test_stop_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.stop 0
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_list_projects _switch () =
    error_client
    >>= Api.Project.list 0
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_start_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.start (create_time_entry ())
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_create_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.create (create_time_entry ())
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_current_time_entry _switch () =
    error_client
    >>= Api.TimeEntry.current
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

  let test_list_workspaces _switch () =
    error_client
    >>= Api.Workspace.list
    |> map_err Piaf.Error.to_string
    |> map_err CCString.trim
    |> map_err (check string "Returns error" "Connect Error: connection error")
    |> Lwt.map Result.get_error

end

let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ Alcotest_lwt.run "foo" [
    "Normal behaviour", TestNormalBehaviour.[
      test_case "Creating time entry response is parsed" `Quick test_create_time_entry;
      test_case "Starting time entry response is parsed" `Quick test_start_time_entry;
      test_case "Stopping time entry response is parsed" `Quick test_stop_time_entry;
      test_case "Getting current time entry response is parsed" `Quick test_current_time_entry;
      test_case "Getting all workspaces response is parsed" `Quick test_list_workspaces;
      test_case "Getting all projects response is parsed" `Quick test_list_projects;
    ];
    "Page not found", TestNotFound.[
      test_case "Stopping time entry response is parsed" `Quick test_stop_time_entry;
      test_case "Getting all projects response is parsed" `Quick test_list_projects;
    ];
    "Error case", TestConnectionError.[
      test_case "Creating time entry response returns error" `Quick test_create_time_entry;
      test_case "Starting time entry response returns error" `Quick test_start_time_entry;
      test_case "Stopping time entry response returns error" `Quick test_stop_time_entry;
      test_case "Getting current time entry response returns error" `Quick test_current_time_entry;
      test_case "Getting all workspaces response returns error" `Quick test_list_workspaces;
      test_case "Getting all projects response returns error" `Quick test_list_projects;
    ];
  ]
