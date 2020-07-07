open TrackOClock.Toggl
open Lwt

(* Create the authenticated client using the token in environment *)
let username = Sys.getenv "toggl_token"
let password = "api_token"

module Client = TrackOClock.Toggl.Client(struct let auth = Auth.Basic {username ; password} end)
module Api = Api(Client)
open Api

let get_or_failwith = function
  | Ok x -> x
  | Error e -> Piaf.Error.to_string e |> failwith

let client = Client.create @@ Uri.of_string "https://api.toggl.com" >|= get_or_failwith

(* Utility functions for writing tests *)
let wait () = Lwt_unix.sleep 1.

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

let create_time_entry _switch ({id; _}: Types.project) =
  client
  >>= TimeEntry.create (Types.create_time_entry 
                          ~pid:id
                          ~description:"Test time entry"
                          ~tags:["foo"]
                          ~billable:false
                          ~start:"2020-01-01T00:00:00Z"
                          ~duration:5
                          ())
  >|= get_or_failwith
  |> both (wait ())

let start_time_entry _switch ({id; _}: Types.project) =
  client
  >>= TimeEntry.start (Types.create_time_entry ~pid:id ~description:"Test time entry" ())
  >|= get_or_failwith
  |> both (wait ())

let stop_time_entry _switch (_pause, time_entry: unit * Types.time_entry) =
  client
  >>= TimeEntry.stop (CCOpt.get_exn time_entry.id)
  >|= get_or_failwith
  |> both (wait ())

let delete_time_entry _switch (_pause, time_entry: unit * Types.time_entry) =
  client
  >>= TimeEntry.delete (CCOpt.get_exn time_entry.id)
  |> both (wait ())

let get_time_entry _switch (_pause, time_entry: unit * Types.time_entry) =
  client
  >>= TimeEntry.details (CCOpt.get_exn time_entry.id)
  >|= get_or_failwith
  |> both (wait ())

let get_current_time_entry _switch (_pause: unit) =
  client
  >>= TimeEntry.current
  >|= get_or_failwith
  |> both (wait ())

(* Tests *)
let test_start_get_stop_delete switch () =
  get_workspace switch ()
  >>= get_project switch
  >>= start_time_entry switch
  >|= ignore
  >>= get_current_time_entry switch
  >>= stop_time_entry switch
  >>= delete_time_entry switch
  >|= ignore

let test_start_stop_delete switch () =
  get_workspace switch ()
  >>= get_project switch
  >>= start_time_entry switch
  >>= stop_time_entry switch
  >>= delete_time_entry switch
  >|= ignore

let test_start_get_delete switch () =
  get_workspace switch ()
  >>= get_project switch
  >>= start_time_entry switch
  >|= ignore
  >>= get_current_time_entry switch
  >>= delete_time_entry switch
  >|= ignore

let test_start_delete switch () =
  get_workspace switch ()
  >>= get_project switch
  >>= start_time_entry switch
  >|= ignore
  >>= get_current_time_entry switch
  >>= delete_time_entry switch
  >|= ignore

let test_create_get_delete switch () =
  get_workspace switch ()
  >>= get_project switch
  >>= create_time_entry switch
  >>= get_time_entry switch
  >>= delete_time_entry switch
  >|= ignore

let () =
  let open Alcotest_lwt in
  Lwt_main.run @@ Alcotest_lwt.run "Toggl integration tests." [
    "No exception raised", [
      test_case "Start, get current, stop and delete time entry work without exception" `Slow test_start_get_stop_delete ;
      test_case "Start, get current and delete time entry work without exception" `Slow test_start_get_delete;
      test_case "Start, stop and delete time entry work without exception" `Slow test_start_stop_delete ;
      test_case "Start and delete time entry work without exception" `Slow test_start_delete;
      test_case "Create, get by id and delete a time entry without exception" `Slow test_create_get_delete;
    ]
  ]
