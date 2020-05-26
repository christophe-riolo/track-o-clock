open Authenticated
include Toggl_j

module Auth = struct
  type t =
    | Basic of {username: string ; password: string}
    | ApiToken of string

  let get_client meth =
    (match meth with
     | ApiToken token -> Authenticated.Basic { username=token ; password="api_token" }
     | Basic {username; password} -> Authenticated.Basic { username ; password })
    |> create_client
end

module Api(Client: Authenticated.Client) = struct

  module TimeEntry = struct
    let make = Toggl_v.create_time_entry ~created_with:"trackoclock"

    let create t (client: Client.t) =
      let body = {time_entry = t}
                 |> string_of_wrapped_time_entry
                 |> Piaf.Body.of_string
      in Client.post client ~body "/api/v8/time_entries"

    let start t (client: Client.t) =
      let body = {time_entry = t}
                 |> string_of_wrapped_time_entry
                 |> Piaf.Body.of_string
      in Lwt_result.(
          Client.post client ~body "/api/v8/time_entries/start"
          >>= Util.status_200_or_error
          >|= data_time_entry_of_string
        )

    let stop tid (client: Client.t) =
      let body = Piaf.Body.empty
      in Lwt_result.(
          "/api/v8/time_entries/" ^ string_of_int tid ^ "/stop"
          |> Client.put client ~body
          >>= Util.status_200_or_error
          >|= data_time_entry_of_string
        )
  end

  module Workspace = struct
    let list (client: Client.t) =
      Lwt_result.(
        Client.get client "/api/v8/workspaces"
        >>= Util.status_200_or_error
        >|= workspace_list_of_string
      )
  end

  module Project = struct
    let list wid (client: Client.t) =
      Lwt_result.(
        "/api/v8/workspaces/" ^ (string_of_int wid) ^ "/projects"
        |> Client.get client
        >>= Util.status_200_or_error
        >|= project_list_of_string
      )
  end
end
