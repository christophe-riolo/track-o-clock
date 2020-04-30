open Authenticated

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
    type t = {
      description: string option;
      wid: int option;
      pid: int option;
      tid: int option;
      billable: bool;
      start: string;
      stop: string option;
      duration: int;
      created_with: string;
      tags: string list;
      duronly: bool;
      at: string option
    } [@@deriving yojson]

    let make ?description ?wid ?pid ?tid ?(billable=false) ~start ?stop ~duration ?(tags=[]) ?(duronly=false) ?at () =
      {description; wid; pid; tid; billable; start; stop; duration; tags; duronly; at; created_with="toggl_revery"}

    let create t (client: Client.t) =
      let body = t
                 |> to_yojson
                 |> (fun te -> `Assoc [("time_entry", te)])
                 |> Yojson.Safe.to_string
                 |> Piaf.Body.of_string
      in Client.post client ~body "/api/v8/time_entries"
  end

  module Workspace = struct
    type t = {
      id: int;
      name: string;
      profile: int;
      premium: bool;
      admin: bool;
      default_hourly_rate: float;
      default_currency: string;
      only_admins_may_create_projects: bool;
      only_admins_see_billable_rates: bool;
      only_admins_see_team_dashboard: bool;
      projects_billable_by_default: bool;
      rounding: int;
      rounding_minutes: int;
      api_token: string;
      at: string;
      ical_enabled: bool
    } [@@deriving yojson]

    let list (client: Client.t) =
      Lwt_result.(
        Client.get client "/api/v8/workspaces"
        >>= Util.status_200_or_error
        >|= Yojson.Safe.from_string
        >|= Yojson.Safe.Util.to_list
        >|= List.map (fun x -> CCResult.get_or_failwith@@of_yojson x)
      )
  end
end
