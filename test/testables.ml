open Alcotest

module Toggl = struct
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
