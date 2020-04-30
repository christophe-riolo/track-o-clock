let status_200_or_error response =
  if Piaf.Response.status response |> Piaf.Status.is_successful
  then Piaf.Response.body response |> Piaf.Body.to_string |> Lwt_result.ok
  else Piaf.Response.body response |> Piaf.Body.to_string |> Lwt.map (fun x -> Error x)
