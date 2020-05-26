let status_200_or_error (response: Piaf.Response.t) : (string, Piaf.Error.t) Lwt_result.t =
  if Piaf.Status.is_successful response.status
  then Piaf.Body.to_string response.body
  else Lwt_result.(Piaf.Body.to_string response.body >>= (fun x -> Lwt.return (Error (`Msg x))))
