module type Client = module type of Piaf.Client

module F(A: sig val header: string end) : Client = struct
  open Piaf.Client

  type t = Piaf.Client.t
  let create = create
  let shutdown = shutdown

  let add_authorization = function
    | None -> Some ["Authorization", A.header]
    | Some headers -> Some (CCList.Assoc.set ~eq:CCString.equal "Authorization" A.header headers)

  let request client ?headers =
    let headers = add_authorization headers
    in request client ?headers

  let head client ?headers =
    let headers = add_authorization headers
    in head client ?headers

  let get client ?headers =
    let headers = add_authorization headers
    in get client ?headers

  let post client ?headers =
    let headers = add_authorization headers
    in post client ?headers

  let put client ?headers =
    let headers = add_authorization headers
    in put client ?headers

  let patch client ?headers =
    let headers = add_authorization headers
    in patch client ?headers

  let delete client ?headers =
    let headers = add_authorization headers
    in delete client ?headers

  module Oneshot = struct
    let head ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.head ?config ?headers

    let get ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.get ?config ?headers

    let post ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.post ?config ?headers

    let put ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.put ?config ?headers

    let patch ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.patch ?config ?headers

    let delete ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.delete ?config ?headers

    let request ?config ?headers =
      let headers = add_authorization headers
      in Oneshot.request ?config ?headers
  end
end

module Authentication = struct
  type meth = [ `Basic | `Session ]

  let create_client ~(meth: meth) username password : ((module Client), string) result =
    match meth with
    | `Session -> Ok (module F(struct let header = "Bearer foo" end)) (* TODO *)
    | `Basic -> (match Base64.encode (username ^ ":" ^ password) with
        | Ok creds -> Ok (module F(struct let header = "Basic " ^ creds end))
        | Error `Msg e -> Error e)
end

