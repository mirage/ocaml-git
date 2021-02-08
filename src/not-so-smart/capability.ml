type t = [ Capability_v1.t | Capability_v2.t ]

let filter_by ~protocol_v lst =
  let filter =
    match protocol_v with
    | `V1 -> ( function #Capability_v1.t as c -> Some c | _ -> None)
    | `V2 -> ( function #Capability_v2.t as c -> Some c | _ -> None)
    | _ -> invalid_arg "unsupported protocol version"
  in
  List.filter_map filter lst

let to_string = function
  | #Capability_v1.t as c -> Capability_v1.to_string c
  | #Capability_v2.t as c -> Capability_v2.to_string c

exception Capability_expect_value of string

let of_string ?(protocol_v = `V1) ?value s =
  match protocol_v with
  | `V1 -> (Capability_v1.of_string ?value s :> t)
  | `V2 -> (Capability_v2.of_string s :> t)
  | _ -> invalid_arg "unsupported protocol version"

let pp ppf = function
  | #Capability_v1.t as c -> Capability_v1.pp ppf c
  | #Capability_v2.t as c -> Capability_v2.pp ppf c

let compare a b =
  match a, b with
  | (#Capability_v1.t as a), (#Capability_v1.t as b) ->
      Capability_v1.compare a b
  | (#Capability_v2.t as a), (#Capability_v2.t as b) ->
      if Capability_v2.equal a b then 0
      else
        invalid_arg
          "Capability.compare: comparison for capabilities for git wire \
           protocol v2 is undefined"
  | _ ->
      invalid_arg
        "Capability.compare: comparison between such capabilities is undefined"

let equal a b = compare a b = 0
