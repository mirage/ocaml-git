type t =
  [ `Multi_ack
  | `Multi_ack_detailed
  | `No_done
  | `Thin_pack
  | `Side_band
  | `Side_band_64k
  | `Ofs_delta
  | `Agent of string
  | `Shallow
  | `Deepen_since
  | `Deepen_not
  | `No_progress
  | `Include_tag
  | `Report_status
  | `Delete_refs
  | `Quiet
  | `Atomic
  | `Push_options
  | `Allow_tip_sha1_in_want
  | `Allow_reachable_sha1_in_want
  | `Push_cert of string
  | `Symref of string * string
  | `Other of string
  | `Parameter of string * string ]

let to_string = function
  | `Multi_ack -> "multi_ack"
  | `Multi_ack_detailed -> "multi_ack_detailed"
  | `No_done -> "no-done"
  | `Thin_pack -> "thin-pack"
  | `Side_band -> "side-band"
  | `Side_band_64k -> "side-band-64k"
  | `Ofs_delta -> "ofs-delta"
  | `Agent agent -> Fmt.str "agent=%s" agent
  | `Shallow -> "shallow"
  | `Deepen_since -> "deepen-since"
  | `Deepen_not -> "deepen-not"
  | `No_progress -> "no-progress"
  | `Include_tag -> "include-tag"
  | `Report_status -> "report-status"
  | `Delete_refs -> "delete-refs"
  | `Quiet -> "quiet"
  | `Atomic -> "atomic"
  | `Push_options -> "push-options"
  | `Allow_tip_sha1_in_want -> "allow-tip-sha1-in-want"
  | `Allow_reachable_sha1_in_want -> "allow-reachable-sha1-in-want"
  | `Push_cert cert -> Fmt.str "push-cert=%s" cert
  | `Symref (ref0, ref1) -> Fmt.str "symref=%s:%s" ref0 ref1
  | `Other capability -> capability
  | `Parameter (key, value) -> Fmt.str "%s=%s" key value

exception Capability_expect_value of string

let of_string ?value = function
  | "multi_ack" -> `Multi_ack
  | "multi_ack_detailed" -> `Multi_ack_detailed
  | "no-done" -> `No_done
  | "thin-pack" -> `Thin_pack
  | "side-band" -> `Side_band
  | "side-band-64k" -> `Side_band_64k
  | "ofs-delta" -> `Ofs_delta
  | "shallow" -> `Shallow
  | "deepen-since" -> `Deepen_since
  | "deepen-not" -> `Deepen_not
  | "no-progress" -> `No_progress
  | "include-tag" -> `Include_tag
  | "report-status" -> `Report_status
  | "delete-refs" -> `Delete_refs
  | "quiet" -> `Quiet
  | "atomic" -> `Atomic
  | "push-options" -> `Push_options
  | "allow-tip-sha1-in-want" -> `Allow_tip_sha1_in_want
  | "allow-reachable-sha1-in-want" -> `Allow_reachable_sha1_in_want
  | "push-cert" -> (
      match value with
      | Some value -> `Push_cert value
      | None -> raise (Capability_expect_value "push-cert"))
  | "agent" -> (
      match value with
      | Some value -> `Agent value
      | None -> raise (Capability_expect_value "agent"))
  | "symref" -> (
      match Option.bind value (Astring.String.cut ~sep:":") with
      | Some (ref0, ref1) -> `Symref (ref0, ref1)
      | None -> raise (Capability_expect_value "symref"))
  | capability -> (
      match value with
      | Some value -> `Parameter (capability, value)
      | None -> `Other capability)

let pp ppf = function
  | `Multi_ack -> Fmt.pf ppf "Multi-ACK"
  | `Multi_ack_detailed -> Fmt.pf ppf "Multi-ACK-detailed"
  | `No_done -> Fmt.pf ppf "No-done"
  | `Thin_pack -> Fmt.pf ppf "Thin-PACK"
  | `Side_band -> Fmt.pf ppf "Side-Band"
  | `Side_band_64k -> Fmt.pf ppf "Side-Band-64K"
  | `Ofs_delta -> Fmt.pf ppf "Offset-delta"
  | `Agent agent -> Fmt.pf ppf "(Agent %s)" agent
  | `Shallow -> Fmt.pf ppf "Shallow"
  | `Deepen_since -> Fmt.pf ppf "Deepen-Since"
  | `Deepen_not -> Fmt.pf ppf "Deepen-Not"
  | `No_progress -> Fmt.pf ppf "No-Progress"
  | `Include_tag -> Fmt.pf ppf "Include-Tag"
  | `Report_status -> Fmt.pf ppf "Report-Status"
  | `Delete_refs -> Fmt.pf ppf "Delete-Refs"
  | `Quiet -> Fmt.pf ppf "Quiet"
  | `Atomic -> Fmt.pf ppf "Atomic"
  | `Push_options -> Fmt.pf ppf "Push-Options"
  | `Allow_tip_sha1_in_want -> Fmt.pf ppf "Allow-Tip-SHA1-in-Want"
  | `Allow_reachable_sha1_in_want -> Fmt.pf ppf "Allow-Reachable-SHA1-in-Want"
  | `Push_cert cert -> Fmt.pf ppf "(Push Cert %s)" cert
  | `Symref (ref0, ref1) -> Fmt.pf ppf "(Symref (%s, %s))" ref0 ref1
  | `Other capability -> Fmt.pf ppf "(other %s)" capability
  | `Parameter (key, value) -> Fmt.pf ppf "(%s %s)" key value

let compare a b =
  match a, b with
  | `Multi_ack, `Multi_ack
  | `Multi_ack_detailed, `Multi_ack_detailed
  | `No_done, `No_done
  | `Thin_pack, `Thin_pack
  | `Side_band, `Side_band
  | `Side_band_64k, `Side_band_64k
  | `Ofs_delta, `Ofs_delta
  | `Shallow, `Shallow
  | `Deepen_since, `Deepen_since
  | `Deepen_not, `Deepen_not
  | `No_progress, `No_progress
  | `Include_tag, `Include_tag
  | `Report_status, `Report_status
  | `Delete_refs, `Delete_refs
  | `Quiet, `Quiet
  | `Atomic, `Atomic
  | `Push_options, `Push_options
  | `Allow_tip_sha1_in_want, `Allow_tip_sha1_in_want
  | `Allow_reachable_sha1_in_want, `Allow_reachable_sha1_in_want ->
      0
  | `Push_cert a, `Push_cert b | `Agent a, `Agent b | `Other a, `Other b ->
      String.compare a b
  | `Symref (refa0, refa1), `Symref (refb0, refb1) ->
      let res = String.compare refa0 refb0 in
      if res = 0 then String.compare refa1 refb1 else res
  | `Parameter (ka, va), `Parameter (kb, vb) ->
      let res = String.compare ka kb in
      if res = 0 then String.compare va vb else res
  | a, b -> if a > b then 1 else -1

let equal a b = compare a b = 0
