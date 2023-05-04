let satisfy predicate str =
  try
    for i = 0 to String.length str - 1 do
      if predicate str.[i] then invalid_arg "satisfy"
    done;
    false
  with Invalid_argument _ -> true

let is_not_refname = function
  | ' ' | '~' | '^' | ':' | '?' | '*' -> true
  | chr -> if Char.code chr < 32 || Char.code chr > 126 then true else false

let reference =
  Crowbar.map Crowbar.[ bytes ] @@ fun str ->
  if String.length str = 0 || satisfy is_not_refname str then
    Crowbar.bad_test ();
  str

let capability =
  Crowbar.choose
    Crowbar.
      [
        const `Multi_ack;
        const `Multi_ack_detailed;
        const `No_done;
        const `Thin_pack;
        const `Side_band;
        const `Side_band_64k;
        const `Ofs_delta;
        const `Shallow;
        const `Deepen_since;
        const `Deepen_not;
        const `No_progress;
        const `Include_tag;
        const `Report_status;
        const `Delete_refs;
        const `Quiet;
        const `Atomic;
        const `Push_options;
        const `Allow_tip_sha1_in_want;
        const `Allow_reachable_sha1_in_want;
      ]

let zero_uid = String.make Digestif.SHA1.digest_size '\x00'

let sha1 =
  Crowbar.map Crowbar.[ bytes_fixed Digestif.SHA1.digest_size ] @@ fun uid ->
  if String.equal uid zero_uid then Crowbar.bad_test ();
  Digestif.SHA1.of_raw_string uid

let capabilities =
  Crowbar.map Crowbar.[ list1 capability ] @@ fun lst ->
  match List.sort_uniq Smart.Capability.compare lst with
  | [] -> Crowbar.bad_test ()
  | v -> v

let packet = Crowbar.range ~min:4 64

let advertised_ref =
  Crowbar.map Crowbar.[ sha1; reference; bool ] @@ fun uid refname peeled ->
  uid, refname, peeled

let ( >>= ) = Crowbar.dynamic_bind

let () =
  let of_string str =
    let ctx = Smart.Context.make ~client_caps:[] in
    let state =
      Smart.decode ctx (Smart.packet ~trim:false) (fun _ctx res -> Return res)
    in
    let pos = ref 0 in
    let rec go = function
      | Smart.Read { buffer; off; len; k; eof } ->
          if !pos = String.length str then go (eof ())
          else
            let len = min (String.length str - !pos) len in
            Bytes.blit_string str !pos buffer off len;
            pos := !pos + len;
            go (k len)
      | Smart.Write _ -> Crowbar.fail "Unexpected [Write]"
      | Smart.Error (`Protocol err) -> Crowbar.failf "%a" Smart.pp_error err
      | Smart.Return v -> v
    in
    go state
  in

  Crowbar.add_test ~name:"pkt-line" Crowbar.[ packet >>= bytes_fixed; range 4 ]
  @@ fun payload len ->
  let str = Fmt.str "%04x" (String.length payload + 4) ^ payload in
  let res = of_string str in
  Crowbar.check_eq ~pp:(Fmt.fmt "%S") ~eq:String.equal ~cmp:String.compare res
    payload;
  let str = Fmt.str "%04x" len in
  let res = of_string str in
  Crowbar.check_eq ~pp:(Fmt.fmt "%S") ~eq:String.equal ~cmp:String.compare res
    ""

let () =
  let of_string str =
    let ctx = Smart.Context.make ~client_caps:[] in
    let state =
      Smart.decode ctx Smart.advertised_refs (fun _ctx res -> Return res)
    in
    let pos = ref 0 in
    let rec go = function
      | Smart.Read { buffer; off; len; k; eof } ->
          if !pos = String.length str then go (eof ())
          else
            let len = min (String.length str - !pos) len in
            Bytes.blit_string str !pos buffer off len;
            pos := !pos + len;
            go (k len)
      | Smart.Write _ -> Crowbar.fail "Unexpected [Write]"
      | Smart.Error (`Protocol err) -> Crowbar.failf "%a" Smart.pp_error err
      | Smart.Return v -> v
    in
    go state
  in
  let to_string v =
    let ctx = Smart.Context.make ~client_caps:[] in
    let buf = Buffer.create 0x1000 in
    let state =
      Smart.encode ctx Smart.send_advertised_refs v (fun _ctx ->
          Return (Buffer.contents buf))
    in
    let rec go = function
      | Smart.Write { buffer; off; len; k } ->
          Buffer.add_substring buf buffer off len;
          go (k len)
      | Smart.Read _ -> Crowbar.failf "Unexpected [Read]"
      | Smart.Error (`Protocol err) -> Crowbar.failf "%a" Smart.pp_error err
      | Smart.Return v -> v
    in
    go state
  in
  Crowbar.add_test ~name:"advertised-refs"
    Crowbar.[ list advertised_ref; capabilities; list sha1 ]
  @@ fun refs capabilities shallows ->
  let shallows = List.map Digestif.SHA1.to_hex shallows in
  let refs =
    List.map
      (fun (hash, refname, peeled) ->
        Digestif.SHA1.to_hex hash, refname, peeled)
      refs
  in
  let shallows = if refs = [] then [] else shallows in
  let str = to_string (Smart.Advertised_refs.v1 ~shallows ~capabilities refs) in
  let res = of_string str in
  Crowbar.check_eq ~pp:Smart.Advertised_refs.pp
    ~eq:(Smart.Advertised_refs.equal ~uid:String.equal ~reference:String.equal)
    (Smart.Advertised_refs.v1 ~shallows ~capabilities refs)
    res
