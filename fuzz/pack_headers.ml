module Us = Carton.Make (struct
  type 'a t = 'a
end)

let unix =
  {
    Carton.bind = (fun x f -> f (Us.prj x));
    Carton.return = (fun x -> Us.inj x);
  }

let z = Bigstringaf.create De.io_buffer_size
let allocate bits = De.make_window ~bits
let o = Bigstringaf.create De.io_buffer_size

let map payload ~pos len =
  if pos < 0L then Bigstringaf.empty
  else if pos >= Int64.of_int (Bigstringaf.length payload) then
    Bigstringaf.empty
  else
    let max = Int64.sub (Int64.of_int (Bigstringaf.length payload)) pos in
    let len = min (Int64.of_int len) max in
    let len = Int64.to_int len in
    Bigstringaf.sub payload ~off:(Int64.to_int pos) ~len

let () =
  Crowbar.add_test ~name:"pack-headers never fails" Crowbar.[ int64; bytes ]
  @@ fun pos bytes ->
  let payload = Bigstringaf.of_string bytes ~off:0 ~len:(String.length bytes) in
  let t =
    Carton.Dec.make payload ~z ~allocate ~uid_ln:20
      ~uid_rw:(fun x -> x)
      (fun _ -> assert false)
  in
  let kind, length, _pos, _slice =
    Carton.Dec.header_of_entry ~map t pos
      {
        Carton.Dec.W.payload;
        offset = pos;
        length = Bigstringaf.length payload;
      }
  in
  ignore @@ (kind, length)

let () =
  Crowbar.add_test ~name:"can decode what we encode" Crowbar.[ int8; int ]
  @@ fun kind length ->
  let kind = kind land 7 in
  let length = abs length in
  let payload = Bigstringaf.create 20 in
  let len = Carton.Enc.encode_header ~o:payload (kind land 7) length in
  let payload = Bigstringaf.sub payload ~off:0 ~len in
  let t =
    Carton.Dec.make payload ~z ~allocate ~uid_ln:20
      ~uid_rw:(fun x -> x)
      (fun _ -> assert false)
  in
  let kind', length', _pos, _slice =
    Carton.Dec.header_of_entry ~map t 0L
      { Carton.Dec.W.payload; offset = 0L; length = Bigstringaf.length payload }
  in
  Crowbar.check_eq ~pp:Fmt.int kind kind';
  Crowbar.check_eq ~pp:Fmt.int length length'
