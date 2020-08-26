let identity x = x

let () =
  Crowbar.add_test ~name:"binary search"
    Crowbar.[ list (bytes_fixed 20); list (bytes_fixed 20) ]
  @@ fun entries exists ->
  let entries = List.sort_uniq String.compare entries in
  let entries = Array.of_list entries in
  let length = 8 + (4 * 256) + (Array.length entries * 20) in
  let index = Bigstringaf.create length in

  Bigstringaf.set_int32_be index 0 0xff744f63l ;
  Bigstringaf.set_int32_be index 4 0x2l ;

  let v = ref 0 in

  for code = 0 to 255 do
    while !v < Array.length entries && entries.(!v).[0] = Char.chr code do
      incr v
    done ;

    Bigstringaf.set_int32_be index (8 + (4 * code)) (Int32.of_int !v)
  done ;

  for v = 0 to Array.length entries - 1 do
    Bigstringaf.blit_from_string entries.(v) ~src_off:0 index
      ~dst_off:(8 + (256 * 4) + (v * 20))
      ~len:20
  done ;

  let index =
    Carton.Dec.Idx.make index ~uid_ln:20 ~uid_rw:identity ~uid_wr:identity in
  List.iter (fun v -> ignore @@ Carton.Dec.Idx.exists index v) exists
