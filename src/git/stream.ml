let src =
  Logs.Src.create "git.stream" ~doc:"logs git's internal stream computation"

module Log = (val Logs.src_log src : Logs.LOG)

let hdr = function
  | `Blob -> Fmt.strf "blob %Ld\000"
  | `Tree -> Fmt.strf "tree %Ld\000"
  | `Tag -> Fmt.strf "tag %Ld\000"
  | `Commit -> Fmt.strf "commit %Ld\000"

type ('uid, 't) digest = {
  empty : 't;
  feed_string : string -> 't -> 't;
  feed_bigstring : Bigstringaf.t -> 't -> 't;
  get : 't -> 'uid;
}

let digest digest kind length serializer v =
  let hdr = hdr kind (length v) in
  let state = Encore.Lavoisier.emit v serializer in
  let rec go ctx = function
    | Encore.Lavoisier.Partial { buffer = str; off; len; continue } ->
        let str = String.sub str off len in
        let ctx = digest.feed_string str ctx in
        go ctx (continue ~committed:len)
    | Fail -> Fmt.failwith "Invalid Git object"
    | Done -> digest.get ctx in
  go (digest.feed_string hdr digest.empty) state
