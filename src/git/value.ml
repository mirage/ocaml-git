module type S =
sig
  module Digest
    : Ihash.IDIGEST
  module Inflate
    : Common.INFLATE
  module Deflate
    : Common.DEFLATE

  module Hash
    : Common.BASE
  module Blob
    : Blob.S with type Hash.t = Hash.t
  module Commit
    : Commit.S with type Hash.t = Hash.t
  module Tree
    : Tree.S with type Hash.t = Hash.t
  module Tag
    : Tag.S with type Hash.t = Hash.t

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tree   of Tree.t
    | Tag    of Tag.t

  module A : Common.ANGSTROM with type t = t
  module F : Common.FARADAY  with type t = t
  module D : Common.DECODER  with type t = t
                              and type raw = Cstruct.t
                              and type init = Inflate.window * Cstruct.t * Cstruct.t
                              and type error = [ `Decoder of string | `Inflate of Inflate.error ]
  module M : Common.MINIENC  with type t = t
  module E : Common.ENCODER  with type t = t
                              and type raw = Cstruct.t
                              and type init = int * t * int * Cstruct.t
                              and type error = [ `Deflate of Deflate.error ]

  include Ihash.DIGEST with type t := t and type hash := Hash.t
  include Common.BASE with type t := t
end

module Make (Digest : Ihash.IDIGEST with type t = Bytes.t
                                    and type buffer = Cstruct.t)
    (Inflate : Common.INFLATE)
    (Deflate : Common.DEFLATE)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
       and module Inflate = Inflate
       and module Deflate = Deflate
= struct
  module Digest = Digest
  module Inflate = Inflate
  module Deflate = Deflate
  module Hash = Helper.BaseBytes

  module Blob   = Blob.Make(Digest)
  module Commit = Commit.Make(Digest)
  module Tree   = Tree.Make(Digest)
  module Tag    = Tag.Make(Digest)

  type t =
    | Blob   of Blob.t
    | Commit of Commit.t
    | Tree   of Tree.t
    | Tag    of Tag.t

  let pp ppf = function
    | Blob blob     -> Fmt.pf ppf "(Blob %a)" (Fmt.hvbox Blob.pp) blob
    | Commit commit -> Fmt.pf ppf "(Commit %a)" (Fmt.hvbox Commit.pp) commit
    | Tree tree     -> Fmt.pf ppf "(Tree %a)" (Fmt.hvbox Tree.pp) tree
    | Tag tag       -> Fmt.pf ppf "(Tag %a)" (Fmt.hvbox Tag.pp) tag

  module A =
  struct
    type nonrec t = t

    let kind =
      let open Angstrom in
      (((string "blob" *> return `Blob) <?> "blob")
       <|> ((string "commit" *> return `Commit) <?> "commit")
       <|> ((string "tag" *> return `Tag) <?> "tag")
       <|> ((string "tree" *> return `Tree) <?> "tree"))
      <* commit

    let int64 =
      let open Angstrom in
      take_while (function '0' .. '9' -> true | _ -> false)
      >>| Int64.of_string
      <* commit

    let decoder =
      let open Angstrom in
      kind <* take 1
      >>= fun kind -> int64 <* advance 1
      >>= fun length -> match kind with
      | `Commit -> Commit.A.decoder >>| fun commit -> Commit commit
      | `Blob   -> Blob.A.decoder   >>| fun blob -> Blob blob
      | `Tree   -> Tree.A.decoder   >>| fun tree -> Tree tree
      | `Tag    -> Tag.A.decoder    >>| fun tag -> Tag tag
  end

  module F =
  struct
    type nonrec t = t

    let length = function
      | Commit commit -> Commit.F.length commit
      | Tag tag       -> Tag.F.length tag
      | Tree tree     -> Tree.F.length tree
      | Blob blob     -> Blob.F.length blob

    let string_of_value = function
      | Commit _ -> "commit"
      | Blob _   -> "blob"
      | Tree _   -> "tree"
      | Tag _    -> "tag"

    let int64 e x = Farfadet.string e (Int64.to_string x)

    let sp = ' '
    let nl = '\000'

    let encoder e x =
      let open Farfadet in

      eval e [ !!string; char $ sp; !!int64; char $ nl ]
        (string_of_value x) (length x);

      match x with
      | Commit commit -> Commit.F.encoder e commit
      | Blob blob     -> Blob.F.encoder e blob
      | Tag tag       -> Tag.F.encoder e tag
      | Tree tree     -> Tree.F.encoder e tree
  end

  module M =
  struct
    type nonrec t = t

    open Minienc

    let length = function
      | Blob blob -> Blob.F.length blob
      | Tree tree -> Tree.F.length tree
      | Tag tag -> Tag.F.length tag
      | Commit commit -> Commit.F.length commit

    let string_of_value = function
      | Commit _ -> "commit"
      | Blob _   -> "blob"
      | Tree _   -> "tree"
      | Tag _    -> "tag"

    let sp = ' '
    let nl = '\000'

    let encoder x k e =
      (write_string (string_of_value x)
       @@ write_char sp
       @@ write_string (Int64.to_string (length x))
       @@ write_char nl
       @@ (match x with
           | Tree tree -> Tree.M.encoder tree
           | Tag tag -> Tag.M.encoder tag
           | Blob blob -> write_bigstring (Cstruct.to_bigarray (blob :> Cstruct.t))
           | Commit commit -> Commit.M.encoder commit) k)
      e
  end

  module D = Helper.MakeInflater(Inflate)(A)
  module E = Helper.MakeDeflater(Deflate)(M)

  let digest = function
    | Blob blob     -> Blob.digest blob
    | Commit commit -> Commit.digest commit
    | Tree tree     -> Tree.digest tree
    | Tag tag       -> Tag.digest tag

  let equal   = (=)
  let hash    = Hashtbl.hash

  let int_of_kind = function
    | Commit _ -> 0
    | Tree _ -> 1
    | Blob _ -> 2
    | Tag _ -> 3

  let compare a b = match a, b with
    | Commit a, Commit b -> Commit.compare a b
    | Blob a, Blob b -> Blob.compare a b
    | Tree a, Tree b -> Tree.compare a b
    | Tag a, Tag b -> Tag.compare a b
    | a, b ->
      if int_of_kind a > int_of_kind b
      then (-1)
      else if int_of_kind a < int_of_kind b
      then 1
      else if F.length a > F.length b
      then (-1)
      else if F.length a < F.length b
      then 1
      else Pervasives.compare a b

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end
