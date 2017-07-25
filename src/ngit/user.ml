type tz_offset =
  { sign    : [ `Plus | `Minus ]
  ; hours   : int
  ; minutes : int }

type t =
  { name  : string
  ; email : string
  ; date  : int64 * tz_offset option }

let pp_sign fmt = function
  | `Plus -> Format.fprintf fmt "`Plus"
  | `Minus -> Format.fprintf fmt "`Minus"

let pp_tz_offset fmt { sign; hours; minutes; } =
  Format.fprintf fmt "{ @[<hov>sign = @[<hov>%a@];@ \
                               hours = %02d;@ \
                               minutes = %02d;@] }"
    pp_sign sign hours minutes

let pp_option pp_data fmt = function
  | Some x -> pp_data fmt x
  | None -> Format.fprintf fmt "<none>"

let pp fmt { name; email; date = (n, tz_offset) } =
  Format.fprintf fmt "{ @[<hov>name = @[<hov>%s@];@ \
                               email = @[<hov>%s@];@ \
                               date = @[<hov>(%Ld, %a)@];@] }"
    name email n (pp_option pp_tz_offset) tz_offset

module A =
struct
  type nonrec t = t

  let sp = Angstrom.char ' '
  let pl = Angstrom.char '+'
  let mn = Angstrom.char '-'

  let is_not_lt chr = chr <> '<'
  let is_not_gt chr = chr <> '>'

  let int64 =
    let open Angstrom in
    take_while (function '0' .. '9' -> true | _ -> false) >>| Int64.of_string

  let decoder =
    let open Angstrom in
    take_while is_not_lt <* take 1 <* commit
    >>= fun name    -> let name = String.sub name 0 (String.length name - 1) in
                        take_while is_not_gt <* commit
    >>= fun email   -> take 2 *> int64 <* commit
    >>= fun second  -> sp *> ((pl *> return `Plus)
                                <|> (mn *> return `Minus))
                        <* commit
    >>= fun sign    -> take 2 >>| int_of_string <* commit
    >>= fun hours   -> take 2 >>| int_of_string <* commit
    >>= fun minutes ->
      let tz_offset =
        if sign = `Plus
        && hours = 0
        && minutes = 0
        then None else Some { sign
                            ; hours
                            ; minutes }
      in
      return { name
             ; email
             ; date = (second, tz_offset) }
    <* commit
end

module F =
struct
  type nonrec t = t

  let length t =
    let string x = Int64.of_int (String.length x) in
    let ( + ) = Int64.add in

    let tz_offset_length = 5L in
    (string t.name) + 1L + 1L + (string t.email) + 1L + 1L + (string (Int64.to_string (fst t.date))) + 1L + tz_offset_length

  open Farfadet

  let lt = '<'
  let gt = '>'
  let sp = ' '

  let int64 e x = string e (Int64.to_string x)

  let digit e x =
    if x < 10
    then eval e [ char $ '0'; !!string ] (string_of_int x)
    else if x < 100
    then string e (string_of_int x)
    else raise (Invalid_argument "User.F.digit")

  let sign' e = function
    | `Plus -> char e '+'
    | `Minus -> char e '-'

  let date e = function
    | None -> string e "+0000"
    | Some { sign; hours; minutes } ->
      eval e [ !!sign'; !!digit; !!digit ] sign hours minutes

  let encoder e t =
    eval e [ !!string; char $ sp; char $ lt; !!string; char $ gt; char $ sp; !!int64; char $ sp; !!date ]
      t.name t.email (fst t.date) (snd t.date)
end

module M =
struct
  type nonrec t = t

  open Minienc

  let lt = '<'
  let gt = '>'
  let sp = ' '

  let write_int64 x k e = write_string (Int64.to_string x) k e

  let write_digit x k e =
    if x < 10
    then (write_char '0' @@ write_string (string_of_int x) k) e
    else if x < 100
    then write_string (string_of_int x) k e
    else raise (Invalid_argument "User.M.digit")

  let write_sign x k e = match x with
    | `Plus -> write_char '+' k e
    | `Minus -> write_char '-' k e

  let write_date x k e = match x with
    | None -> write_string "+0000" k e
    | Some { sign; hours; minutes; } ->
      (write_sign sign
       @@ write_digit hours
       @@ write_digit minutes k)
        e

  let encoder x k e =
    (write_string x.name
     @@ write_char sp
     @@ write_char lt
     @@ write_string x.email
     @@ write_char gt
     @@ write_char sp
     @@ write_int64 (fst x.date)
     @@ write_char sp
     @@ write_date (snd x.date) k)
    e
end

module D = Helper.MakeDecoder(A)

let equal   = (=)
let compare = Pervasives.compare
let hash    = Hashtbl.hash

module Set = Set.Make(struct type nonrec t = t let compare = compare end)
module Map = Map.Make(struct type nonrec t = t let compare = compare end)
