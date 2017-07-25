type 'a t =
  { bulk        : 'a option array
  ; mutable idx : int
  ; len         : int }

let make len =
  let bulk = Array.init len (fun _ -> None) in
  { bulk
  ; idx = 0
  ; len }

let add t obj =
  Array.set t.bulk t.idx (Some obj); (* replace *)
  t.idx <- (t.idx + 1) mod t.len

let iter t f =
  Array.iter (function Some obj -> f obj | None -> ()) t.bulk

exception Find

let find t predicate =
  let obj = ref None in
  try
    iter t
      (fun x ->
        if predicate x
        then begin
          obj := Some x;
          raise Find
        end);
    None
  with Find -> !obj
