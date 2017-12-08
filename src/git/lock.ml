(***********************************************************************)
(*                                                                     *)
(*                            Weaktbl                                  *)
(*                                                                     *)
(*             (C) 2007 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)

(* XXX: remove *)

module Stack = struct
  type 'a t =
    { mutable data : 'a Weak.t
    ; mutable length : int
    ; mutable cursor : int }

  let create n =
    let len = min n (Sys.max_array_length - 1) in
    { data = Weak.create len
    ; length = len
    ; cursor = 0 }

  let length s =
    let flag = ref false
    and pt = ref 0 in
    for i = 0 to s.cursor - 1
    do match Weak.get s.data i with
      | Some _ as d ->
        if !flag
        then Weak.set s.data !pt d;
        incr pt
      | None -> flag := true
    done;
    s.cursor <- !pt;
    s.cursor

  let rec push x s =
    if s.cursor < s.length
    then (Weak.set s.data s.cursor (Some x);
          s.cursor <- s.cursor + 1)
    else
      let len = length s in
      if len >= s.length / 3 && len < s.length * 2 / 3
      then push x s
      else
        let len' = min (len * 3 / 2 + 2) (Sys.max_array_length - 1) in
        if len' = len
        then failwith "Weaktbl.Stack.push: stack connot grow"
        else
          let data' = Weak.create len' in
          Weak.blit s.data 0 data' 0 s.cursor;
          s.data <- data';
          s.length <- len';
          push x s

  let rec pop s =
    if s.cursor <= 0
    then raise Not_found;

    s.cursor <- s.cursor - 1;

    match Weak.get s.data s.cursor with
    | Some x -> x
    | None -> pop s

  let rec top s =
    if s.cursor <= 0
    then raise Not_found;

    match Weak.get s.data (s.cursor - 1) with
    | Some x -> x
    | None ->
      s.cursor <- s.cursor - 1;
      top s
end

module Make (H : Hashtbl.HashedType) = struct

  type box = H.t Weak.t

  let inject k =
    let w = Weak.create 1 in
    Weak.set w 0 (Some k); w
  let project bk =
    Weak.get bk 0

  type bind = box * Obj.t

  let bind_new k v = inject k, Obj.repr v

  type cls = bind Stack.t

  let cls_new bd =
    let cls = Stack.create 1 in
    Stack.push bd cls; cls

  let dummy k =
    cls_new (bind_new k ())

  let rec top_bind cls =
    let (bk, v) as bind = Stack.top cls in
    match project bk with
    | Some k -> k, (Obj.obj v)
    | _ ->
      assert (bind == Stack.pop cls);
      top_bind cls

  let top_key cls =
    fst (top_bind cls)
  and top_value cls =
    snd (top_bind cls)

  module HX = struct
    type t = cls
    let hash x = try H.hash (top_key x) with Not_found -> 0
    let equal x y = try H.equal (top_key x) (top_key y) with Not_found -> false
  end

  module WX = Weak.Make(HX)

  type 'a t = WX.t

  let create = WX.create

  let find tbl key =
    top_value (WX.find tbl (dummy key))

  let add tbl key data =
    let bd = bind_new key data in
    let cls =
      try let c = WX.find tbl (dummy key) in Stack.push bd c; c
      with Not_found -> let c = cls_new bd in WX.add tbl c; c
    in
    let final _ = ignore bd; ignore cls in
    try Gc.finalise final key
    with Invalid_argument _ ->
      Gc.finalise final bd;
      Gc.finalise final cls

end

module H = Make(struct
    type t = Fpath.t
    let equal = Fpath.equal
    let hash = Hashtbl.hash
  end)

type t = elt H.t
and elt = Lwt_mutex.t

let v _ = H.create 24

let make locks path =
  try H.find locks path
  with Not_found ->
    let m = Lwt_mutex.create () in
    H.add locks path m;
    m

let lock m = Lwt_mutex.lock m
let unlock m = Lwt_mutex.unlock m |> Lwt.return

let with_lock m f =
  match m with
  | None -> f ()
  | Some m -> Lwt_mutex.with_lock m f
