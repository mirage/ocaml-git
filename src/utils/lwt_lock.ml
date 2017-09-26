module Stack =
struct
  type 'a t =
    { mutable data : 'a Weak.t
    ; mutable length : int
    ; mutable cursor : int }

  let create n =
    let len = min n (Sys.max_array_length - 1) in
    { data = Weak.create len
    ; length = len
    ; cursor = 0 }

  let iter f s =
    for i = s.cursor - 1 downto 0
    do match Weak.get s.data i with
      | Some x -> f x
      | _ -> ()
    done

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

  let copy s =
    let s' = create s.length in
    Weak.blit s.data 0 s'.data 0 s.cursor;
    s'.cursor <- s.cursor;
    s'

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

module Make (H : Hashtbl.HashedType)
  : Hashtbl.S with type key = H.t
= struct
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

  let all_bind cls =
    let l = ref [] in
    let f (bk, v) = match project bk with
      | Some k ->
        l := (k, Obj.obj v) :: !l
      | _ -> ()
    in Stack.iter f cls;
    List.rev !l

  let all_value cls =
    List.map snd (all_bind cls)

  module HX =
  struct
    type t = cls

    let hash x =
      try H.hash (top_key x)
      with Not_found -> 0

    let equal x y =
      try H.equal (top_key x) (top_key y)
      with Not_found -> false
  end

  module WX = Weak.Make(HX)

  type key = H.t and 'a t = WX.t

  let create =
    WX.create
  and clear =
    WX.clear

  let find_all tbl key =
    try all_value (WX.find tbl (dummy key))
    with Not_found -> []

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

  let remove tbl key =
    try ignore (Stack.pop (WX.find tbl (dummy key)))
    with Not_found -> ()

  let replace tbl key data =
    remove tbl key;
    add tbl key data

  let mem tbl key =
    try ignore (find tbl key); true
    with Not_found -> false

  let iter f tbl =
    let f' (bk, v) = match project bk with Some k -> f k (Obj.obj v) | None -> () in
    WX.iter (Stack.iter f') tbl

  let fold f tbl acc =
    let acc' = ref acc in
    let f' k v = acc' := f k v !acc' in
    iter f' tbl; !acc'

  let length tbl = WX.fold (fun cls -> (+) (Stack.length cls)) tbl 0

  let copy tbl =
    let tbl' = WX.create (WX.count tbl * 3 / 2 + 2) in
    WX.iter (fun cls -> WX.add tbl' (Stack.copy cls)) tbl;
    tbl'

  let filter_map_inplace f tbl =
    let delta = ref [] in

    iter
      (fun k v -> match f k v with
         | Some v' when v' == v -> ()
         | other -> delta := (k, other) :: !delta) tbl;
    let handle_delta = function
      | (k, None) -> remove tbl k
      | (k, Some v) -> remove tbl k; add tbl k v
    in
    List.iter handle_delta !delta

  let stats _ = assert false
  let reset _ = assert false
end

module H = Make(struct type t = Fpath.t let equal = Fpath.equal let hash = Hashtbl.hash end)

type +'a io = 'a Lwt.t

type t = elt H.t
and key = H.key
and elt = Lwt_mutex.t

let locks = H.create 24

let make locks' path =
  assert (locks == locks');

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
