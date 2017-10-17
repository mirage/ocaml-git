module type STORE =
sig
  module Hash
    : S.HASH
  module Path
    : S.PATH
  module Value
    : Value.S
      with module Hash = Hash

  type t
  type error

  val pp_error : error Fmt.t
  val read : t -> Hash.t -> (Value.t, error) result Lwt.t
end

module Make (S : STORE) =
struct
  open S

  module Log =
  struct
    let src = Logs.Src.create "git.traverse" ~doc:"logs git's traverse event"
    include (val Logs.src_log src : Logs.LOG)
  end

  (* XXX(dinosaure): convenience and common part between the
     file-system and the mem back-end - to avoid redundant code. *)

  let fold t (f : ('acc -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'acc Lwt.t)) ~path acc hash =
    let names = Hashtbl.create 0x100 in

    let open Lwt.Infix in

    let rec walk close rest queue acc =
      match rest with
      | [] ->
        (match Queue.pop queue with
         | rest -> walk close [ rest ] queue acc
         | exception Queue.Empty -> Lwt.return acc)
      | hash :: rest ->
        if Hash.Set.exists ((=) hash) close
        then walk close rest queue acc
        else
          let close' = Hash.Set.add hash close in

          read t hash >>= function
          | Ok (Value.Commit commit as value) ->
            let rest' = Value.Commit.tree commit :: rest in
            List.iter (fun x -> Queue.add x queue) (Value.Commit.parents commit);
            f acc ~length:(Value.Commit.F.length commit) hash value >>= fun acc' ->
            walk close' rest' queue acc'
          | Ok (Value.Tree tree as value) ->
            let path = try Hashtbl.find names hash with Not_found -> path in
            Lwt_list.iter_s (fun { Value.Tree.name; node; _ } ->
                Hashtbl.add names node Path.(path / name)[@warning "-44"];
                Lwt.return ()) (Value.Tree.to_list tree) >>= fun () ->
            let rest' = rest @ List.map (fun { Value.Tree.node; _ } -> node) (Value.Tree.to_list tree) in
            f acc ~name:path ~length:(Value.Tree.F.length tree) hash value >>= fun acc' ->
            walk close' rest' queue acc'
          | Ok (Value.Blob blob as value) ->
            let path = try Hashtbl.find names hash with Not_found -> path in
            f acc ~name:path ~length:(Value.Blob.F.length blob) hash value >>= fun acc' ->
            walk close' rest queue acc'
          | Ok (Value.Tag tag as value) ->
            Queue.add (Value.Tag.obj tag) queue;
            f acc ~length:(Value.Tag.F.length tag) hash value >>= fun acc' ->
            walk close' rest queue acc'
          | Error err ->
            Log.err (fun l -> l ~header:"fold" "Retrieve an error when we try to read the Git object %a: %a."
                        Hash.pp hash pp_error err);
            walk close' rest queue acc
    in

    walk Hash.Set.empty [ hash ] (Queue.create ()) acc
end
