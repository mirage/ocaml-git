module type S =
sig
  include (module type of Fpath)
  (* XXX(dinosaure): lint [Fpath]. *)
end
