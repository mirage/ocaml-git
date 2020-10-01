module type HASH = S.HASH
module type BASE = S.BASE
module type DIGEST = S.DIGEST

module Blob = Blob
module Commit = Commit
module Tree = Tree
module Tag = Tag
module User = User
module Reference = Reference
module Value = Value
module Traverse_bfs = Traverse_bfs
module Object_graph = Object_graph
module Search = Search
module Mem = Mem
module Store = Store
module Hash = Hash
module Sync = Sync
module Cstruct_append = Cstruct_append

module type S = Minimal.S
