include S

module type S = Minimal.S
module type HASH = HASH
module type BASE = BASE
module type DIGEST = DIGEST

module Blob = Blob
module Commit = Commit
module Tree = Tree
module Tag = Tag
module Reference = Reference
module User = User
module Value = Value
module Traverse_bfs = Traverse_bfs
module Object_graph = Object_graph
module Search = Search
module Mem = Mem
module Store = Store
module Hash = Hash
module Sync = Sync
module Cstruct_append = Cstruct_append
