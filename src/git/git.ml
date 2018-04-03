include S
module type S = Minimal.S

module Crc32        = Crc32
module Error        = Error
module Helper       = Helper

module Blob         = Blob
module Commit       = Commit
module Tree         = Tree
module Tag          = Tag
module Reference    = Reference
module User         = User
module Value        = Value

module Fanout       = Fanout
module Bucket       = Bucket
module Rabin        = Rabin

module Minienc      = Minienc

module Traverse_bfs = Traverse_bfs
module Gc           = Collector
module Object_graph = Object_graph
module Search       = Search
module Negociator   = Negociator
module Revision     = Revision

module Capability   = Capability
module Sync         = Sync
module Smart        = Smart

module Loose        = Loose
module Pack_info    = Pack_info
module Pack_engine  = Pack_engine
module Packed_refs  = Packed_refs
module Pack         = Pack
module Unpack       = Unpack
module Index_pack   = Index_pack

module Mem          = Mem
module Store        = Store

module Inflate      = Inflate
module Deflate      = Deflate
module Buffer       = Cstruct_buffer
module Hash         = Hash
