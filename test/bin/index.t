Generate index file
  $ carton.index-pack -o bomb.idx ../carton/bomb.pack
  d1c2ce2fc6dfaaa18d0ea1b564334d738b0e2339
  $ diff bomb.idx ../carton/bomb.idx
  $ carton.index-pack < ../carton/bomb.pack > bomb.idx
  $ diff bomb.idx ../carton/bomb.idx
  $ carton.index-pack -o bomb.idx < ../carton/bomb.pack
  d1c2ce2fc6dfaaa18d0ea1b564334d738b0e2339
  $ diff bomb.idx ../carton/bomb.idx
  $ carton.index-pack -v -o bomb.idx ../carton/bomb.pack
  Indexing objects:   0% (0/18)Indexing objects:   5% (1/18)Indexing objects:  11% (2/18)Indexing objects:  16% (3/18)Indexing objects:  22% (4/18)Indexing objects:  27% (5/18)Indexing objects:  33% (6/18)Indexing objects:  38% (7/18)Indexing objects:  44% (8/18)Indexing objects:  50% (9/18)Indexing objects:  55% (10/18)Indexing objects:  61% (11/18)Indexing objects:  66% (12/18)Indexing objects:  72% (13/18)Indexing objects:  77% (14/18)Indexing objects:  83% (15/18)Indexing objects:  88% (16/18)Indexing objects:  94% (17/18)Indexing objects: 100% (18/18), done.
  Resolving deltas:   0% (0/3).Resolving deltas:  33% (1/3).Resolving deltas:  66% (2/3).Resolving deltas: 100% (3/3), done.
  d1c2ce2fc6dfaaa18d0ea1b564334d738b0e2339
