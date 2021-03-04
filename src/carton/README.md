# Carton, a library to manipulate PACK files

A PACK file is an archive of several objects. The goal of it is to store many
objects in a light weight way. PACK files are used by Git and the distribution
provides some tools to introspect them:

## carton.get

On your Git repository:

```sh
$ ls .git/objects/pack
pack-066646d20f899ba23e2839340a04c9e0aa87e2e7.idx
pack-066646d20f899ba23e2839340a04c9e0aa87e2e7.pack
$ carton.get .git/objects/pack/*.pack 12
00000000: 7472 6565 2064 3138 6130 3161 3837 6262  tree d18a01a87bb
00000010: 3033 3431 6662 6230 6564 6234 6633 3532  0341fbb0edb4f352
00000020: 3233 6564 6633 3365 6163 3761 630a 7061  23edf33eac7ac.pa
00000030: 7265 6e74 2038 3437 3739 3433 6564 6465  rent 8477943edde
00000040: 3861 6563 3233 3237 3035 3235 3864 3938  8aec232705258d98
00000050: 3963 3938 3766 6164 6562 3132 390a 6175  9c987fadeb129.au
00000060: 7468 6f72 2064 696e 6f73 6175 7265 203c  thor dinosaure <
00000070: 726f 6d61 696e 2e63 616c 6173 6369 6265  romain.calascibe
00000080: 7474 6140 676d 6169 6c2e 636f 6d3e 2031  tta@gmail.com> 1
00000090: 3631 3431 3636 3333 3020 2b30 3130 300a  614166330 +0100.
000000a0: 636f 6d6d 6974 7465 7220 6469 6e6f 7361  committer dinosa
000000b0: 7572 6520 3c72 6f6d 6169 6e2e 6361 6c61  ure <romain.cala
000000c0: 7363 6962 6574 7461 4067 6d61 696c 2e63  scibetta@gmail.c
000000d0: 6f6d 3e20 3136 3134 3837 3638 3735 202b  om> 1614876875 +
000000e0: 3031 3030 0a0a 4164 6420 736f 6d65 2062  0100..Add some b
000000f0: 696e 6172 6965 7320 746f 2073 686f 7720  inaries to show 
00000100: 686f 7720 746f 2075 7365 2063 6172 746f  how to use carto
00000110: 6e0a                                     n.
```

You extracted a Git object at the offset `12` into the only PACK file of your
Git repository. You can have several informations such as:
- the depth of the object
- the path to re-construct it
- the length
- the kind

Several informations are not _like Git_ but they are similar. Indeed, carton
does not know about Git objects and it does not have clues to understand them
such as a _commit_.

In our case, the kind:
- `a` is a commit
- `b` is a tree
- `c` is a blob
- `d` is a tag

The path and the depth is how many objects we need to reconstruct to finally
reconstruct the requested objects. Indeed, the PACK file compresses objects
together. By this way, similar objects are highly compressed.

## carton.index-pack

On your Git repository, you can look at the `*.idx` file. This file is an
_index_ of the PACK file and it allows a fast access on it. When the user
requests an object with its hash, we do a lookup on the index file.

Carton is able to (re-)generate the index file:
```sh
$ carton.index-pack .git/objects/pack/*.pack -o pack.idx
066646d20f899ba23e2839340a04c9e0aa87e2e7
$ diff pack.idx .git/objects/pack/*.idx
```

Depending on your PACK file (if it's huge or not), the process can take a time.
The program generates a index file `pack.idx` which is strictly the same as
the index of your Git repository.

## carton.verify-pack

Of course, from a given index file, we can check an entire PACK file:
```sh
$ carton.verify-pack .git/objects/pack/*.idx
```

This program produces the same output as `git verify-pack`
