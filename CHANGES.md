### 1.5.2 (2015-07-04)

* Fix handling of empty paths (#89)
* Fix the serialization of dates in commit objects
* Expose `Git.Packed_value.PIC.pretty`
* Improve the efficiency of `Git_unix.FS.remove`
* Support shallow packs (#81)
* Fix an mmap leak introduced in `1.5.*` (#90)
* Remove the dependency to OUnit for the tests
* Improve the pretty printers and the output of `ogit`

### 1.5.1 (2015-06-18)

* Fix filesystem expansion when a filen ame becomes a directory name or when
  a directory name becomes a file name (#87)
* Fix the order of entries in the tree objects (#86)
* Fix the compilation of tests (#85)
* Fetch all remote refs on synchronize (#83, by @AltGr)

### 1.5.0 (2015-06-12)

* Compatibility with `cohttp.0.18.` (#80 by @rgrinberg)
* Simplify the mirage sync API to use `conduit 0.8.4` (breaking API changes)
* Change `ogit cat-file` to behave exactly as `git cat-file`
  The previous command is renamed to `ogit cat` (#75 by @codinuum)
* `ogit` now supports short hashes instead of full SHA1 (#75 by @codinuum)
* Add `Git.Pack.Raw.read` to read raw pack files (#75 by @codinuum)
* `Git.Pack_index.t` now uses a cache of entries. This is more efficient
  than the previous representation (#75 by @codinuum)
* Add `Git.Pack_index.mem` to find an entry in the pack index cache
  (#75 by @codinuum)
* Add `Git.Pack_index.find_offset` to find an offset in the pack index
  cache (#75 by @codinuum)
* Add `Git.Packed_value.to_value` to unpack a value stored in a pack file
  (#75 by @codinuum)
* Support synchronisation for MirageOS unikernels (#70)

### 1.4.11 (2015-03-11)

* Fix multi round-trips in the smart HTTP protocol. This fixes
  depth-limited clones (#71) and fetches.
* Create the `git.http` library for abstracting away bits of the
  smart HTTP protocol.
* Add `User-Agent` in the headers of the smart HTTP protocol. This
  makes `bitbucket.org` happy. (#66, patch from @vklquevs)

### 1.4.10 (2015-02-05)

* Fix support for the smart HTTP protocol (report by @talex5,
  mirage/irmin#138)

### 1.4.9 (2015-02-04)

* Remove the `OGITTMPDIR` and alway store temp files under
  `git/tmp` (mirage/irmin#132)

### 1.4.8 (2015-02-04)

* Fix LRU cache: SHA1 should be unique in the cache (regression
  introduced in 1.4.3). This was causing confusing read results
  under load.
* Reading objects now updates the LRU cache
* Fix a regression in `ogit cat-file` which were displaying nothing
  for small objects.

### 1.4.7 (2015-02-03)

* Fix the filesystem updates for non-bare repositories (reported by @avsm)
* `Git.write_index` now takes an optional `index` argument
* Index entries should be fixed alphabetically
* Remove raw printf (#60)
* More flexibility on where to write temp files. The directory name can be
  configured by write calls, and the default is `OGITTMPDIR` if set,
  then `Filename.get_temp_dir_name` -- as it was in 1.4.5, see #51

### 1.4.6 (2015-01-29)

* Expose `Git.Value.Cache.set_size` to change the LRU cache size
* Reduce the default LRU cache size (in 1.4.4 it was set to 64k, now it's 512)
* More precise type for commit dates
* Add `git.top` to load toplevel printers for Git values

### 1.4.5 (2015-01-19)

* Support `packed-refs` files, to read references packed by `git gc` (reported
  by Gregory Tsipenyuk)
* Fix the filesystem backend when TMPDIR is not on the same partition as the
  Git repository (#51, patch from @vklquevs)

### 1.4.4 (2015-01-12)

* Support the smart HTTP Git protocol (#26)
* Best-effort creation of files when expanding the index into the filesystem:
  Skip the invalid filenames and continue. Users are expected to sanitize
  their filenames if they want to use a non-bare repository (#11)
* Overwrite changed file when expanding the index into the filesystem (#4)
* Do not recompute the hash of blob files when expanding the index into the
  filesystem. This help fixing a speed issue with non-bare repo with lots of
  file.
* Rename `{write,read}_cache` to `{write,read}_index`
* Rename Cache to Index
* Expose the protocol capabilities to the client
* Support side-band-64k protocol capability (#44)
* Fix support for git+ssh (#39)
* Expose zlib compression level (#41)
* Maintain a cache of opened files (#29, Pierre Chambart)

### 1.4.3 (2014-12-19)

* Fix regression introduced in 1.4.3 appearing when synchronising big
  repositories (#38)
* Fix concurrent read/write by using an atomic rename (#35)
* Tree objects can also point to commits (@codinuum)
* Reduce allocation (@codinuum)
* Use LRU cache instead of an unbounde Hashtbl (code imported for
  Simon Cruanes's CCache implementation)
* Remove the crazy unbounded caching in Git.FS. Use the LRU everywhere (#22)
* Fix fd leaking (#29)
* Update to dolog.1.0
* Remove dependency to camlp4
* Remove lots of warnings
* Move `Git_unix` and `Git_mirage` in their own subdirs as it was causing issues
  to oasis (#5, Simon Cruanes)
* Use `Bytes` instead of `String` (#5, Simon Cruanes)

### 1.4.2 (2014-12-14)

* Fix `Git_unix.IO.write_file` to work on empty files

### 1.4.1 (2014-12-4)

* Fix `ogit --version` (#22)
* Expose the backend type
* Expose Git_unix.Sync.IO

### 1.4.0 (2014-11-20)

* Port to Conduit 0.6.0 API.
* Depend on `ocaml-hex`

### 1.3.0 (2014-10-16)

* Remove the dependency on `core_kernel`
* Use `ocaml-nocrypto` instead of `ocaml-sha1`

### 1.2.0: (2014-06-10)

* Can consume Mirage's `V1_LWT.FS` signature to generate a
  persistent store. This allows to store Git repos directly
  inside raw block devices (no need of filesystem anymore).
* Minor API refactoring to abstract the Unix layer cleanly.
* Expose a filesystem functor to create filesystem backends
  independent of Unix.
* Simplify the ocamlfind packages: there's only `git` and `git.unix`.

### 1.1.0: (2014-06-02)

* Support for push (not optimized at all)
* Fix the generation of `.dot` file representing the Git repo
* Fix serialization of executable files in the cache
* Fix reading the total number of keys in a pack index file
* Use `ocaml-conduit` to set-up connections with remote repositories
* USe `ocaml-uri` to specify Git Remote Identifiers
* Depend the implementation of patience diff in pure OCaml (unused for now)

### 1.0.2: (2014-04-19)

* Catch, improve and propagate Zlib inflation errors (which usually pop-ups
  on incomplete files)

### 1.0.1: (2014-04-10)

* Escape invalid chars in path names
* Do not link with camlp4 when using as a libray

### 1.0.0: (2014-02-10)

* Support for reading and writing pack indexes
* Support for reading and writing pack files
* Refactor the API: each Git object has now its own file, with a consistent
  signature
* `ogit clone` now correctly set-up the cache file (which means that
  `git status` and `git diff` will now work as expected on a repository created
   by `ogit`)
* Add `ogit read-tree COMMIT`
* Add `ogit ls-files [--all]`
* Support for reading and writing cache files

### 0.10.2: (2014-01-20)

* Strip the contents of references file
* Improve the pretty-printing of SHA1 values
* Add some info message when reading files in the local backend

### 0.10.1: (2014-01-15)

* Add missing files (fix build)
* Add `GitTypes.S.mem_reference`
* Add `GitTypes.S.remove_reference`
* Add `GitTypes.S.mem` to check if an object exists in the store

### 0.10.0: (2014-01-14)

* Support for in-memory stores
* Add `ogit cat-file`
* Add `ogit ls-remote`
* Add `ogit fetch`
* Add `ogit clone`
* Switch non-blocking IO using Lwt

### 0.9.0: (2014-01-04)

* Initial release
