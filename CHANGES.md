### 3.14.0 (2023-09-30) Paris - France

- Fix tests with `git.2.41.0+` (@metanivek, #622)
- Fix raw parsing in the middle of the buffer (@metanivek, #623)

### 3.13.0 (2023-03-24) Paris - France

- Update tests with `mirage-crypto-rng` (@Zimmi48, #612)
- Update to `awa.0.2.0` (@hannesm, #613)

### 3.12.0 (2023-02-13) Paris - France

- Export a new function to close file-descriptors for PACK file (for git-unix)
  (@dinosaure, @emillon, @moyodiallo, #590)

### 3.11.0 (2023-01-10) Paris - France

- Limit the stream between the incoming flow and the PACK analyzer
  It will bind the analyze with the I/O throughput (@dinosaure, #608)
- Remove OCaml 5.0.0 warnings (@samoht, #609)

### 3.10.1 (2022-12-07) Paris - France

- Add the NixOS CI to test `ocaml-git` (@Et7f3, #600, #473)
- Specify few more headers fields when we fetch/push via HTTP (@reynir, @hannesm, @dinosaure, #603, #601)
- Improve the error message about `Smart_git.Endpoint.of_string` (@reynir, @dinosaure, #605, #604)

### 3.10.0 (2022-10-19) Paris - France

- Do not append a leading slash to path (#580, @reynir, @dinosaure)
- Fix some typos (#585, @hannesm, @dinosaure)
- Add the `main` reference (#586, @hannesm, @dinosaure)
- Upgrade `git-paf` with `paf.0.2.0` (#587, @dinosaure)
- Explain when the user give a bad argument about TLS authenticator (#593, reported by @reynir #582, @dinosaure)
- Use `x509.0.16.2` which raise an explanation if it's a bad argument (#594, @hannesm, @dinosaure)

### 3.9.1 (2022-04-11) Paris - France

- Remove `bigarray-compat` package and `mmap` (@dinosaure, @hannesm)
- Fix the push process with a copy of illegal shared bigarray (@dinosaure, #569)

### 3.9.0 (2022-03-24) Paris - France

- Fix `ocaml-git` with OCaml 5.00.0 (@dinosaure, @stedolan, #559)
- Update binaries with `cmdliner.1.1.0` (@MisterDA, @dinosaure, #558)
- Fix the `reset` function and properly delete PACK files (@dinosaure, @patricoferris, @dinakajoy, #561)
- Fix dependencies of `guit` (@maiste, @dinosaure, #560)
- Move the `happy-eyeballs` layer needed by MirageOS into `mimic.0.0.5` (@dinosaure, #563)

### 3.8.1 (2022-04-08) Paris - France

- Fix the push process with a copy of illegal shared bigarray (@dinosaure, 5e5392f)

### 3.8.0 (2022-02-03) Paris - France

- Adapt `git-mirage` with `awa.0.1.0` (@hannesm, #553)
- Support new version of `git` into tests (@dinosaure, #532, #554)
- Provide `Store.read_opt` for finding Git object (@CraigFe, #551)
- Unify `git-mirage-http` and `git-mirage-ssh` about the _authenticator_ (@dinosaure, @hannesm, #538, #555)

### 3.7.1 (2022-04-07) Paris - France

- Fix the push process with a copy of illegal shared bigarray (@dinosaure, 2aeb677)

### 3.7.0 (2021-12-12) Paris - France

- Drop unneeded `mirage-protocols` dependency (@hannesm, #537)
- Delete HTTP functor and use happy-eyeballs (@dinosaure, #539)
- Be compatible with `mirage-protocols.7.0.0` (@dinosaure, #541)
- Use `Lwt.pause` instead of `Lwt_unix.yield` (@dinosaure, #542)
- Link with logs and `logs.fmt` (@MisterDA, #544)

### 3.6.0 (2021-10-20) Paris - France

- Fix the documentation (@dinosaure, #534)
- Remove some output from Git when we initialize a Git repository (@dinosaure, #534)
- Fix `fmt` deprecation functions (@dinosaure, #534)
- Use a warning instead of an error when a reference does not exists (@zshipko, @dinosaure, #533)
- Use the last version of `dns` (6.0.0) (@dinosaure, @hannesm, #531)

### 3.5.0 (2021-09-08) Paris - France

- Fix compilation of benchmarks (@dinosaure, #516)
- Remove `paf.cohttp` dependency from MirageOS stack (@dinosaure, #519)
- Use `Cstruct.length` instead of `Cstruct.len` (@dinosaure. #522)
- Update to `tls.0.14.0` (@dinosaure, #529)

### 3.4.0 (2021-22-04) Paris - France

- Fix several issues on `git-unix` (@dinosaure, @sternenseemann, #488)
- Fix set of references by `git-unix` (@dinosaure, @jnavila, #490, #489)
- Remove dependency with `curl` (@dinosaure, #491)
- Provide `Git_unix.ctx` (@dinosaure, #493)
- Update the plumbing between `carton.0.4.1` and `git` (@dinosaure, #493)
- Expose information used by `Git_unix` to start a connection (@dinosaure, #498)
- Add missing `fmt` and `logs` on several `dune` files (@CraigFe, @dinosaure, #499)
- Be able to pass threads argument when we fetch (@dinosaure, #500)
- Fix documentation on `search.ml` (@dinosaure, #501)
- Be compatible with `alcotest.1.4.0` (@dinosaure, #504)
- Delete `mimic` from the distribution (now available on https://github.com/dinosaure/mimic)
  (@dinosaure, #505)
- Fix temporary directories on unix tests (@dinosaure, #506)
- Use the `non-stream` API of `decompress` for _loose_ objects (@clecat, @dinosaure, #502)
- Use `git-paf` and `paf.0.0.2` instead of CoHTTP (@dinosaure, #508)
- Don't try to download tags implicitely (@dinosaure, #507)
- Fix bug about negotiation over HTTP connection (@dinosaure, #507)

### 3.3.3 (2021-21-03) Paris - France

- Fix stack-overflow on tree objects (@zshipko, @dinosaure, #485)

### 3.3.2 (2021-15-03) Paris - France

- Fix infinite loop when we don't have enough spaces for an
  inflated output (#480, @dinosaure, @Ngoguey42)
- Fix bug about ephemeron/`cstruct_append`/`O_TRUNC` (#481, @dinosaure)

### 3.3.1 (2021-05-03) Paris - France

- Fix tests on NixOS (@sternenseemann, #472)
- Fix report status over git://, ssh:// and http(s):// with
  side-band-64k and report-status capabilities (@dinosaure, @hannesm, #474)
- Be sure that creation of a tree is tail-rec (@dinosaure, @zshipko, #476)
- Update to decompress.1.3.0 (@dinosaure, #477)

### 3.3.0 (2021-17-02) Paris - France

- Fix tests when we push to an empty repository (#462, @dinosaure, @ulugbekna)
- Fix new smart tests without a global git config (#463, @sternenseemann, @dinosaure)
- Refactor tests (#464, @ulugbekna, @dinosaure)
- Refactor `find_common.ml` (#465, @ulugbekna, @dinosaure)
- Some preparation about Git protocol v2 (#466, @ulugbekna, @dinosaure)
- Add HTTP/HTTPS support on the unikernel example (#467, @dinosaure)
- Fix bug about push and capabilities (#468, @dinosaure, @hannesm, @ulugbekna)
- Implement HTTP push and handle username & password in the given `Uri.t` (#469, @dinosaure)
- **breaking changes**
  Commit object must have a double LF to separate the header and the body. This format changes
  hashes of commit and invalid old commits generated by `ocaml-git`. It seems that on the Git
  side, the format is not fully respected - and it's why Git did not complain about that on our
  tests for a long time.

  However, `git fsck` does this check and an HTTP push to GitHub run `git fsck` at the end. To
  be able to push (or run our _unikernel_ over HTTP/S), we must generate right commits.

  So, we advise users to make a fake commit with Git (`git commit --allow-empty -m.`) on
  repositories used by `ocaml-git` and fetch it with `depth:1`. Otherwise, this new version
  of `ocaml-git` will refute older commits - due to the inherent isomorphism between
  decoder/encoder in `ocaml-git`.

  For Irmin users, this breaking changes does not change anything when `irmin` reformats correctly
  the message to put the second LF. However, a breaking change exists on the API level when the
  Git commit (and a Git tag) expects a `string option` now (instead of a simple `string).

  For more details and tests, see #470 (@dinosaure)

### 3.2.0 (2021-06-02) Saint-Malo - France

- Fix windows support (@dinosaure, #445)
- Remove `dune subst` (@dinosaure, @kit-ty-kate, #446)
- Remove useless `Lwt_io` module intests (@dinosaure, @ulugbekna, #447)
- Fix bug when we _unixiz_ a `Mirage_flow.S` (@dinosaure, #450)
- Fix unikernel, the git functoria device must take properly the given `ctx` (@dinosaure, @hannesm, #452)
- Unmonad `mmap` (see `carton`) (@dinosaure, #454)
- Fix when we push first into a Git repository (@dinosaure, @hannesm, #455 & #456)
- Improve a bit the unikernel example (@dinosaure, #458)
- Improve the `git-mirage` support with `functoria` (@dinosaure, #459)

### 3.1.1 (2021-27-01) Paris - France

- Fix `git-unix` and PACK files location (@dinosaure, #444, #443)
- Initialise (as `git`) correctly (add an `HEAD` reference, at least) (@dinosaure, #443)

### 3.1.0 (2021-13-01) Paris - France

- Handle IP address on the endpoint (@dinosaure, #436)

### 3.0.0 (2021-08-01) Paris - France

- Rewrite of `ocaml-git` (@dinosaure, #395)
- Delete useless constraints on digestif's signature (@dinosaure, #399)
- Add support of CoHTTP with UNIX and MirageOS (@ulugbekna, #400)
- Add progress reporting on fetch command (@ulugbekna, #405)
- Lint dependencies on packages (`git-cohttp-unix` and `git-cohttp-mirage`)
  and update to the last version of CoHTTP (@hannesm, #407)
- Fix internal `Cstruct_append` implementation (@dinosaure, #401)
- Implement shallow commit (@dinosaure, #402)
- Update to `conduit.3.0.0` (@dinosaure, #408) (deleted by the integration of `mimic`)
- Delete use of `ocurl` (@dinosaure, #410)
- Delete the useless **old** `git-mirage` package (@hannesm, #411)
- Fix about unresolved endpoint with `conduit.3.0.0` (@dinosaure, #412)
- Refactors fetch command (@ulugbekna, #404)
- Fix ephemerons about temporary devices (@dinosaure, #413)
- Implementation of `ogit-fetch` as an example (@ulugbekna, #406)
- Rename `nss` to `git-nss` (@dinosaure, #415)
- Refactors `git-nss` (@ulugbekna, #416)
- Update README.md (@ulugbekna, #417)
- Replace deprecated `Fmt` functions (@ulugbekna, #421)
- Delete physical equality (@ulugbekna, #422)
- Rename `prelude` argument by `uses_git_transport` (@ulugbekna, #423)
- Refactors Smart decoder (@ulugbekna, #424)
- Constraint to use `fmt.0.8.7` (@dinosaure, #425)
- Small refactors in `git-nss` (@dinosaure, #427)
- Delete `conduit.3.0.0` and replace it by `mimic` (@dinosaure, #428)
- Delete the useless `verify` function on `fetch` and `push` (@dinosaure, #429)
- Delete `pin-depends` on `awa` (@dinosaure, #431)

### 2.1.3 (2020-30-06) Paris - France

- Move to `encoder.0.5` (#393, @dinosaure)
- Move to `angstrom.0.14.0` (#384, @sternenseemann)
- Add missing Time argument to `Resolver_mirage.Make_with_stack` (#389, @talex5)
- Fix .ocamlformat file (#389, @talex5)
- Enlarge internal buffer used to parse Git object (#389, @dinosaure)
- Update README.md (#383, @dinosaure)
- Use `mirage-crypto` instead `nocrypto` (#382, @hannesm)
- Add constraint on `dune` package (@kit-ty-kate)

### 2.1.2 (2019-14-11) Paris - France

- unlock `git` to use, at least, `checkseum.0.0.9` (#373, @dinosaure)
- remove build directive on dune dependency (#374, @CraigFe)
- adapt to MirageOS 3.7.0 (#376, @hannesm)
- fix infinite loop with index file (version 2) (#378, @kit-ty-kate, @dinosaure)

### 2.1.1 (2019-10-04) Somone - Sénégal

- Replace Pervasives by Stdlib and depends on `stdlib-shims` virtual package (@dinosaure, #372)
- Add `pp_fetch_one` and `pp_update_and_create` (@dinosaure, @pascutto, @samoht, #370)
- `git-mirage` depends on `conduit-mirage` instead `mirage-conduit` (@dinosaure, #371)
- Drop support of OCaml < 4.07.0
- Delete `mirage-fs-unix` useless dependency
- Constraint to use only `decompress.0.9.0`

### 2.1.0 (2019-07-10)

- Move to the last version of `decompress` (@dinosaure, #366)
- Check order of entries in a tree object (bug found by @samoht, fixed by @dinosaure, #365)
- Use `mmap` package (@dinosaure, #347, #360)
- Update README.md (@tcoopman, @dinosaure, #337, #359)
- `trim` the window used to pack (@pqwy, @dinosaure, #357, #358)
- Use lastest version of `lru.0.3.0` (@pqwy, @dinosaure, #352, #356)
- Fix smart protocol (fixed by @clecat and @dinosaure, feedbacks from @hannesm)
 * Pull-request #351, #350, #338
 * Issues #335, #342, #346
 
   + regression tests was added (@dinosaure)
   + semantics about negociation was explained (@clecat)
   + end-to-end tests partially done (@hannesm)
 
- Remove `sexplib` dependency (@samoht, #349)
- Fix smart protocol to accept empty response from `ls-remote` (bug found by @hannesm, fixed by @dinosaure, #348)
- Add `io-page-unix` as dependency to tests `git-mirage` (@dinosaure, #345)
- Remove deprecated `Cstruct.add_len` (replaced by `ke`) (@dinosaure, #345)
- Use `Uri.user_info` to be able to be authentified by a service like GitHub (@linse, review by @dinosaure, #341, #343)
- avoid clash between `digestif.c` and `digestif.ocaml` implementation (same for `checkseum`)
 * remove implementation dependencies on `git-unix` and `git-mirage` (bug found by @hannesm and @linse, fixed by @dinosaure, #339)
 
   This update should be fixed by `dune`'s variants and `>= digestif.0.7.2` and `>= checkseum.0.1.0`
 
- **breaking-change** add `etmp` as already-allocated buffer to encode Git object (@dinosaure, #336)
 * add `ke.0.3` as new dependency
- consumed inputs for every entries in a tree (bug found by @zspicko, fixed by @dinosaure, #334)

### 2.0.0 (2018-10-17)

- New world, new version

### 1.11.3 (2017-11-20)

- add ocplib-endian to dependencies of git (#233, @hannesm)

### 1.11.2 (2017-08-02)

- Update to conduit.1.0 and cohttp.0.99 (#226, @samoht)

### 1.11.1 (2017-07-25)

- [git-unix] Fix linking issue of the `ogit` binary on some package
  configurations (#225, @samoht)

### 1.11.0 (2016-06-01)

* Add `ogit add-commit-parent` to add a new parent to a given commit
  (#208, @samoht)
* port to jbuilder (#209, @samoht)
* port to mtime 1.0 (#212, @dinosaure and @samoht)
* use Decompress instead of camlzip (#211, @dinosaure and @samoht)

### 1.10.1 (2017-04-15)

* Improve API docs (#201, @olleolleolle)
* Compat with cmdliner 1.0 (#202, @samoht)
* Fix typos and links in docs (@smeruelo and @olleolleolle)

### 1.10.0 (2017-02-15)

* Adapt to Mirage3 (@samoht, @avsm, @yomimono, @hannesm)
* Add IO.test_and_set to automatically update references (#185, @samoht)
* Better Windows support (#187, @samoht)
  - unix: Translate Git references into valid Windows filenames
  - fix/work-around upstream issues to make the tests pass on Windows
* Split the package into 4 packages: `git`, `git-http`, `git-unix` and
  `git-mirage` (#189, @samoht)

### 1.9.3 (2016-11-09)

* Turn a bunch of info message into debug statements (#169, @samoht)

### 1.9.2 (2016-10-16)

* Do not depend on `ocaml-crc` anymore: less C code means, easier to
  port to other MirageOS backends (#166, @g2p)

### 1.9.1 (2016-10-04)

* Cache all intermediate values when expending packed values. This
  speeds-up pack access quite a lot in practice, especially when the
  pack files are big (#163)

### 1.9.0 (2016-09-30)

* Use safe-string (#160, @samoht)
* Support camlzip 1.06 (#160, @samoht)
* Use topkg, remove pack (#158, @samoht)

### 1.8.0 (2016-05-03)

* References can point to non-commit hashes (#123, @samoht)
* Rename `Git.SHA` into `Git.Hash` (@samoht)
* Remove `Git.Sync.populate` but add `Git.Sync.clone` to replace the
  previous `fetch+populate` (#122, @samoht)
* Fix the API so that references can point to any kind of hashes (#123, @samoht)
* Fix duplicated references when mixing packed and loose references
  (#124, @samoht)
* Use astring, logs and fmt (@samoht)
* Add `Store.size`. Before, the only way to get the size of a file was to
  decompress the whole thing. The main win comes from getting the size of raw
  packed objects without decompressing. (#143, @talex5)
* Workaround a memory leak in lwt: See ocsigen/lwt#229, which caused
  `IO.rec_file` and thus the watch mechanism on Unix to leak memory
  (#146, @samoht)
* Fix performance regression when reading large files (#147, @chambart)

### 1.7.3 (2016-02-19)

* Git_unix now works on MinGW

### 1.7.2 (2016-01-21)

* Do not mmap files, as there is no way to easily control the unmap
  of bigarrays. This remove EMFILE errors under load (#133, @talex5)
* Fix comparison of entries in the index file. This fix a random issue
  which might happen when entries in a pack file are checkout and
  updated (@samoht)

### 1.7.1 (2015-08-10)

* Support cohtpp 0.19.1. (#119, @rgrinberg)
* Revert API break for the Sync functor. Now still takes only a `Store.S`
  as parameter. (#120)

### 1.7.0 (2015-08-06)

* Changes to the `Search` API:
  - Remove `find_exn`
  - the type `t` has a new case `Tree_root` to represent tree roots
  - the type `path` is now structured (not a list of strings anymore).
    this removes the confusing semantics of empty strings in paths.

* Changes to the `Global_graph` API:
  - `closure` takes an optional `full` argument to work over
    commit objects only
  - All the arguments of `pack` are labelled and its result type
    has changed.
  - Add `keys` to return the topological sort of keys in the graph.


* Improve the size of pack files received when fetching (#115)
  - advertise the fact that `ocaml-git` clients support `ofs-delta`
    and `thin-pack`
  - Compute a minimal set of `haves` to send to the server after
    the discovery phase, where we already know all the server
    references (#114)

* Unpack shallow pack files after a fetch
* Full support for shallow packs (#81)
* During fetch, we now respect the "allow-reachable-sha1-in-want"
  server (non-)capability. A proper error is reported to the client
  if that's not the case.
* Parametrize the codebase over the Inflate implementation. Useful
  to change the inflate algorithm (or to not use any at all, which
  would be what we want for big files and/or for efficient writes)
  and to simplify the port to other backends. `Store.S` implementations
  now expose their `Inflate` implementation.
* Parametrize the codebase over the SHA implementation. Useful to change
  the SHA algorihm (the unix backend provides SHA256) or simplify the
  port to other backends (the mirage backend uses a pure OCaml implementation
  extracted from uuidm). `Store.S` implementations now expose their `Digest`
  implementation (#68)

* Better sync API (#113)
  - Change the arguments of `Sync.fetch`
  - Remove `Sync.clone`
  - Add `Sync.populate` to be called after a fetch to properly populate
    the local Git repository (similar to what `git clone` does).
  - Add `ogit clone --no-checkout` and make `ogit clone --bare` more similar
    to the same `git` command.

* Support Github http(s) URLs without .git (#111)
* Add a `dot_git` optional parameter to `Store.create` to specify where
  the Git metadata should be stored (default is still `<root>/.git`). This
  is useful to properly support `bare` repositories (#110)
* Full support for 32 bit platform (ie. continue the patch started in 1.6.2).
* Rework the `Git.Pack` and `Git.Pack_index` API to speed-up random access.
* Add a `Git.SHA.Array` module to work with contiguous arrays of SHA1.
* Rename `Git.SHA.lenght` to `Git.SHA.hex_length` to avoid confusion.
* Expose `Git.SHA.of_short_hex` to create short hashes. `Git.SHA.of_hex` now
  only accept 40 characters hexa-decimal strings.
* Improve the output of `ogit clone` by showing progress when the pack file
  is downloaded and when the delta are resolved.

### 1.6.2 (2015-07-16)

* Support 32 bit platform by avoiding creating large strings. This also improve
  the performance of reading and synchronizin large pack files
  (#103, #105 @gregtatcam)

### 1.6.1 (2015-07-14)

* Fix a bug in `ogit pull` using the smart HTTP protocol when the HTTP temporary
  buffer could sometimes be overfill.
* Avoid closing twice the same fd in the smart HTTP protocol.
* Avoid the GC to close a fd while we are still using a channel built on top of
  it -- this affects the smart HTTP protocol only.
* Add an opam file for the `mirage-git` package.

### 1.6.0 (2015-07-11)

* Allow some references to contain pointer to other references (#96)
* Improve the support for 32bit architectures (#97)
* Add `Reference.pp_head_contents` and `Reference.equal_head_contents`.
* Remove `Store.clear` and replace it by `Memory.clear`, `Memory.clear_all`
  and `FS.remove`. This let users have a finer control over the memory
  consumption of the program over time (related to #90)
* Rename all `pp_hum` functions into `pp`.
* Fix regression in `Sync.fetch` and add unit-tests (running only in slow mode).
* Fix reading of `.git/HEAD` when the contents is a commit hash.
* Depends on `Stringext` for all the extra string function needed.

### 1.5.3 (2015-07-10)

* Fix listing of packed references (#98)

### 1.5.2 (2015-07-04)

* Fix handling of empty paths (#89)
* Fix the serialization of dates in commit objects
* Expose `Git.Packed_value.PIC.pretty`
* Improve the efficiency of `Git_unix.FS.remove`
* Partial support for shallow packs (#81)
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
