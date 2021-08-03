### 0.4.3 (2021-03-08) Paris - France

- Use `Cstruct.length` instead of `Cstruct.len` (@dinosaure, #522)
- Fix big endian support via `decompress` and `checkseum` (@dinosaure, @talex5, @tmcgilchrist, #523)
- Handle huge PACK files (@dinosaure, @TheLortex, #526)

### 0.4.2 (2021-11-05) Paris - France

- Fix stream of inflated contents on PACK entry (@dinosaure, @talex5, @emillon, #515, #514)

### 0.4.1 (2021-22-04) Paris - France

- Add a `sector_size` argument when we make a PACK decoder
  (@dinosaure, #493, #497)

### 0.4.0 (2021-15-03) Paris - France

- Handle `trunc` argument when we process a `thin` PACK file
  **breaking changes**
  An optional argument is added on the record which abstract the file-system.
  It should be correctly handled by underlying implementation of the
  file-system. It appears that, at top, we need to figure out such option,
  specially for Git and `Cstruct_append` to correctly access to memories.

### 0.3.0 (2021-05-03) Paris - France

- Provides binaries to manipulate PACK files (@dinosaure, #475)
  **breaking changes**
  A transitive breaking changes from decompress.1.3.0 when
  the compressor expects a `De.Lz77.window` instead of
  `De.window`
- Update to decompress.1.3.0 (@dinosaure, #477)

### 0.2.0 (2021-05-02) Saint-Malo - France

- Unmonad `mmap` (@dinosaure, #454)
  `mmap` is a _syscall_ which does not block. The ability to use it outside
  the scheduler monad (like LWT) permits us to _detach_ multiple processes
  to analyze a PACK file.

  With this PR, we take the advantage of `Thread` or `Lwt_preemptive`
  (or more acccurately, the concurrency) to analyze a large PACK file and
  speed-up the `clone`/`fetch` process.

  The distribution comes with a new binary, `carton.verify-pack` which is
  `git verify-pack`.

### 0.1.0 (2021-08-01) Paris - France

- First release of carton
