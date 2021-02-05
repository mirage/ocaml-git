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
