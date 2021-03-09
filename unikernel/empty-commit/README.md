## A simple MirageOS to fetch & push an empty commit to a Git repository

```sh
$ mirage configure
$ mirage build
$ ./minigit -r git://github.com/dinosaure/art
$ ./minigit -r http://github.com/dinosaure/art
$ ./minigit -r https://github.com/dinosaure/art
$ ./minigit -r https://user:pass@github.com/dinosaure/art
$ ./minigit --ssh-seed seed -r git@github.com:dinosaure/art
```

This is a simple unikernel which wants to fetch the given Git repository
over multiple protocols (TCP/IP, SSH or HTTP - with or without TLS). Then,
it creates a new empty commit and push it. Finally, it re-fetch the Git
repository and compare local and remove last commit.

This is a translation of what the unikernel does in bash:
```sh
$ git clone --depth=1 git://localhost/
$ git commit --allow-empty -m "."
$ A=$(git show-ref -s HEAD)
$ git push
$ git fetch
$ B=$(git show-ref -s HEAD)
$ [ "$A" = "$B" ]
```

NOTE: the option `--branch` exists if you want to push on something else than
`refs/heads/master` (GitHub defaults new repositories to `refs/heads/main`).

### Run the unikernel

To be able to see if the unikernel works, you should simply compile it
to the UNIX target. Then, on the other side, you can create a local
Git repository:
```sh
$ mkdir simple
$ cd simple
$ git init --bare
$ cd ..
$ git daemon --base-path=. \
  --export-all \
  --reuseaddr \
  --informative-errors \
  --verbose \
  --enable-receive-pack
```

Finally, you are able to fill your Git repository with empty commits:
```sh
$ mirage configure
$ make depends
$ mirage build
$ ./minigit -l debug -r git://localhost/simple
```

**NOTE**: `ocaml-git` does **not** handle conflicts. Even if this example
works, if someone else push on the Git repository at the same time, you will
probably get a conflict. If you are interesting to solve such issue, you
should take a look on [irmin][https://github.com/mirage/irmin].
