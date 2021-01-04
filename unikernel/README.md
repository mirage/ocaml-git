## A simple MirageOS to fetch a Git repository

```sh
$ mirage configure
$ mirage build
$ ./minigit -r git://github.com/dinosaure/art
$ ./minigit -r http://github.com/dinosaure/art
$ ./minigit -r https://github.com/dinosaure/art
$ ./minigit --ssh-seed seed -r git@github.com:dinosaure/art
```

An error occurs when the user wants to fetch a Git repository
over SSH without the SSH seed (to generate the private SSH key):

```sh
$ ./minigit -r git@github.com:dinosaure/art
Fatal error: exception (Failure "Invalid flow")
```
