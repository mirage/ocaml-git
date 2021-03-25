Tests fetch command
  $ mkdir simple
  $ mkdir simple-clone
  $ cd simple
  $ git config init.defaultBranch master
  $ git config user.email "romain@mirage.io"
  $ git config user.name "Romain Calascibetta"
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/simple/.git/
  $ git commit --allow-empty -q -m .
  $ mkfifo pipe-ic
  $ mkfifo pipe-oc
  $ git-upload-pack . < pipe-oc > pipe-ic &
  $ exec 7> pipe-oc
  $ guit.fetch -q -r ../simple-clone fifo://pipe/ refs/heads/master:refs/heads/master
  $ git log --pretty=oneline HEAD > ../expected.out
  $ cd ..
  $ cd simple-clone
  $ git log --pretty=oneline HEAD > ../result.out
  $ cd ..
  $ diff expected.out result.out
