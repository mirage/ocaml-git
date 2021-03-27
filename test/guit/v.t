Tests v
  $ mkdir simple
  $ cd simple
  $ guit.v
  $TESTCASE_ROOT/simple ready to be used as a Git store.
  $ git checkout -b new
  Switched to a new branch 'new'
  $ git commit --allow-empty -q -m .
  $ guit.v -q
  $ git branch
  * new
