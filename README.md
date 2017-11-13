# gitblame

Ever wondered what happened to files in some Git repo? Now you can `gitblame` a directory to see what files were there and what happened to them.
```
$ mkdir gitblame-test
$ cd gitblame-test/
$ git init
Initialized empty Git repository in /private/tmp/gitblame-test/.git/
$ git commit -am 'Init' --allow-empty
[master (root-commit) 7d07456] Init
$ touch {a,b,c}
$ ls
a b c
$ git add .
$ git commit -m 'Add some files'
[master 7a8c7f3] Add some files
 3 files changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 a
 create mode 100644 b
 create mode 100644 c
$ git rm b
rm 'b'
$ git commit -m 'Remove b'
[master 286bfbd] Remove b
 1 file changed, 0 insertions(+), 0 deletions(-)
 delete mode 100644 b
$ ls
a c
$ git mv c c-new
$ git commit -m 'Mv c'
[master fd1a5fe] Mv c
 1 file changed, 0 insertions(+), 0 deletions(-)
 rename c => c-new (100%)
$ ~/go/bin/gitblame
fd1a5fe lukasz.czaplinski 2017-11-13 R c
286bfbd lukasz.czaplinski 2017-11-13 D b
7a8c7f3 lukasz.czaplinski 2017-11-13 A a
```
