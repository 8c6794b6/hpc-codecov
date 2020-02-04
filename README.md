hpc-codecov
===========

[![CircleCI](https://img.shields.io/circleci/build/gh/8c6794b6/hpc-codecov/master?logo=circleci)](https://circleci.com/gh/8c6794b6/hpc-codecov)
[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov/branch/master/graph/badge.svg)](https://codecov.io/gh/8c6794b6/hpc-codecov)

The ``hpc-codecov`` package contains an executable and library codes
for generating [codecov.io](https://codecov.io) test coverage report
from ``.tix`` and ``.mix`` files made by
[hpc](https://hackage.haskell.org/package/hpc).

The ``hpc-codecov`` executable does not try to find out the location
of ``.tix`` and ``mix`` files. Instead, let the user to explicitly
specify the file path and directories. The rational behind the
decision is to support multiple version of multiple build tools, such
as [cabal-install](http://hackage.haskell.org/package/cabal-install)
v1-style legacy build, cabal v2-style build, and
[stack](https://docs.haskellstack.org/en/stable/README/).


Examples
--------

Following shows two examples for generating test coverage report of
the ``hpc-codecov`` package itself, with ``cabal-install`` (version
3.0.0) Nix-style local build commands, and with ``stack``.

### With cabal-install

First, run the tests with coverage option to generate ``.tix`` and
``mix`` files:

```console
$ cabal v2-configure --enable-test --enable-coverage
$ cabal v2-test
```

Then generate a Codecov.io JSON coverage data from the ``.tix`` and
``.mix`` files.

```console
$ proj=hpc-codecov-0.1.0.0
$ mix=$(find ./dist-newstyle -name vanilla)/mix/$proj
$ tix=$(find ./dist-newstyle -name $proj.tix)
$ hpc-codecov --mix=mix --exclude=Paths_hpc_codecov --out=reports $tix
```

The ``--out`` specifies the output file to write the JSON
report. Observing the contents of ``reports`` with
[``jq``](https://stedolan.github.io/jq/):

```console
$ jq . reports | head -20
{
  "coverage": {
    "src/Trace/Hpc/Codecov/Report.hs": {
      "54": 1,
      "55": "1/2",
      "56": "1/2",
      "57": 1,
      "58": 1,
      "63": "1/2",
      "67": 1,
      "70": 1,
      "71": 1,
      "72": "1/2",
      "73": 1,
      "77": 1,
```

Send the resulting report file to Codecov.io with the [bash
uploader](https://codecov.io/bash):

```console
$ bash <(curl -s https://codecov.io/bash) -f reports
```

According to the Codecov.io
[FAQ](https://docs.codecov.io/docs/frequently-asked-questions), the
uploader should work from [Travis](https://travis-ci.org/),
[CircleCI](https://circleci.com/), and
[AppVeyor](https://www.appveyor.com/) for public projects.


### With stack

Build the package and run the tests with coverage option:

```console
$ stack build --test --coverage
```

As done in ``cabal-install`` example, specify the path of ``.tix`` and
``.mix`` files. Using ``path`` sub-command to get the local hpc root
directory and dist directory:

```console
$ hpcroot=$(stack path --local-hpc-root)
$ tix=$(find $hpcroot -name 'test-main.tix')
$ mix=$(stack path --distdir)/hpc
$ hpc-codecov --mix=$mix --exclude=Paths_hpc_codecov -o reports $tix
```

Then send the resulting report file:

```console
$ bash <(curl -s https://codecov.io/bash) -f reports
```


References
----------

- [HPC publication](http://ittc.ku.edu/~andygill/papers/Hpc07.pdf)
- [Codecov API reference](https://docs.codecov.io/reference)
