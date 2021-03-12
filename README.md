# hpc-codecov [![Hackage](https://img.shields.io/hackage/v/hpc-codecov)](https://hackage.haskell.org/package/hpc-codecov) [![Stackage](https://www.stackage.org/package/hpc-codecov/badge/lts)](https://www.stackage.org/lts/package/hpc-codecov)

[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov/branch/master/graph/badge.svg)](https://codecov.io/gh/8c6794b6/hpc-codecov)
[![Travis](http://img.shields.io/travis/8c6794b6/codecov-haskell/master.svg?logo=travis)](https://travis-ci.com/8c6794b6/hpc-codecov)
[![CircleCI](https://img.shields.io/circleci/build/gh/8c6794b6/hpc-codecov/master?logo=circleci)](https://circleci.com/gh/8c6794b6/hpc-codecov)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/dijqtsoqgc26oghj?svg=true)](https://ci.appveyor.com/project/8c6794b6/hpc-codecov)
[![GitHub](https://img.shields.io/github/workflow/status/8c6794b6/hpc-codecov/ci?logo=github)](https://github.com/8c6794b6/hpc-codecov/actions?query=workflow%3ci)

The ``hpc-codecov`` package contains an executable and library codes
for generating [Codecov](https://codecov.io) JSON coverage report from
``.tix`` and ``.mix`` files made with
[hpc](https://hackage.haskell.org/package/hpc).

The ``hpc-codecov`` executable does not try to find out the location
of ``.tix`` and ``mix`` files. Instead, let the user to explicitly
specify the file paths and directories. The rational behind the
decision is to support multiple versions of multiple build tools, such
as [cabal-install](http://hackage.haskell.org/package/cabal-install)
legacy v1-style build, v2-style build, and
[stack](https://docs.haskellstack.org/en/stable/README/).


Examples
--------

Following shows two examples for generating test coverage report of
the ``hpc-codecov`` package itself, one with ``cabal-install``
Nix-style local build commands, and another with ``stack``.

### With cabal-install

First, run the tests with coverage option to generate ``.tix`` and
``mix`` files:

```console
$ cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library
$ cabal v2-configure --enable-test --enable-coverage
$ cabal v2-test
```

Then generate a Codecov JSON coverage data from the ``.tix`` and
``.mix`` files:

```console
$ proj=hpc-codecov-0.1.0.0
$ tix=$(find ./dist-newstyle -name $proj.tix)
$ mix=$(find ./dist-newstyle -name vanilla -print -quit)/mix/$proj
$ hpc-codecov --mix=$mix --exclude=Paths_hpc_codecov --out=codecov.json $tix
```

The ``--out`` option specifies the output file to write the JSON
report. Observing the contents of ``codecov.json`` with
[``jq``](https://stedolan.github.io/jq/):

```console
$ jq . codecov.json | head -10
{
  "coverage": {
    "src/Trace/Hpc/Codecov/Error.hs": {
      "27": 1,
      "30": 1,
      "31": 1,
      "32": 1,
      "33": 1,
      "34": 1,
      "51": 0,
```

Send the resulting JSON report file to Codecov with the [bash
uploader](https://github.com/codecov/codecov-bash/). The file name
``codecov.json`` is listed in the uploader script as one of the file
name patterns to upload, no need to specify the report filename
explicitly:

```console
$ bash <(curl -s https://codecov.io/bash)
```

According to the Codecov
[FAQ](https://docs.codecov.io/docs/frequently-asked-questions), the
uploader should work from [Travis](https://travis-ci.org/),
[CircleCI](https://circleci.com/), and
[AppVeyor](https://www.appveyor.com/) for public projects without
Codecov token.


### With stack

Build the package and run the tests with coverage option:

```console
$ stack --numeric-version
2.3.3
$ stack build --test --coverage
```

As done in ``cabal-install`` example, specify the path of ``.tix`` and
``.mix`` files. Using ``path`` sub-command to get the local hpc root
directory and dist directory:

```console
$ hpcroot=$(stack path --local-hpc-root)
$ tix=$(find $hpcroot -name 'test-main.tix')
$ mix=$(stack path --dist-dir)/hpc
$ hpc-codecov --mix=$mix --exclude=Paths_hpc_codecov -o codecov.json $tix
```

Then send the resulting report file:

```console
$ bash <(curl -s https://codecov.io/bash)
```


References
----------

- [HPC publication](http://ittc.ku.edu/~andygill/papers/Hpc07.pdf)
- [Codecov API reference](https://docs.codecov.io/reference)
