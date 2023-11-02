# hpc-codecov [![Hackage](https://img.shields.io/hackage/v/hpc-codecov)](https://hackage.haskell.org/package/hpc-codecov) [![Stackage](https://www.stackage.org/package/hpc-codecov/badge/lts)](https://www.stackage.org/lts/package/hpc-codecov)

[![codecov](https://codecov.io/gh/8c6794b6/hpc-codecov/branch/master/graph/badge.svg)](https://codecov.io/gh/8c6794b6/hpc-codecov)
[![CircleCI](https://img.shields.io/circleci/build/gh/8c6794b6/hpc-codecov/master?logo=circleci)](https://circleci.com/gh/8c6794b6/hpc-codecov)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/dijqtsoqgc26oghj?svg=true)](https://ci.appveyor.com/project/8c6794b6/hpc-codecov)
[![GitHub](https://img.shields.io/github/actions/workflow/status/8c6794b6/hpc-codecov/ci.yml?branch=master&logo=github)](https://github.com/8c6794b6/hpc-codecov/actions/workflows/ci.yml)

The ``hpc-codecov`` package contains an executable and library codes
for generating [Codecov](https://codecov.io) JSON coverage report,
[LCOV](https://github.com/linux-test-project/lcov) tracefile report,
or [Cobertura](https://cobertura.github.io/cobertura/) XML report from
``.tix`` and ``.mix`` files made with
[hpc](https://hackage.haskell.org/package/hpc). The generated report
is ready to be uploaded to Codecov with other tools such as [Codecov
uploader](https://docs.codecov.com/docs/codecov-uploader).

The ``hpc-codecov`` executable can search ``.tix`` and ``mix`` files
under the directories made by the
[cabal-install](http://hackage.haskell.org/package/cabal-install) and
[stack](https://docs.haskellstack.org/en/stable/README/) build tools.
The executable also has options to explicitly specify the file paths
and directories for ``.tix`` and ``mix`` files, to support generating
reports with test data made by other build tools than
``cabal-install`` and ``stack``.


Installing
----------

### From Package Repository

``hpc-codecov`` is available from
[Hackage](https://hackage.haskell.org/package/hpc-codecov) and
[Stackage](https://www.stackage.org/lts/package/hpc-codecov).  To
install with ``cabal-install``, run:

```console
$ cabal install hpc-codecov
```

To install with ``stack``, run:

```console
$ stack install hpc-codecov
```

### Pre-compiled binaries

For Windows, MacOS, and Linux (with glibc and libgmp), pre-compiled
binary executables are available
[here](https://github.com/8c6794b6/hpc-codecov/releases/latest).


QuickStart
----------

To illustrate an example, initializing a sample project named
``my-project`` with ``cabal-install``:

```console
$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library
$ cabal init --simple --tests --test-dir=test -p my-project
```

The directory contents look like below:

```console
.
├── app
│   └── Main.hs
├── CHANGELOG.md
├── my-project.cabal
├── src
│   └── MyLib.hs
└── test
    └── MyLibTest.hs
```

Run tests with coverage option:

```console
$ cabal test --enable-coverage
```

Write Codecov coverage report to ``codecov.json``:

```console
$ hpc-codecov cabal:my-project-test -X my-project -o codecov.json
```

Show coverage report contents:

```console
$ cat codecov.json
{"coverage":{"test/MyLibTest.hs":{"4":1}}}
```


Using in GitHub workflow
------------------------

See
[hpc-codecov-action](https://github.com/8c6794b6/hpc-codecov-action)
to generate Codecov coverage report from GitHub workflow.


Examples
--------

### Showing help

Show usage information:

```console
$ hpc-codecov --help
```

### Project using cabal-install

Search under the directory made by ``cabal-install``, generating a report
for a test suite named ``my-project-test``. Skip searching under the
directories with base name ``my-project``, and exclude modules named
``Main`` and ``Paths_my_project`` from the report. Note the use of
comma to separate multiple values for the ``-x`` option:

```console
$ hpc-codecov -X my-project -x Main,Paths_my_project cabal:my-project-test
```

### Project using stack

Search under the directory made by ``stack`` for a test suite named
``my-project-test``, show verbose information, and write output to
``codecov.json``:

```console
$ hpc-codecov --verbose -o codecov.json stack:my-project-test
```

### Project using stack, with multiple packages

Search under the directory made by ``stack`` for a combined report of
multiple cabal packages, and write output to ``codecov.json``:

```consle
$ hpc-codecov stack:all -o codecov.json
```

### Project using stack, with multiple packages, generate LCOV tracefile

Search under the directory made by ``stack`` for a combined report of
multiple cabal packages, and write the output report in LCOV tracefile
format to ``lcov.info``:

```consle
$ hpc-codecov stack:all -f lcov -o lcov.info
```

### Project using stack, with multiple packages, generate Cobertura XML file

Search under the directory made by ``stack`` for a combined report of
multiple cabal packages, and write output report in Cobertura XML
format to ``coverage.xml``:

```consle
$ hpc-codecov stack:all -f cobertura -o coverage.xml
```

### Project using stack, running via Docker

Search under the directory made by ``stack`` for a combined report of
multiple cabal packages, running via Docker:

```
$ docker run --rm -v $PWD:$PWD ghcr.io/8c6794b6/hpc-codecov hpc-codecov -r $PWD stack:all
```

Low-level examples
------------------

The following shows two examples for generating a test coverage report
of the ``hpc-codecov`` package itself without specifying the build
tool. One with using the build artifacts made by ``cabal-install``
Nix-style local build commands, and another with ``stack``.

### With cabal-install

First, run the tests with the coverage option to generate ``.tix`` and
``mix`` files:

```console
$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library
$ cabal v2-configure --enable-test --enable-coverage
$ cabal v2-test
```

Then generate a Codecov JSON coverage data from the ``.tix`` and
``.mix`` files:

```console
$ proj=hpc-codecov-0.4.0.0
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
    "src/Trace/Hpc/Codecov/Options.hs": {
      "48": 1,
      "50": 1,
      "52": 1,
      "54": 1,
      "56": 1,
      "59": 1,
      "63": 1,
```

Send the resulting JSON report file to Codecov with the [Codecov
uploader](https://github.com/codecov/uploader). The file name
``codecov.json`` is listed in the uploader script as one of the file
name patterns to upload, no need to specify the report filename
explicitly:

```console
$ codecov -t ${CODECOV_TOKEN}
```

According to the Codecov
[FAQ](https://docs.codecov.io/docs/frequently-asked-questions), the
uploader should work from [Travis](https://travis-ci.com/),
[CircleCI](https://circleci.com/),
[Azure](https://azure.microsoft.com/en-us/services/devops/pipelines),
and [GitHub Actions](https://github.com/features/actions) for public
projects without the Codecov token (i.e., without the `-t
${CODECOV_TOKEN}` option).


### With stack

Build the package and run the tests with the coverage option:

```console
$ stack --numeric-version
2.5.1
$ stack build --test --coverage
```

As done in ``cabal-install`` example, specify the path of ``.tix`` and
``.mix`` files. Using the ``path`` sub-command to get the local hpc root
directory and dist directory:

```console
$ hpcroot=$(stack path --local-hpc-root)
$ tix=$(find $hpcroot -name 'test-main.tix')
$ mix=$(stack path --dist-dir)/hpc
$ hpc-codecov --mix=$mix --exclude=Paths_hpc_codecov -o codecov.json $tix
```


References
----------

- [HPC publication](http://ittc.ku.edu/~andygill/papers/Hpc07.pdf)
- [Codecov API reference](https://docs.codecov.io/reference)
- [LCOV](https://github.com/linux-test-project/lcov)
- [Cobertura](https://cobertura.github.io/cobertura/)
