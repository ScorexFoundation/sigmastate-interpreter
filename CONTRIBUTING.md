# Contributing 

## Building

Clone the repository.

```shell
$ git clone git@github.com:ScorexFoundation/sigmastate-interpreter.git
$ cd sigmastate-interpreter
$ sbt test
```

Then you can compile the library with SBT and run tests.

```shell
$ sbt
sbt:sigma-state> compile
sbt:sigma-state> test
```

By default SBT uses Scala 2.12 for compilation and running tests. To compile for Scala 2.13 use the following commands:

```shell
$ sbt
sbt:sigma-state> ++2.13.8
sbt:sigma-state> compile
sbt:sigma-state> test
```

You can also run SBT commands for all Scala versions at once:

```shell
$ sbt
sbt:sigma-state> +compile
sbt:sigma-state> +test
```

To run specific test suite use the following command:

```shell
sbt:sigma-state> testOnly <full test class name>
```

## Releasing
To publish release version to Sonatype, do the following:
- make a tag with version number `vX.Y.Z` (used by `sbt-dynver` to set `version` in `build.sbt`);
- use the new tag to make a Github release, which triggers [`release.yml`](.github/workflows/release.yml) workflow and publishes release version to Sonatype;
