## Updated release

This is a maintenance release which adds several new features and fixes some minor bugs.
See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.5.3
* Windows                    (appveyor), R 3.5.3
* Windows                 (win-builder), R 3.5.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 3.6.1
* macOS 10.15.1 Catalina        (local), R 3.6.1
* Ubuntu 16.04              (travis-ci), R 3.6.1
* Ubuntu 18.04                  (local), R 3.6.1
* Windows                    (appveyor), R 3.6.1
* Windows                 (win-builder), R 3.6.1

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.0.0 (2019-11-21 r77446)
* Ubuntu 18.04                  (local), R 4.0.0 (2019-11-21 r77446)
* Windows                    (appveyor), R 4.0.0 (2019-11-20 r77445)
* Windows                 (win-builder), R 4.0.0 (2019-11-20 r77445)


## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly mispelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed.
Summary at https://github.com/PredictiveEcology/SpaDES.tools/blob/master/revdep/README.md.
