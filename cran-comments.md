## Updated release

This is a maintenance release to address problems identified during CRAN checks.
See NEWS.md for a full list of changes.

## Test environments

### Previous R versions
* macOS Mojave        (travis-ci), R 3.5.3
* macOS Mojave            (local), R 3.5.3
* Ubuntu 16.04        (travis-ci), R 3.5.3
* Windows              (appveyor), R 3.5.3
* Windows 7               (local), R 3.5.3

### Current R versions
* macOS Mojave        (travis-ci), R 3.6.1
* macOS Mojave            (local), R 3.6.1
* Ubuntu 16.04        (travis-ci), R 3.6.1
* Ubuntu 18.04            (local), R 3.6.1
* Windows              (appveyor), R 3.6.1
* Windows           (win-builder), R 3.6.1
* Windows 7               (local), R 3.6.0

### Development R version
* Ubuntu 16.04        (travis-ci), R 3.7.0 (2019-07-05 r76788)
* Ubuntu 18.04            (local), R 3.7.0 (2019-07-05 r76788)
* Windows              (appveyor), R 3.7.0 (2019-07-05 r76788)
* Windows           (win-builder), R 3.7.0 (2019-07-05 r76788)

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
