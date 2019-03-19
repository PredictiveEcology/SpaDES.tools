## Updated release

This is a maintenance release to address problems identified during CRAN checks.
See NEWS.md for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0
* Windows 7               (local), R 3.4.4

### Current R versions
* macOS Mojave       (travis-ci), R 3.5.3
* macOS Mojave           (local), R 3.5.3
* Ubuntu 14.04       (travis-ci), R 3.5.3
* Ubuntu 18.04           (local), R 3.5.3
* Windows             (appveyor), R 3.5.3
* Windows          (win-builder), R 3.5.3
* Windows 7              (local), R 3.5.2

### Development R version
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-03-15 r76244)
* Ubuntu 18.04           (local), R 3.6.0 (2019-03-18 r76245)
* Windows             (appveyor), R 3.6.0 (2019-03-15 r76239)
* Windows          (win-builder), R 3.6.0 (2019-03-15 r76244)

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
