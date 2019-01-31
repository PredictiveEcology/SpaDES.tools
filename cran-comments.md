## Updated release

This release updates an example to work with the latest version of `raster`.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0
* Windows 7               (local), R 3.4.4

### Current R versions
* macOS Mojave       (travis-ci), R 3.5.2
* macOS Mojave           (local), R 3.5.2
* Ubuntu 14.04       (travis-ci), R 3.5.2
* Ubuntu 18.04           (local), R 3.5.2
* Windows             (appveyor), R 3.5.2
* Windows          (win-builder), R 3.5.2
* Windows 7              (local), R 3.5.2

### Development R version
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-01-29 r76025)
* Ubuntu 18.04           (local), R 3.6.0 (2019-01-25 r76014)
* Windows             (appveyor), R 3.6.0 (2019-01-26 r76018)
* Windows          (win-builder), R 3.6.0 (2019-01-27 r76018)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly mispelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/SpaDES.tools/blob/master/revdep/README.md.

* `SpaDES` and `SpaDES.core` produce a WARNING due to no `DISPLAY` variable being set during headless checks.
