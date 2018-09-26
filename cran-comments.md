## Updated release

This release fixes CRAN check WARNINGs about functions that had been moved to `reproducible` package.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0
* Windows 7               (local), R 3.4.4

### Current R versions
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.1
* Ubuntu 18.04         (local), R 3.5.1
* Windows           (appveyor), R 3.5.1
* Windows        (win-builder), R 3.5.1
* Windows 7            (local), R 3.5.1

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-20 r74923)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-28 r74935)
* Windows           (appveyor), R 3.6.0 (2018-07-01 r74950)
* Windows        (win-builder), R 3.6.0 (2018-07-12 r74955)

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

* `SpaDES` and `SpaDES.core` produce a WARNING due to no `DISPLAY` wariable being set during headless checks.
