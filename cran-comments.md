## Updated release

This release fixes problems with Suggested package use highlighted by CRAN checks.
See `NEWS.md`.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.6 Catalina       (GitHub), R 4.1.1
* macOS 11.1 Big Sur            (local), R 4.1.1
* Ubuntu 20.04                 (GitHub), R 4.1.1
* Ubuntu 20.04                  (local), R 4.1.1
* Windows                      (GitHub), R 4.1.1
* Windows                       (local), R 4.1.1
* Windows                 (win-builder), R 4.1.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2021-05-29 r80411)
* Ubuntu 20.04                  (local), R-devel (2021-05-31 r80426)
* Windows                      (GitHub), R-devel (2021-05-30 r80415)
* Windows                 (win-builder), R-devel (2021-09-23 r80951)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

## Downstream dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 
Summary at <https://github.com/PredictiveEcology/SpaDES.tools/blob/master/revdep/README.md>.
