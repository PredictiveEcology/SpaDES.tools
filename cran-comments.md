## Release information

This is a resubmission to restore the package to CRAN following archival due to removal of dependency package `Require`, which has now been restored to CRAN.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.0.5
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.7 Big Sur           (GitHub), R 4.2.2
* macOS 11.7 Big Sur            (local), R 4.2.2
* macOs (m1) Big Sur             (rhub), R 4.2.2
* Ubuntu 20.04                 (GitHub), R 4.2.2
* Ubuntu 20.04                  (local), R 4.2.2 Patched (2022-11-10 r83330)
* Windows                      (GitHub), R 4.2.2
* Windows                       (local), R 4.2.2
* Windows                 (win-builder), R 4.2.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-12-20 r83482)
* Ubuntu 20.04                  (local), R-devel (2022-12-19 r83478)
* Windows                      (GitHub), R-devel (2022-12-20 r83482 ucrt)
* Windows                 (win-builder), R-devel (2022-12-21 r83491 ucrt)
## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

## Downstream dependencies

Currently none, but we are working to resubmit our other packages that depend on this one, and they are passing.
