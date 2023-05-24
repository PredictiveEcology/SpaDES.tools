## Release information

This is an major update of this package to address the global transition from `raster`, `sp`, `rgeos` and `rgdal` to `terra` and `sf`. The order of authors was also changed and `cre` were updated in this release to reflect a change in maintainer.  

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS Monterey 12.6.5        (GitHub), R 4.3.0
* macOS (m2) Ventura 13.2.1     (local), R 4.3.0
* macOs (m1) Big Sur             (rhub), R 4.3.0
* Ubuntu 20.04                 (GitHub), R 4.3.0
* Ubuntu 20.04                  (local), R 4.3.0 
* Windows                      (GitHub), R 4.3.0
* Windows                       (local), R 4.3.0
* Windows                 (win-builder), R 4.3.0

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-05-19 r84449 ucrt)
* Windows                      (GitHub), R-devel (2023-05-19 r84449 ucrt)
* Windows                 (win-builder), R-devel (2023-05-19 r84449 ucrt)
## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

2.  Related to the Suggested package that is available in a additional repository.
    We provide instructions for installing suggested package `NLMR` from another repository:

    The Description field contains
        "https://PredictiveEcology.r-universe.dev")'.
    Please enclose URLs in angle brackets (<...>).

## Downstream dependencies

Currently none, but we are working to resubmit our other packages that depend on this one, and they are passing.
