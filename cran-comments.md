## Release information

This is an minor update to complete the final edge cases for the  transition from `raster`, `sp`, `rgeos` and `rgdal` to `terra` and `sf`. We also address reverse depends packages that will be submitted to CRAN. Also, there are minor changes to address CRAN submission issues.

See `NEWS.md` for a full list of changes.

## Test environments

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.2.3

### Current R versions
* macOS 12.6.3 Monterey        (GitHub), R 4.3.1
* macOS (M2) 13.2.1 Ventura     (local), R 4.3.1
* macOS (M1) Big Sur             (rhub), R 4.3.1
* Ubuntu 20.04                 (GitHub), R 4.3.1
* Ubuntu 20.04                  (local), R 4.3.1
* Windows                      (GitHub), R 4.3.1
* Windows                       (local), R 4.3.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-07-12 r84683)
* Windows                      (GitHub), R-devel (2023-07-12 r84683 ucrt)
* Windows                 (win-builder), R-devel (2023-07-12 r84683 ucrt)
* Windows                       (local), R-devel (2023-07-12 r84683 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 0 NOTEs

Related to the Suggested package that is available in a additional repository.
We provide instructions for installing suggested package `NLMR` from another repository:

    The Description field contains
        "https://PredictiveEcology.r-universe.dev"


## Downstream dependencies

All pass the revdeps checks.
