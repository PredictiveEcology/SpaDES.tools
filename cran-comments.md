## Release information

This is an minor update to complete the final edge cases for the  transition from `raster`, `sp`, `rgeos` and `rgdal` to `terra` and `sf`.
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
* Ubuntu 20.04                 (GitHub), R-devel (2023-07-13 r84688)
* Ubuntu 20.04                  (local), R-devel (2023-07-13 r84688)
* Windows                      (GitHub), R-devel (2023-07-13 r84688 ucrt)
* Windows                 (win-builder), R-devel (2023-07-17 r84702 ucrt)
* Windows                       (local), R-devel (2023-07-12 r84683 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs

1. Alex Chubaty took over the maintainer role for this submission;

2. The suggested package `NLMR` is available in a additional repository, for which we provide instructions for installation:

    The Description field contains
        "https://PredictiveEcology.r-universe.dev" 

## Downstream dependencies

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
