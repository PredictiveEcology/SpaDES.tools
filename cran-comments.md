## Release information

This update drops support for R 4.1 due to changes in dependency packages.
See `NEWS.md` for a full list of changes.

## Test environments

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3                 (GitHub), R 4.3.3
* macOS 13.3.1            (mac-builder), R 4.3.3
* macOS 14.4.1                  (local), R 4.3.3
* Ubuntu 20.04                 (GitHub), R 4.3.3
* Ubuntu 20.04                  (local), R 4.3.3
* Windows                      (GitHub), R 4.3.3
* Windows                       (local), R 4.3.3
* Windows                 (win-builder), R 4.3.3

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2024-04-09 r86391)
* Ubuntu 20.04                  (local), R-devel (2024-04-10 r86396)
* Windows                      (GitHub), R-devel (2024-04-09 r86391 ucrt)
* Windows                 (win-builder), R-devel (2024-04-09 r86391 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs

1. The suggested package `NLMR` is available in a additional repository, for which we provide instructions for installation:

    The Description field contains
        "https://PredictiveEcology.r-universe.dev" 


## Downstream dependencies

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
