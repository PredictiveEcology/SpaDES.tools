## Release information

This update fixes an issue with test files being left behind in user's home directory.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 13.3.1            (mac-builder), R 4.5.1
* macOS 14.7.6                 (GitHub), R 4.5.1
* macOS 26.0.1                  (local), R 4.5.1
* Ubuntu 24.04                 (GitHub), R 4.5.1
* Ubuntu 24.04                  (local), R 4.5.1
* Windows                      (GitHub), R 4.5.1
* Windows                       (local), R 4.5.1
* Windows                 (win-builder), R 4.5.1

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel (2025-10-09 r88913)
* Ubuntu 24.04                  (local), R-devel (2025-10-08 r88906)
* Windows                      (GitHub), R-devel (2025-10-09 r88913 ucrt)
* Windows                 (win-builder), R-devel (2025-10-09 r88913 ucrt)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

1. The suggested package `NLMR` is available in a additional repository, for which we provide instructions for installation:

    The Description field contains
        "https://PredictiveEcology.r-universe.dev" 


## Downstream dependencies

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
