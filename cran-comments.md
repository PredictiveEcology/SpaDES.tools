## Resubmission

This is an update to an existing package. We introduce several new functions, and update previous functions. We submitted this package 3 days ago with an external package "suggests" that was not allowed by CRAN policies. We have removed that "suggests" package.


## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.1.0
* Ubuntu 14.04        (travis-ci), R 3.2.0
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.1.0
* Windows              (appveyor), R 3.2.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0

### Current R versions
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-08 r74873)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-05 r74852)
* Windows           (appveyor), R 3.6.0 (2018-06-07 r74868)
* Windows        (win-builder), R 3.6.0 (2018-06-07 r74865)

## R CMD check results

There were no ERRORs or WARNINGs or NOTES


## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/SpaDES.tools/blob/master/revdep/README.md.

* `SpaDES.core` produces an error due to a non-CRAN package in Suggests, but passes once that dependency is installed.
