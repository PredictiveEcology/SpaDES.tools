## Updated release

This release fixes CRAN concerns about package declarations for tests and vignettes.

The maintainer email address has changed, about which I notified CRAN on March 28, 2018 and sent followup on June 12, 2018 (in response to `fpCompare` submission) and again on June 15, 2018 (in response to `SpaDES` submission).

We also introduce several new functions, and update previous functions (see NEWS.md).

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
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-15 r74903)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-15 r74903)
* Windows           (appveyor), R 3.6.0 (2018-06-11 r74889)
* Windows        (win-builder), R 3.6.0 (2018-06-13 r74894)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

1. Maintainer's email address has changed (notified CRAN 2018-05-28, 2018-06-12, and 2018-06-15).

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Alex M Chubaty <alex.chubaty@gmail.com>'
        
    New maintainer:
      Alex M Chubaty <alex.chubaty@gmail.com>
    Old maintainer(s):
      Alex M Chubaty <alexander.chubaty@canada.ca>

2. Some words were flagged as possibly mispelled, but they are false positives:

    Possibly mis-spelled words in DESCRIPTION:
      SpaDES (4:27)
      automata (6:25)

## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.
Summary at https://github.com/PredictiveEcology/SpaDES.tools/blob/master/revdep/README.md.

* `SpaDES` and `SpaDES.core` produce an error due to a non-CRAN package in Suggests, but passes once that dependency is installed.
