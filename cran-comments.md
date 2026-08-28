## Why this submission

SpaDES.tools was removed from CRAN on 2026-07-13 because a package it depends
on, reproducible, had been removed. Nothing was wrong with SpaDES.tools
itself. reproducible returned to CRAN on 2026-08-25, so SpaDES.tools can be
restored.

The last version on CRAN was 2.1.1. This submission is 2.1.3, which also
carries the changes prepared for 2.1.2 (never submitted). See NEWS.md.

## Test environments

Checked with `R CMD check --as-cran --no-manual`. All passed.

* Ubuntu 26.04, R 4.6.1 (local)
* Ubuntu 24.04.4, R 4.6.1, R 4.5.3, R 4.4.3, R-devel (4.7.0)
* Windows Server 2025, R 4.6.1, R 4.5.3, R 4.4.3, R-devel (4.7.0)
* macOS 26.5.2, R 4.6.1
* win-builder, R 4.5.3 (oldrelease), R 4.6.1 (release), R-devel (4.7.0)
* mac-builder, macOS 26.6 arm64, R 4.6.1 Patched (2026-07-27 r90311)

Two of those (Ubuntu and Windows, R 4.6.1) were checked with only the required
packages installed, not the suggested ones.

## R CMD check results

No ERRORs. No WARNINGs. One NOTE, for the reason given above:

    New submission

    Package was archived on CRAN

    CRAN repository db overrides:
      X-CRAN-Comment: Archived on 2026-07-13 as requires archived package
        'reproducible'

## Memory and undefined-behaviour checks

This release adds compiled code (Rcpp helpers in `spread()`/`spread2()`), so it
was additionally checked in the r-hub containers:

* gcc ASAN + UBSAN (`-fsanitize=address,undefined,bounds-strict`): clean
* valgrind: clean -- no invalid reads/writes, no uninitialised values,
  `definitely lost: 0 bytes`
* rchk: no protection-stack or unprotected-variable findings in this package

## Reverse dependencies

None on CRAN at present. The two that existed, SpaDES.core and SpaDES, were
removed on 2026-07-13 for the same reason as this package, and are being
resubmitted.

We ran revdepcheck on 2026-05-16 against the 10 packages in the wider SpaDES
family (SpaDES.core, SpaDES, LandR, NetLogoR, map, scfmutils,
SpaDES.experiment, fireSenseUtils, LandWebUtils, usefulFuns) and found no new
problems.
