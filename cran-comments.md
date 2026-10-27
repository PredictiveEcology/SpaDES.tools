## Why this submission

This is a minor release.
It adds `spreadCpp()`, a faster C++ alternative to `spread()`, and fixes several bugs in `spread()`, `spread2()` and `adj()`; see NEWS.md.
It also fixes tests that fail with the development version of terra (1.9-54), which now errors on an argument the tests passed to `terra::focal()`.

## Test environments

Checked with `R CMD check --as-cran`. All passed.

* Ubuntu 24.04.5, R 4.6.1 (local, with the PDF manual)
* Ubuntu 24.04.5, R 4.6.1, R 4.5.3, R 4.4.3, R-devel (4.7.0)
* Windows Server 2025, R 4.6.1, R 4.5.3, R 4.4.3, R-devel (4.7.0)
* macOS 26.6.2, R 4.6.1
* win-builder, R 4.5.3 (oldrelease), R 4.6.1 (release), R-devel (4.7.0)

Three of those (local Ubuntu, and Ubuntu and Windows with R 4.6.1) were checked with only the required packages installed, not the suggested ones.

mac-builder could not run the check: on four tries between 2026-10-26 and 2026-10-27 it reported every dependency, Rcpp included, as not available, before checking anything.
macOS is covered by the GitHub Actions run above.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Memory and undefined-behaviour checks

This release adds compiled code (`spreadCpp()`), so it was additionally checked in the r-hub containers with R-devel (2026-10-25 r90763):

* gcc ASAN + UBSAN: clean
* valgrind: clean -- no invalid reads/writes, no uninitialised values,
  `definitely lost: 0 bytes`
* rchk: no findings in this package's code (only in Rcpp's headers)

## Reverse dependencies

We checked 2 reverse dependencies (NetLogoR, SpaDES.core), comparing R CMD check results across the CRAN (2.1.3) and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
