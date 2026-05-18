## Release information

This is a minor update. The main changes are performance improvements to
`spread()`, `spread2()`, and `rasterizeReduced()` (the per-iteration
neighbour computation in `spread()`/`spread2()` is now done in C++ via new
Rcpp helpers; output is value-identical to the previous implementation and
is verified against pre-Rcpp snapshots), and a new built-in generator for
`neutralLandscapeMap()` that allowed the GitHub-only `NLMR` package to be
dropped entirely (removed from `Suggests`, `Remotes`, and
`Additional_repositories`). See `NEWS.md` for a full list of changes.

This release also changes the maintainer, from Alex M Chubaty
<achubaty@for-cast.ca> to Eliot J B McIntire
<eliot.mcintire@nrcan-rncan.gc.ca> (both are existing package authors).

## Test environments

### Previous R versions
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R-oldrelease  [submitted 2026-05-16; results pending]

### Current R versions
* macOS                        (GitHub), R 4.5.x
* Ubuntu 24.04                 (GitHub), R 4.5.x (incl. _R_CHECK_DEPENDS_ONLY_)
* Windows                      (GitHub), R 4.5.x (incl. _R_CHECK_DEPENDS_ONLY_)
* Ubuntu 24.04.4                (local), R 4.5.3 (2026-03-11)
* Windows                 (win-builder), R-release  [submitted 2026-05-16; results pending]

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Windows                      (GitHub), R-devel (ucrt)
* Windows                 (win-builder), R-devel  [submitted 2026-05-16; results pending]

### Sanitizers / memory checks (R-hub, for the new C++ code)
* clang-asan, clang-ubsan, gcc-asan, valgrind, rchk  [run for this release]

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE

1. New maintainer:

        Eliot J B McIntire <eliot.mcintire@nrcan-rncan.gc.ca>
      Old maintainer(s):
        Alex M Chubaty <achubaty@for-cast.ca>

   This maintainer change is intentional; both individuals are existing
   authors of the package.

## Downstream dependencies

We checked 10 reverse dependencies (revdepcheck), comparing R CMD check
results across CRAN and the development version of this package.

 * We saw 0 new problems.
 * 2 packages (fireSenseUtils, LandWebUtils) could not be installed in the
   check environment because of unrelated missing dependencies (`RCurl`
   and `MuMIn`, respectively). These installation failures are identical
   on both the CRAN and development versions and are not attributable to
   any change in this release.
