## Release information

This is a resubmission following archival. SpaDES.tools was archived on
2026-07-13 because its dependency `reproducible` was archived; the archival
was not caused by any problem in SpaDES.tools itself. `reproducible` is back
on CRAN as of 2026-08-25
(<https://cran.r-project.org/web/packages/reproducible/index.html>), so this
package's dependencies are all available again.

The last version on CRAN was 2.1.1. Version 2.1.3 therefore carries the
changes prepared for 2.1.2 (which was never submitted) as well as those made
since. Highlights:

* `spread()` and `spread2()` are substantially faster: the per-iteration
  neighbour computation is now done in C++ via new Rcpp helpers. Output is
  value-identical to the previous implementation and is verified against
  snapshots captured from the pre-Rcpp baseline across 30 seeded scenarios.
* `spread2()` accepts a numeric vector for `spreadProbRel`, avoiding a full
  raster re-materialisation on every iteration.
* `neutralLandscapeMap()` gained a built-in generator, which allowed the
  GitHub-only `NLMR` package to be dropped entirely (removed from `Suggests`,
  `Remotes`, and `Additional_repositories`).

See `NEWS.md` for the full list.

## Test environments

### Current R versions
* Ubuntu 24.04                  (local), R 4.5.3

<!-- TODO (before submitting): add rows for the GitHub Actions matrix
     (Ubuntu/macOS/Windows x oldrel-2..devel) once green on `release/2.1.3`,
     and for win-builder oldrelease/release/devel. The C++ changes in this
     release also warrant the R-hub sanitizer runs (clang-asan, clang-ubsan,
     gcc-asan, valgrind) before submission. -->

## R CMD check results

There were no ERRORs or WARNINGs.

There were two NOTEs, both expected:

* `New submission` and `Package was archived on CRAN`. The archival was a
  cascade: CRAN's own comment records "Archived on 2026-07-13 as requires
  archived package 'reproducible'". `reproducible` is back on CRAN.
* `checking HTML version of manual ... Skipping checking math rendering:
  package 'V8' unavailable`, from the local check environment only.

## Downstream dependencies

This package currently has no reverse dependencies on CRAN: its two CRAN
revdeps, SpaDES.core and SpaDES, were archived alongside it on 2026-07-13 for
the same reason, and are being resubmitted.

revdepcheck was last run on 2026-05-16 against the 10 packages in the wider
SpaDES ecosystem (SpaDES.core, SpaDES, LandR, NetLogoR, map, scfmutils,
SpaDES.experiment, fireSenseUtils, LandWebUtils, usefulFuns) and saw 0 new
problems.
