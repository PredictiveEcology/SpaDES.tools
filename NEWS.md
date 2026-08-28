# SpaDES.tools (development version)

# SpaDES.tools 2.1.3

## Deprecated and defunct
* `wrap()` is deprecated in favour of `wrapTorus()`, which does the same thing
  under a name that does not collide with `terra::wrap()` (an unrelated
  function that serialises a `SpatRaster` for saving). `wrap()` still works and
  redirects.
* `transitions()` is defunct. It was never fully implemented and there is no
  replacement; please contact the maintainer if you have a use for it.
* `rndmPolygonSpatialPoints()` and `rndmPolygonSpatialPolygons()` (already
  deprecated) now redirect to their `SpatVector` equivalents and convert the
  result back to `sp`, instead of carrying their own copies of the geometry
  code.
* Passing a `SpatialPoints*` object to `crw()` now warns. It is converted to a
  `SpatVector` on entry and converted back on return.

## Enhancements
* `spread2()` now accepts a numeric vector for `spreadProbRel`, not just a raster. A raster `spreadProbRel` is re-materialised in full on every iteration in order to subset a handful of cells from it, which dominates runtime for callers that step one iteration at a time (`iterations = 1L` in a loop); passing a pre-materialised vector avoids this. Rasters and the scalar `NA` default are unaffected, and results are unchanged.
* `adj()` now delegates to `terra::adjacent()` for the 4-neighbour,
  8-neighbour and bishop cases, including `match.adjacent` and `include`. This
  removes a large amount of duplicated neighbour-finding code. The `torus`
  case keeps its own integer arithmetic, which measured roughly 3x faster than
  routing it through `wrapTorus()`.
* `crw()` handles `sp` input by converting it once on entry and converting
  back on return, rather than through a separate code path. See the bug fixes
  below for what that path had been doing.

## Behaviour changes
* `rings(returnDistances = FALSE)` now writes the ring `id` into the returned
  raster. Both branches previously wrote distances, so the argument changed
  only the background value (`0` rather than `NA`) and not the ring cells.
  Writing ids matches the `allowOverlap = TRUE` branch, which writes summed
  ids in the same situation, and matches the example in `?rings`, which fills
  the raster with `rngs$id` by hand.

## Bug fixes
* `heading()` returned `0` instead of `180` for a due-south bearing. The
  quadrant corrections keyed off the sign of the x offset, which is zero due
  south, so neither correction applied; it now uses `atan2()`, which resolves
  all four quadrants directly.
* `dwrpnorm2()` failed for any `mu` longer than 1, with "argument is not
  numeric or logical". A leftover line recycled a variable named `var`, which
  resolved to `stats::var` -- the function, not a value.
* `randomStudyArea(seed = )` did not restore the caller's random number
  stream. It called `set.seed()` on the saved state vector, which takes a
  single number, so the stream was reseeded from that vector's first element
  rather than restored.
* `crw()` given a `SpatialPoints*` object silently dropped the `x1`/`y1`
  columns it documents (returning a `SpatialPointsDataFrame` with no columns),
  and returned a plain matrix rather than an `sp` object when that result was
  passed back in. Both are in the example in `?crw`.
* `move()` checked for a `NULL` hypothesis after comparing it, so the check
  could never run: the comparison raised "argument is of length zero" first.
* `crw()` accepted a `SpatVector` of polygons and treated its vertices as
  agent positions. The guard meant to reject non-point geometries was nested
  inside a condition that could only be true when the object was not a
  `SpatVector` at all.
* `numPerPatchMap` example in `specificNumPerPatch()` remapped values in
  place, so later values could collide with ones already rewritten.

## Documentation and testing
* The README, which is the home page of the package website, has been
  rewritten: what the package is for with runnable examples, accurate
  installation requirements (including the GDAL/GEOS/PROJ system libraries
  that `terra` needs), and per-branch build and coverage badges.
* Test coverage raised from 66.7% to 83.5%, which is where most of the bug
  fixes above came from.

# SpaDES.tools 2.1.2

## Enhancements
* `spread()` and `spread2()` are now faster: the per-iteration `adj()` call
  has been replaced with surgical Rcpp helpers (`adjPairsMatrix()` for
  `spread()`, `adjPairsWithId()` for `spread2()`) that compute neighbour
  pairs and apply edge filtering in C++. Output is value-identical to the
  previous implementation across a multi-seed verification battery (the
  `pixels`/`initialPixels` columns of `spread2()`'s data.table are now
  integer rather than numeric, but values are unchanged).
  Measured speedups: `spread2` ~1.4×, `spread` ~1.6–2.3× depending on
  `spreadProb` and scenario.
* `spread()`'s no-flags branch (no `id`/`returnIndices`/`circle`/
  `relativeSpreadProb`/`neighProbs` set, non-matrix path) also now goes
  through `adjPairsMatrix()` so the inner loop no longer calls `adj()`.
* New seeded determinism tests for `spread()` and `spread2()`: each runs a
  parameter grid twice with the same seed and asserts bit-identical output.
* New drop-in equivalence tests (`test-spread-snapshots.R`): assert that
  `spread()` / `spread2()` output matches RDS snapshots captured from the
  pre-Rcpp baseline across 30 seeded scenarios (15 `spread` × 3 seeds and
  16 `spread2` × 3 seeds). Snapshots and the regeneration script live in
  `tests/testthat/_spread_snapshots/`.
* `rasterizeReduced()`: improved speed (#103). Benchmark on a 2000×2000
  raster with 200 codes: single-column ~5× (0.65 s → 0.13 s), multi-column
  ~3.7× (0.69 s → 0.18 s).
* `neutralLandscapeMap()` now uses a built-in generator
  (`type = "gaussian"`, the only supported `type`) that fills a padded grid
  with i.i.d. normal noise and smooths it with a square mean kernel,
  producing a roughly Gaussian random field. Use the new `smooth` argument
  to control autocorrelation length.
* **Breaking:** the `NLMR`-backed `type` values (`"nlm_mpd"`,
  `"nlm_gaussianfield"`, etc.) have been removed from
  `neutralLandscapeMap()`, and the `NLMR` dependency has been dropped
  entirely (removed from `Suggests`, `Remotes`, and
  `Additional_repositories`). The built-in `"gaussian"` generator replaces
  this functionality with no external dependency
  (PredictiveEcology/SpaDES.core#334).

# SpaDES.tools 2.1.1

* `reproducible:::isGridded` now exported as `.isGridded` (#99);

# SpaDES.tools 2.0.9

* cleanup test files left behind in `distanceFromEachPoint`;

# SpaDES.tools 2.0.8

## Dependency changes
* drop support for R 4.2;

## Enhancements
* improved `spread` for use with `SpatRasters` (#93);
* improved speed of `.pointDistance` (#97);
* fix a failing test when using `testthat > 3.2.3` (#98);
* documentation improvements;

# SpaDES.tools 2.0.7

## Bug fixes
* fixed issue with `rasterizeReduced()`;
* updates to deal with changes to RNG in `dqrng` (#96; @rstub). if backwards compatibility is needed, set `dqrng::dqRNGkind("Xoroshiro128+")` before running `spread` to ensure numerical reproducibility with previous versions;

# SpaDES.tools 2.0.6

## Enhancements
* minor documentation updates

## Dependency changes
* drop support for R 4.1 due to changes in dependency packages

## Bug fixes
* none

# SpaDES.tools 2.0.5

## Enhancements
* `randomPolygons()` works with `sf` objects;
* use numeric vector in `crw` to accommodate downstream dependency changes;

## Dependency changes
* requires `reproducible (>= 2.0.9)`

## Bug fixes
* fixed `splitRaster()` tests that broke due to changes in `reproducible::Filenames()` (#91)

# SpaDES.tools 2.0.4

## Enhancements
* improved documentation
* address new features of `quickPlot`
* minor modifications to `crw` to allow downstream packages that use `matrix` classes instead of `sf` or `SpatVector` classes for "agents"
* address timing issue on CRAN machines

## Dependency changes
* removed Suggests dependencies `CircStats`, `dplyr`, `purrr`, `DBI`, `RColorBrewer`, `googledrive`, `snow`

## Bug fixes
* none

# SpaDES.tools 2.0.0

## Enhancements
* wholesale transition to `terra` from `raster` and `sp`. Some functionality is available for `sf`, but this will not be complete.
* `splitRaster()` uses `".tif"` as the default raster file type
* most example plotting uses `terra::plot` directly, but in a few cases, `quickPlot::Plot` is used as it is much better (e.g., `spread2(plot.it = TRUE)`)

## Dependency changes
* drop support for R 4.0 (dependency package `reproducible` supports R >= 4.1);
* now has only 7 Imports and Depends, which becomes 15 recursive dependencies (down from 40)
* add `terra` to Imports
* removed dependencies `magrittr`, `rgeos`, `rlang`, `Require`, `graphics`
* added to `Suggests`: `withr`
* moved to Suggests: `quickPlot`, `fastmatch`, `raster`, `sp`, `sf`, `CircStats`
* added `snow` to Suggests as it's used for parallel `raster` operations;
* added `tools` to Suggests for `file.ext()`

## Bugfixes
* all open issues dealt with
* fixes related to migration to `terra`
* other minor bug fixes

# SpaDES.tools 1.0.2

## Dependency changes
* none

## Enhancements
* `terra` compatibility added
* `neutralLandscapeMap` expanded to use more `NLMR` functions

## Bug Fixes
* suppress additional spurious warnings

# SpaDES.tools 1.0.1

## Enhancements
* a new function, `neutralLandscapeMap`, replaces the defunct `gaussMap` in creating a neutral landscape map using the midpoint displacement algorithm available in `NLMR::nlm_mpd`.

# SpaDES.tools 1.0.0

This is a major `SpaDES.tools` update causing breaking changes due to multiple CRAN packages no longer available as of R version 4.2 released April 2022.

## Dependency changes
* Removed dependencies `ffbase` and `ff` and deprecated use of `lowMemory` argument in `spread` (`ffbase` was removed from CRAN April 2022).
* Removed dependency `RandomFields` which is no longer maintained. We had to drop support for random landscape generation via `gaussMap()`; but see the `NLMR` package for tools to create random neutral landscapes.

## Enhancements
* `mergeRaster()` can deal with rasters with different origins extents (i.e., that were not produced using `splitRaster()`)

## Bug Fixes
* `spread2` was unable to use `maxSize` when `start` was the output of a previous `spread2` call to which `maxSize` had not been supplied. This has now been fixed.

# SpaDES.tools 0.3.10

## Dependency changes
* drop support for R 3.6 (#65)

## Enhancements
* none

## Bug Fixes
* fix error in `spread2()` causing failures on R-devel

# SpaDES.tools 0.3.9

## Dependency changes
* `Require` moved to `Suggests`, because it's only used in examples.

## Enhancements
* none

## Bug Fixes
* fixed misc CRAN check issues with Suggested packages
* fixed bug in `splitRaster()` (#62; @anbm-dk)

# SpaDES.tools 0.3.8

## Dependency changes
* Dropped support for R 3.5 due to changes in dependency packages. R versions 3.6 and higher are supported.

## Enhancements
* `distanceFromEachPoint`: `cl` argument can now accept a numeric scalar indicating how many cores to use with an `mclapply` internally
* `distanceFromEachPoint`: minor speed improvements
* `spread3` has new arguments (`sdDist`, `dispersalKernel`)
* improved speed for `spread` under most conditions, especially larger events, possibly up to 2x.
* `splitRaster()` can now specify file extension, instead of defaulting to `.grd`
* several under the hood improvements for stability, speed

## Bug Fixes
* `distanceFromEachPoint` bugfixes under some cases (some values of `toCells`, `angles` would cause errors)

# SpaDES.tools 0.3.6

## Dependency changes
* none

## Bug Fixes
* `spread` with `allowOverlap = TRUE` would give wrong results. Fixed.
* update a test to deal with forthcoming `raster` changes.

## Enhancements
* none

# SpaDES.tools 0.3.5

We have identified a non-trivial bug in `spread` when using `allowOverlap = TRUE`.
Until we can patch this bug, using this argument value will generate an error.
We are currently working on a fix, which may take some time.

## Dependency changes
* removed orphaned package `bit`; its reverse dependencies, `ff` and `ffbase`, moved to Suggests

## Bugfixes
* none

## Enhancements
* `spread` sped up for `allowOverlap = TRUE`
* New values for `returnIndices` arg in `spread`, allowing for `numeric`. New option, `2` allows for the fastest return possible, only `pixelIndices`

# SpaDES.tools 0.3.4

## Dependency changes
* none

## Bugfixes
* fix issues with failing tests on CRAN

## Enhancements
* none

# SpaDES.tools 0.3.3

## Dependency changes
* Requires R >= 3.5
* Added `animation` since it is used in the `spread3` example.
* Added `backports` for R-oldrel support
* Removed `profvis` and `microbenchmark` from Suggests: all benchmarking is removed from automated tests

## Bugfixes
* minor Bug Fixes

## Enhancements
* `spread2` speed enhancements

# SpaDES.tools 0.3.2

## Dependency changes
* Added `animation`, `fasterize`, and `sf` to suggests (used in examples)

## Bugfixes
* minor bug fixes

## Enhancements
* `spread2` now takes a numeric for `allowOverlap`, to allow for 1 more scenario of overlapping, namely, allow between event overlapping, and within event overlapping only if the overlap occurs in the same iteration, otherwise, do not allow overlap within event.
* new function `spread3` for simulating *e.g.*, insect spread
* `pointDistance` is now exported

# SpaDES.tools 0.3.1

## Dependency changes
* `RandomFields` moved to Suggests to prevent clashes with `data.table` multithreading

## Enhancements
* `randomPolygon` now has `area` argument, instead of `hectares` (deprecated), and it now succeeds for more cases.
* `mergeRaster` now accepts a function passed by the user when `mosaic` is triggered (#55, @tati-micheletti)
* `initiateAgents` example fixed to work with newer `raster` package versions (@rhijmans)

# SpaDES.tools 0.3.0

## New features
* `splitRaster` defaults to in-memory rasters when no file path specified (#47, @ianmseddy).

## Removed features
* Previously deprecated functions removed to fix CRAN check WARNINGs. These are now located in the `reproducible` package.
* `compiler` package no longer used; is automatic in R >= 3.5

# SpaDES.tools 0.2.0

## New features
* new arguments added to `gaussMap` (#20, @ianmseddy).
* move `fastMask` to `reproducible` package

## Deprecated functions
* `fastCrop()`: `raster::crop` is faster than `velox::crop` under many tests.
* `fastMask()`: moved to the `reproducible` package.

## Bug Fixes
* minor bug fixes (`spread2`)
* minor test fixes.

## Dependency changes
* `dplyr` and `mgcv` added to Suggests because they are used in tests.

# SpaDES.tools 0.1.1

* add more detailed information to package Description, per CRAN.
* add `randomPolygon` function for single random polygon.
* speed improvements with `spread2` when `asymmetry` used.
* minor bug fixes.

# SpaDES.tools 0.1.0

* A new package, which takes all auxiliary modelling functions out of the `SpaDES` package:

    - see `?SpaDES.tools::SpaDES.tools` for an overview of included functionality.
