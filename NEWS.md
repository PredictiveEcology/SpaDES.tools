Known issues: https://github.com/PredictiveEcology/SpaDES.tools/issues

version 0.3.2.9000
=============

## Dependency changes
* Added `backports` for R-oldrel support
* Requires R >= 3.5

## Bugfixes

## Enhancements
* `spread2` speed enhancements

version 0.3.2
=============

## Dependency changes
* Added `animation`, `fasterize`, and `sf` to suggests (used in examples)

## Bugfixes
* minor bug fixes

## Enhancements
* `spread2` now takes a numeric for `allowOverlap`, to allow for 1 more scenario of overlapping, namely, allow between event overlapping, and within event overlapping only if the overlap occurs in the same iteration, otherwise, do not allow overlap within event.
* new function `spread3` for simulating *e.g.*, insect spread
* `pointDistance` is now exported

version 0.3.1
=============

## Dependency changes
* `RandomFields` moved to Suggests to prevent clashes with `data.table` multithreading

## Enhancements
* `randomPolygon` now has `area` argument, instead of `hectares` (deprecated), and it now succeeds for more cases.
* `mergeRaster` now accepts a function passed by the user when `mosaic` is triggered (#55, @tati-micheletti)
* `initiateAgents` example fixed to work with newer `raster` package versions (@rhijmans)

version 0.3.0
=============

## New features
* `splitRaster` defaults to in-memory rasters when no file path specified (#47, @ianmseddy).

## Removed features
* Previously deprecated functions removed to fix CRAN check WARNINGs. These are now located in the `reproducible` package.
* `compiler` package no longer used; is automatic in R >= 3.5

version 0.2.0
=============

## New features
* new arguments added to `gaussMap` (#20, @ianmseddy).
* move `fastMask` to `reproducible` package

## Deprecated functions
* `fastCrop()`: `raster::crop` is faster than `velox::crop` under many tests.
* `fastMask()`: moved to the `reproducible` package.

## Bug fixes
* minor bug fixes (`spread2`)
* minor test fixes.

## Dependency changes
* `dplyr` and `mgcv` added to Suggests because they are used in tests.

version 0.1.1
=============

* add more detailed information to package Description, per CRAN.
* add `randomPolygon` function for single random polygon.
* speed improvements with `spread2` when `asymmetry` used.
* minor bug fixes.

version 0.1.0
=============

* A new package, which takes all auxiliary modelling functions out of the `SpaDES` package:

    - see `?SpaDES.tools::SpaDES.tools` for an overview of included functionality.
