Known issues: https://github.com/PredictiveEcology/SpaDES.tools/issues

version 0.1.1.9000
=============

## New features

* new function `prepInputs` and its internal modular pieces to aid in common GIS problems.
* `prepInputs` gets a logical `overwrite` argument.
* `downloadData` moved from `SpaDES.core` to here.
* new options added to `gaussMap` (#20, @ianmseddy).

## Dependency changes

* add `fasterize` and `sf` to Imports.
* add `mgcv` to Suggests because it's used in tests.

## Bug fixes

* `prepInputs` does more checking for data types and provides more meaningful messages (#41, @tati-micheletti)

version 0.1.1
=============

* add more detailed information to package Description, per CRAN.
* add `randomPolygon` function for single random polygon.
* speed improvements with `spread2` when `asymmetry` used.
* minor bugfixes.

version 0.1.0
=============

* A new package, which takes all auxillary modelling functions out of the `SpaDES` package:

    - see `?SpaDES.tools::SpaDES.tools` for an overview of included functionality.
