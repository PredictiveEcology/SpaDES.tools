Known issues: https://github.com/PredictiveEcology/SpaDES.tools/issues

version 0.1.1.9000
=============

* minor bugfixes (`spread2`)
* minor test fixes
* new arguments added to `gaussMap` (#20, @ianmseddy).
* move `fastMask` to `reproducible` package
* remove `fastRasterize` as `fasterize::fasterize` is better and on CRAN, and `fastCrop` as `raster::crop` is faster than `velox` `crop` under many tests

## Dependency changes

* add `mgcv`, `dplyr` to Suggests because it's used in tests.


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
