Known issues: https://github.com/PredictiveEcology/SpaDES.tools/issues

version 0.1.1.9000
=============

* new function `prepInputs` and its internal modular pieces to aid in common GIS problems
* now uses fasterize and sf, optionally, in fastMask
* new options added to `gaussMap` (#20, @ianmseddy)
* `downloadData` moved from `SpaDES.core` to here. This fits well with the new `prepInputs`. Also gets a logical `overwrite` argument. 

version 0.1.1
=============

* add more detailed information to package Description, per CRAN
* add `randomPolygon` function for single random polygon
* speed improvements with `spread2` when `asymmetry` used
* minor bugfixes

version 0.1.0
=============

* A new package, which takes all auxillary modelling functions out of the `SpaDES` package:

    - see `?SpaDES.tools::SpaDES.tools` for an overview of included functionality
