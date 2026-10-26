#' Differences between `spread` and `spread2`
#'
#' [spread()] and [spread2()] implement similar contagious spread processes, but
#' they are separate implementations with different argument names and,
#' importantly, **different output column names**. Code cannot be switched from
#' one to the other simply by renaming the function. This topic documents the
#' correspondences; it is referenced from the help of both functions.
#'
#' @section Differences between `spread` and `spread2`:
#'
#' **Which to use.** [spread2()] is the more robust of the two: its behaviour is
#' better tested, it can be called iteratively (passing its own output back in as
#' `start`), and more combinations of its arguments work as documented.
#' [spread()] is generally faster, sometimes substantially so, but several of its
#' argument combinations are untested, deprecated, or unimplemented.
#'
#' **Argument correspondence.** Arguments with the same name in both functions
#' generally mean the same thing, with the exceptions noted below.
#'
#' \tabular{lll}{
#'   **`spread`** \tab **`spread2`** \tab **Notes** \cr
#'   `loci` \tab `start` \tab In `spread2`, `start` may also be the `data.table`
#'     or raster returned by a previous `spread2` call. \cr
#'   `persistence` \tab `persistProb` \tab Default `0` vs `NA` (`NA` behaves as `0`). \cr
#'   `returnIndices` \tab `asRaster` \tab `spread` returns a raster when
#'     `returnIndices = FALSE`, a `data.table` when `1`/`TRUE`, and a bare vector
#'     of pixel indices when `2`. `spread2` returns a raster when
#'     `asRaster = TRUE` (the default) and a `data.table` when `FALSE`; it has no
#'     equivalent of `returnIndices = 2`. \cr
#'   `exactSizes` \tab `exactSize` \tab Different types. `spread`'s `exactSizes`
#'     is a logical that reinterprets `maxSize` as exact; `spread2`'s `exactSize`
#'     is itself the numeric vector of target sizes. \cr
#'   `spreadState` \tab `start` \tab `spread` takes previous state in a dedicated
#'     argument; `spread2` takes it as `start`, carrying state in its attributes. \cr
#'   `quick`, `skipChecks` \tab `skipChecks` \tab `spread` accepts either name. \cr
#'   `relativeSpreadProb` \tab `spreadProbRel` \tab Different mechanisms: a logical
#'     rescaling flag vs an actual surface of relative weights. \cr
#'   `allowOverlap` \tab `allowOverlap` \tab Logical in `spread`; numeric `0:3`
#'     in `spread2` (logical accepted for back-compatibility). \cr
#'   `maxSize` \tab `maxSize` \tab `spread` has default `1e8`; `spread2` has no
#'     default and distinguishes supplied from missing. \cr
#'   `mask`, `mapID`, `lowMemory` \tab -- \tab Not present in `spread2`; all three
#'     are unimplemented or deprecated in `spread` as well. \cr
#'   `id`, `stopRule`, `stopRuleBehavior`, `circleMaxRadius` \tab -- \tab
#'     No equivalent in `spread2`. \cr
#'   -- \tab `returnDirections`, `returnFrom`, `oneNeighbourOnly`,
#'     `maxRetriesPerID` \tab No equivalent in `spread`. \cr
#' }
#'
#' **Output column names.** This is the most common source of errors when
#' converting code. The returned `data.table`s share *no* column names:
#'
#' \tabular{lll}{
#'   **Meaning** \tab **`spread(returnIndices = TRUE)`** \tab **`spread2(asRaster = FALSE)`** \cr
#'   Cell touched by the spread \tab `indices` \tab `pixels` \cr
#'   Cell the event started from \tab `initialLocus` \tab `initialPixels` \cr
#'   Whether the cell can still spread \tab `active` (logical) \tab `state`
#'     (character: `"activeSource"`, `"successful"`, `"inactive"`, `"holding"`,
#'     `"tooSmall"`) \cr
#'   Event identifier \tab `id` \tab not in the main table; see
#'     `attr(out, "spreadState")$clusterDT$id` \cr
#'   Distance from origin \tab `dists` (only with `circle`/`allowOverlap`) \tab
#'     `distance` (with `returnDistances`) \cr
#'   Source of the previous step \tab -- \tab `from` (with `returnFrom`) \cr
#' }
#'
#' @seealso [spread()], [spread2()]
#' @name spreadVsSpread2
#' @rdname spreadVsSpread2
#' @keywords internal
NULL
