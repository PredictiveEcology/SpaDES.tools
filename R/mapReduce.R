utils::globalVariables(c(".N", ".SD"))

################################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced `data.frame` or `data.table` that has at least one
#' column of codes that are represented in the `fullRaster`.
#'
#' @param fullRaster `RasterLayer`/`SpatRaster` of codes used in `reduced` that
#'                   represents a spatial representation of the data. Note that
#'                   if `fullRaster` is a `factor` `SpatRaster`, the active category
#'                   level values are used, not the IDs (see `terra::activeCat` and
#'                   `terra::cats`)
#'
#' @param newRasterCols Character vector, length 1 or more, with the name(s) of
#'                      the column(s) in `reduced` whose value will be
#'                      returned as a `RasterLayer`/`SpatRaster` or list
#'                      of `RasterLayer`/`SpatRaster`s.
#'
#' @param mapcode a character, length 1, with the name of the column in `reduced`
#'                that is represented in `fullRaster`.
#'
#' @param ... Other arguments. None used yet.
#'
#' @return A `RasterLayer`/`SpatRaster` or list of
#'  `RasterLayer`/`SpatRaster` of with same dimensions as `fullRaster` representing
#'  `newRasterCols` spatially, according to the join between the `mapcode`
#'  contained within `reduced` and `fullRaster`
#'
#' @seealso [terra::rast()]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table as.data.table data.table is.data.table
#' @importFrom terra ext levels rast res values
#' @rdname rasterizeReduced
#'
#' @example inst/examples/example_mapReduce.R
#'
rasterizeReduced <- function(reduced, fullRaster, newRasterCols, mapcode = names(fullRaster), ...) {
  if (!inherits(fullRaster, c("Raster", "SpatRaster"))) {
    stop("fullRaster must be a Raster or SpatRaster")
  }
  isSpat <- is(fullRaster, "SpatRaster")

  ## don't use rasterRead; rasterizeReduced can be used independently of reproducible
  rasterFUN <- if (isSpat) function(x) rast(x) else function(x) raster::raster(x)

  ## as.data.table() not setDT(): setDT mutates the caller's input by reference,
  ## silently converting their data.frame to a data.table.
  if (!is.data.table(reduced))
    reduced <- data.table::as.data.table(reduced)

  ncell_ <- ncell(fullRaster)
  isFactorRas <- if (isSpat) isTRUE(is.factor(fullRaster)[1]) else raster::is.factor(fullRaster)

  ## For factor rasters, `fullRaster[1:ncell_]` returns active-category labels.
  ## For numeric rasters we skip the data.table round-trip entirely -- the
  ## previous as.data.table()-and-setkey path dominated runtime on large rasters.
  if (isFactorRas) {
    fullRasterVals <- fullRaster[1:ncell_]
    if (is.data.frame(fullRasterVals)) fullRasterVals <- fullRasterVals[[1L]]
    if (!isSpat) {
      ## RasterLayer factors need explicit ID -> level translation
      fullRasterVals <- raster::factorValues(fullRaster, fullRasterVals)[[1L]]
    }
    if (is.factor(fullRasterVals)) fullRasterVals <- as.character(fullRasterVals)
  } else if (isSpat) {
    fullRasterVals <- terra::values(fullRaster, mat = FALSE)
  } else {
    fullRasterVals <- as.vector(fullRaster[])
  }

  ## Replace the previous keyed-join + unique() round-trip with a single
  ## match() lookup: for each pixel, find the row in `reduced` whose mapcode
  ## column equals the pixel value. Equivalent to the old join when `reduced`
  ## is unique by mapcode (the documented use case); for duplicated mapcodes
  ## this picks the first occurrence, matching the post-`unique()` behaviour.
  matchIdx <- match(fullRasterVals, reduced[[mapcode]])

  fillRas <- function(col) {
    r <- rasterFUN(fullRaster)
    names(r) <- col
    vals <- reduced[[col]][matchIdx]
    if (is.factor(vals) && isSpat) {
      r[] <- as.numeric(vals)
      levs <- unique(data.frame(id = na.omit(as.numeric(vals)),
                                values = na.omit(vals)))
      levels(r) <- levs
    } else {
      ## if factor values are attributed to a RasterLayer,
      ## the attributes table is automatically added
      r[] <- vals
    }
    r
  }

  if (length(newRasterCols) > 1) {
    ras <- lapply(stats::setNames(newRasterCols, newRasterCols), fillRas)
  } else {
    ras <- fillRas(newRasterCols)
  }
  ras
}
