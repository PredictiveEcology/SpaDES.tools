if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("..colsToKeep", ".N", "row_number"))
}

################################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced `data.frame` or `data.table` that has at least one
#' column of codes that are represented in the `fullRaster`.
#'
#' @param fullRaster `RasterLayer` of codes used in `reduced` that
#'                   represents a spatial representation of the data.
#'
#' @param newRasterCols Character vector, length 1 or more, with the name(s) of
#'                      the column(s) in `reduced` whose value will be
#'                      returned as a `Raster` or list of `Raster`s.
#'
#' @param mapcode a character, length 1, with the name of the column in `reduced`
#'                that is represented in `fullRaster`.
#'
#' @param ... Other arguments. None used yet.
#'
#' @return A `RasterLayer` or list of `RasterLayer` of with same
#' dimensions as `fullRaster` representing
#' `newRasterCols` spatially, according to the join between the `mapcode`
#' contained within `reduced` and `fullRaster`
#'
#' @seealso [raster()]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table := data.table key setkeyv setnames
#' @importFrom raster extent getValues raster res
#' @rdname rasterizeReduced
#'
#' @example inst/examples/example_mapReduce.R
#'
rasterizeReduced <- function(reduced, fullRaster, newRasterCols, mapcode = names(fullRaster), ...) {
  if (!is.data.table(reduced))
    reduced <- data.table::setDT(reduced)

  if (!is.null(key(reduced))) {
    if (key(reduced) != mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }
  fullRasterVals <- as.data.table(list(getValues(fullRaster)))
  setnames(fullRasterVals, 1, new = mapcode)
  set(fullRasterVals, NULL, "row_number", seq(ncell(fullRaster)))
  setkeyv(fullRasterVals, mapcode)

  colsToKeep <- c(mapcode, newRasterCols)
  BsumVec <- reduced[, ..colsToKeep][fullRasterVals] # join

  # This was removed by Eliot May 28, 2019 -- seems redundant -- if there are errors, this may be why
  # if (length(newRasterCols) > 1) {
  #   for (i in seq_along(newRasterCols)) {
  #     BsumVec[is.na(get(newRasterCols[i])), c(newRasterCols[i]) := NA]
  #   }
  # } else {
  #   browser()
  #   BsumVec[is.na(get(newRasterCols)), c(newRasterCols) := NA]
  # }
  setkeyv(BsumVec, "row_number")
  if (length(newRasterCols) > 1) {
    ras <- list()
    for (i in newRasterCols) {
      ras[[i]] <- raster(res = res(fullRaster), ext = extent(fullRaster), crs = crs(fullRaster),
                         vals = BsumVec[[i]])
    }
  } else {
    ras <- raster(res = res(fullRaster), ext = extent(fullRaster), crs = crs(fullRaster),
                  vals = BsumVec[[newRasterCols]])
  }
  return(ras)
}
