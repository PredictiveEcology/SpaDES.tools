if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".N", "row_number"))
}

################################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced \code{data.frame} or \code{data.table} that has at least one
#' column of codes that are represented in the \code{fullRaster}.
#'
#' @param fullRaster \code{RasterLayer} of codes used in \code{reduced} that
#'                   represents a spatial representation of the data.
#'
#' @param newRasterCols Character vector, length 1 or more, with the name(s) of
#'                      the column(s) in \code{reduced} whose value will be
#'                      returned as a \code{Raster} or list of \code{Raster}s.
#'
#' @param mapcode a character, length 1, with the name of the column in \code{reduced}
#'                that is represented in \code{fullRaster}.
#'
#' @param ... Other arguments. None used yet.
#'
#' @return A \code{RasterLayer} or list of \code{RasterLayer} of with same
#' dimensions as \code{fullRaster} representing
#' \code{newRasterCols} spatially, according to the join between the \code{mapcode}
#' contained within \code{reduced} and \code{fullRaster}
#'
#' @seealso \code{\link{raster}}
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table := data.table key setkeyv setnames
#' @importFrom raster extent getValues raster res
#' @rdname rasterizeReduce
#'
#' @example inst/examples/example_mapReduce.R
#'
rasterizeReduced <- function(reduced, fullRaster, newRasterCols,
                             mapcode = names(fullRaster), ...) {
  reduced <- data.table(reduced)
  if (!is.null(key(reduced))) {
    if (key(reduced) != mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }
  fullRasterVals <- data.table(getValues(fullRaster))
  setnames(fullRasterVals, 1, new = mapcode)
  fullRasterVals <- fullRasterVals[, row_number := 1L:.N] # nolint
  setkeyv(fullRasterVals, mapcode)

  bSumVec <- reduced[fullRasterVals] # join
  if (length(newRasterCols) > 1) {
    for (i in seq_along(newRasterCols)) {
      bSumVec[is.na(get(newRasterCols[i])), c(newRasterCols[i]) := NA]
    }
  } else {
    bSumVec[is.na(get(newRasterCols)), c(newRasterCols) := NA]
  }
  setkey(bSumVec, row_number)
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
