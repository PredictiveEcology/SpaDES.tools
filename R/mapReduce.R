if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("..colsToKeep", ".N", "row_number"))
}

################################################################################
#' Convert reduced representation to full raster
#'
#' @param reduced `data.frame` or `data.table` that has at least one
#' column of codes that are represented in the `fullRaster`.
#'
#' @param fullRaster `RasterLayer`/`SpatRaster` of codes used in `reduced` that
#'                   represents a spatial representation of the data.
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
#' @seealso [raster()]
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table := data.table key setkeyv setnames
#' @importFrom raster raster
#' @importFrom terra ext values rast res levels
#' @rdname rasterizeReduced
#'
#' @example inst/examples/example_mapReduce.R
#'
rasterizeReduced <- function(reduced, fullRaster, newRasterCols, mapcode = names(fullRaster), ...) {

  browser()
  if (!inherits(fullRaster, c("Raster", "SpatRaster"))) {
    stop("fullRaster must be a Raster or SpatRaster")
  }

  ## don't use rasterRead; rasterizweReduced can be used independently of reproducible
  if (is(fullRaster, "Raster")) {
    rasterFUN <- function(...)
      raster(...)
  } else {
      rasterFUN <- function(...)
        rast(...)
  }

  if (!is.data.table(reduced))
    reduced <- data.table::setDT(reduced)

  if (!is.null(key(reduced))) {
    if (key(reduced) != mapcode) {
      setkeyv(reduced, mapcode)
    }
  } else {
    setkeyv(reduced, mapcode)
  }
  fullRasterVals <- as.data.table(list(as.vector(values(fullRaster))))
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
      ras[[i]] <- rasterFUN(fullRaster)
      names(ras[[i]]) <- names(rasterFUN())

      if (is.factor(BsumVec[[i]]) && is(ras, "SpatRaster")) {
        ras[[i]][] <- as.numeric(BsumVec[[i]])
        levs <- unique(data.frame(id = na.omit(as.numeric(BsumVec[[i]])),
                                  values = na.omit(BsumVec[[i]])))
        levels(ras[[i]][]) <- levs
      } else {
        ## if factor values are attributed to a RasterLayer,
        ## the attributes table is automatically added
        ras[[i]][] <- BsumVec[[i]]
      }
    }
  } else {
    ras <- rasterFUN(fullRaster)
    names(ras) <- names(rasterFUN())

    if (is.factor(BsumVec[[newRasterCols]]) && is(ras, "SpatRaster")) {
      ras[] <- as.numeric(BsumVec[[newRasterCols]])
      levs <- unique(data.frame(id = na.omit(as.numeric(BsumVec[[newRasterCols]])),
                                values = na.omit(BsumVec[[newRasterCols]])))
      levels(ras) <- levs
    } else {
      ## if factor values are attributed to a RasterLayer,
      ## the attributes table is automatically added
      ras[] <- BsumVec[[newRasterCols]]
    }
  }
  return(ras)
}
