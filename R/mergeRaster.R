#' @details \code{mergeRaster} differs from \code{merge} in how overlapping tile
#' regions are handled: \code{merge} retains the values of the first raster in
#' the list. This has the consequence of retaining the values from the buffered
#' region in the first tile in place of the values from the neighbouring tile.
#' On the other hand, \code{mergeRaster} retains the values of the tile region,
#' over the values in any buffered regions. This is useful for reducing edge
#' effects when performing raster operations involving contagious processes.
#' To use the average of cell values, or do another computation, use
#' \code{\link[raster]{mosaic}}.
#' \code{mergeRaster} is also faster than \code{merge}. \code{mergeRaster} also
#' differs from \code{\link[raster]{mosaic}} in speed and hability to take a
#' raster list. It can, however, use the average of cell values, or do other
#' computations. At last, \code{mergeRaster} can also merge tiles of a split
#' raster that were resampled and, therefore, could have had different changes
#' in the buffer sizes on each side of the raster. If the user resamples the
#' tiles and the new resolution is not a multiple of the original one,
#' \code{mergeRaster} will use mosaic with the max function to merge the tiles
#' with a message.
#'
#' @param x    A list of split raster tiles (i.e., from \code{splitRaster}).
#'
#' @return \code{mergeRaster} returns a \code{RasterLayer} object.
#'
#' @seealso \code{\link[raster]{merge}}, \code{\link[raster]{mosaic}}
#'
#' @author Yong Luo, Alex Chubaty, Tati Micheletti & Ian Eddy
#' @export
#' @importFrom magrittr %>%
#' @importFrom raster crop extent merge mosaic alignExtent extent
#' @rdname splitRaster
#'
setGeneric("mergeRaster", function(x) {
  standardGeneric("mergeRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "mergeRaster",
  signature = signature(x = "list"),
  definition = function(x) {
    xminExtent <- sapply(x, xmin) %>% unique() %>% sort() # nolint
    xmaxExtent <- sapply(x, xmax) %>% unique() %>% sort() # nolint
    yminExtent <- sapply(x, ymin) %>% unique() %>% sort() # nolint
    ymaxExtent <- sapply(x, ymax) %>% unique() %>% sort() # nolint
    xBuffer <- unique((xmaxExtent[-length(xmaxExtent)] - xminExtent[-1]) / 2) # nolint
    yBuffer <- unique((ymaxExtent[-length(ymaxExtent)] - yminExtent[-1]) / 2) # nolint
    browser()
    if (any(length(xBuffer) > 1, length(yBuffer) > 1)){
      message(paste0("The tiles present different buffers (likely due to resampling).",
                     " mergeRaster() will use raster::mosaic()."))
      for (i in seq_along(x)) {
        if (i == 1) next
        rTemplate <- x[[1]]
        raster::extent(x[[i]]) <- raster::alignExtent(extent = raster::extent(x[[i]]),
                                                      object = rTemplate,
                                                      snap = "near")
      }
      rasMosaicArgs <- x
      rasMosaicArgs$fun <- mean
      y <- do.call(what = raster::mosaic, args = rasMosaicArgs)
    } else {
      for (i in seq_along(x)) {
        r <- x[[i]]
        if (xmin(r) != min(xminExtent)) {
          xminCut <- xmin(r) + xBuffer
        } else {
          xminCut <- xmin(r)
        }
        if (xmax(r) != max(xmaxExtent)) {
          xmaxCut <- xmax(r) - xBuffer
        } else {
          xmaxCut <- xmax(r)
        }
        if (ymin(r) != min(yminExtent)) {
          yminCut <- ymin(r) + yBuffer
        } else {
          yminCut <- ymin(r)
        }
        if (ymax(r) != max(ymaxExtent)) {
          ymaxCut <- ymax(r) - yBuffer
        } else {
          ymaxCut <- ymax(r)
        }
        x[[i]] <- crop(r, extent(xminCut, xmaxCut, yminCut, ymaxCut))
      }
      y <- do.call(raster::merge, x)
    }
    names(y) <- gsub("_tile[0-9].*$", "", names(x[[1]]))
    return(y)
})
