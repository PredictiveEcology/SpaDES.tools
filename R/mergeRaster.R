#' @details `mergeRaster` differs from `merge` in how overlapping tile regions
#' are handled: `merge` retains the values of the first raster in the list.
#' This has the consequence of retaining the values from the buffered
#' region in the first tile in place of the values from the neighbouring tile.
#' On the other hand, `mergeRaster` retains the values of the tile region,
#' over the values in any buffered regions.
#' This is useful for reducing edge effects when performing raster operations involving
#' contagious processes.
#'
#' @param x    A list of split raster tiles (i.e., from `splitRaster`).
#' @param fun  Function (e.g. `mean`, `min`, or `max` that
#'             accepts a `na.rm` argument. The default is `mean`.
#'
#' @return `mergeRaster` returns a `RasterLayer` object.
#'
#' @seealso [raster::merge()], [raster::mosaic()]
#'
#' @author Yong Luo, Alex Chubaty, Tati Micheletti & Ian Eddy
#' @export
#' @importFrom magrittr %>%
#' @importFrom raster alignExtent crop extent merge mosaic origin projectRaster stack
#' @rdname splitRaster
#'
setGeneric("mergeRaster", function(x, fun = NULL) {
  standardGeneric("mergeRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "mergeRaster",
  signature = signature(x = "list"),
  definition = function(x, fun) {
    if (length(x) > 1) {
      xminExtent <- sapply(x, xmin) %>% unique() %>% sort() # nolint
      xmaxExtent <- sapply(x, xmax) %>% unique() %>% sort() # nolint
      yminExtent <- sapply(x, ymin) %>% unique() %>% sort() # nolint
      ymaxExtent <- sapply(x, ymax) %>% unique() %>% sort() # nolint
      xBuffer <- if (any(length(xminExtent) == 1, length(xmaxExtent) == 1)) {
        0.0
      } else {
        unique((xmaxExtent[-length(xmaxExtent)] - xminExtent[-1]) / 2) # nolint
      }
      yBuffer <- if (any(length(yminExtent) == 1, length(ymaxExtent) == 1)) {
        0.0
      } else {
        unique((ymaxExtent[-length(ymaxExtent)] - yminExtent[-1]) / 2) # nolint
      }

      ## check that all rasters share same origin (i.e., are aligned)
      origins <- sapply(x, origin)
      if (!do.call(identical , as.list(origins[, 1])) |
          !do.call(identical , as.list(origins[, 2]))) {
        x <- append(x[[1]], lapply(x[-1], function(r) {
          template <- suppressWarnings({
            projectRaster(from = r, to = x[[1]], alignOnly = TRUE)
          })
          suppressWarnings({
            projectRaster(from = r, to = template)
          })
        }))
      }

      if (any(length(xBuffer) > 1, length(yBuffer) > 1)) {
        message(paste0("The tiles present different buffers (likely due to resampling).",
                       " mergeRaster() will use raster::mosaic()."))
        rTemplate <- x[[1]]
        for (i in seq_along(x)) {
          if (i == 1) next
          raster::extent(x[[i]]) <- raster::alignExtent(extent = raster::extent(x[[i]]),
                                                        object = rTemplate,
                                                        snap = "near")
        }
        rasMosaicArgs <- x
        if (!is.null(fun)) {
          rasMosaicArgs$fun <- fun
        } else {
          rasMosaicArgs$fun <- mean
        }
        y <- do.call(what = raster::mosaic, args = rasMosaicArgs) ## TODO: use alist with do.call for spatial objects!!!
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

        y <- do.call(raster::merge, x) ## TODO: use alist with do.call for spatial objects!!!
      }

      if (all(vapply(x, is, logical(1), class2 = "RasterLayer"))) {
        regex_tile <- "_tile[0-9].*$"
        names(y) <- if (any(grepl(regex_tile, sapply(x, names)))) {
          gsub("regex_tile", "", names(x[[1]]))
        } else {
          paste(unique(vapply(x, names, character(1))), collapse = "_")
        }
      } else if (all(sapply(x, is, class2 = "RasterBrick")) |
                 all(sapply(x, is, class2 = "RasterStack"))) {
        names(y) <- names(x[[1]])
        y <- stack(y)
      }
      return(y)
    } else {
      return(x[[1]]) ## original raster if it's the only one in the list
    }
})
