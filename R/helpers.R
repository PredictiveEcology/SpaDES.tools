# NA-aware comparison of two vectors
# Copied from http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/.
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' @importFrom reproducible .requireNamespace
extnt <- function(x, ...) {
  if (inherits(x, "Extent") || inherits(x, "SpatExtent")) {
    return(x)
  } else if (inherits(x, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    return(raster::extent(x))
  } else if (inherits(x, "SpatRaster") || inherits(x, "sf")) {
    if (inherits(x, "SpatRaster")) .requireNamespace("terra", stopOnFALSE = TRUE)
    if (inherits(x, "sf")) .requireNamespace("sf", stopOnFALSE = TRUE)
    return(terra::ext(x))
  } else if (inherits(x, "numeric")) {
    return(terra::ext(x, ...))
  } else {
    if (inherits(x, "matrix") &&
        identical(colnames(x), c("min", "max")) &&
        identical(rownames(x), c("s1", "s2"))) {
      return(terra::ext(x))
    } else {
      stop(sprintf("Unable to determine extent of object of type '%s'.", is(x)[1]))
    }
  }
}

#' @importFrom reproducible .requireNamespace
`extnt<-` <- function(x, value) {
  if (inherits(x, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    if (inherits(value, "Extent")) {
      raster::extent(x) <- value
    } else if (inherits(value, "SpatExtent")) {
      .requireNamespace("terra", stopOnFALSE = TRUE)
      x <- terra::rast(x)
      terra::ext(x) <- value
      x <- raster::raster(x)
    }
  } else if (inherits(x, "SpatRaster")) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    if (inherits(value, "Extent")) {
      .requireNamespace("raster", stopOnFALSE = TRUE)
      terra::ext(x) <- terra::ext(value)
    } else if (inherits(value, "SpatExtent")) {
      terra::ext(x) <- value
    }
  }

  return(x)
}

#' Wrapper for `quickPlot::Plot`
#'
#' During transition between `terra` and `raster`, and `sp` and `sf`,
#' there are situations where we want to use Plot, but we have `SpatRaster` objects.
#' `quickPlot` is not yet fully functional for `SpatRaster` class yet.
#' This wrapper can be used.
#'
#' @details
#' A pass through for `Plot`, but with a conversion of `SpatRaster` to `RasterLayer`.
#'
#' @param ... passed to `quickPlot::Plot`
#'
#' @return invoked for side effect of creating a plot.
#'
#' @export
#' @importFrom rlang list2
#' @importFrom graphics plot
Plot2 <- function(...) {
  if (requireNamespace("quickPlot", quietly = TRUE)) {
    dots <- rlang::list2(...)
    spats <- vapply(dots, inherits, "SpatRaster", FUN.VALUE = logical(1))
    if (any(spats) && requireNamespace("raster", quietly = TRUE)) {
      dots[spats] <- lapply(dots[spats], raster::raster)
      nams <- vapply(substitute(placeholderFunction(...))[-1][spats], deparse, backtick = TRUE,
                     FUN.VALUE = character(1))
      names(dots)[spats] <- nams
      do.call(quickPlot::Plot, append(list(dots[spats]), dots[!spats]))
    } else {
      quickPlot::Plot(...)
    }
  } else {
    plot(...)
  }
}
