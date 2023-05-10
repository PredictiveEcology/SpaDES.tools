# NA-aware comparison of two vectors
# Copied from http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/.
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' Wrapper for quikcPlot::Plot
#'
#' During transition between `terra` and `raster`, and `sp` and `sf`,
#' there are situations where we want to use Plot, but we have `SpatRaster` objects.
#' `quickPlot` is not yet fully functional for `SpatRaster` class yet. This wrapper
#' can be used.
#' @export
#' @param ... passed to quickPlot::Plot
#' @details
#' A pass through for `Plot`, but with a conversion of `SpatRaster` to `RasterLayer`
#'
#' @return
#' Called for side effects, namely a plot.
#'
Plot2 <- function(...) {
  if (requireNamespace("quickPlot")) {
    dots <- list(...)
    dots <- rlang::list2(...)
    spats <- vapply(dots, inherits, "SpatRaster", FUN.VALUE = logical(1))
    if (any(spats) && requireNamespace("raster")) {
      dots[spats] <- lapply(dots[spats], raster::raster)
      nams <- vapply(substitute(placeholderFunction(...))[-1][spats], deparse, backtick = TRUE, FUN.VALUE = character(1))
      names(dots)[spats] <- nams
      do.call(Plot, append(list(dots[spats]), dots[!spats]))
    } else {
      Plot(...)
    }
  } else {
    plot(...)
  }
}
