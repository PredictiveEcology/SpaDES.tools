#' Deprecated functions
#'
#' These will be removed in a future release.
#'
#' @export
#' @rdname deprecated
checkGDALVersion <- function() {
  .Deprecated("reproducible::checkGDALVersion")
  reproducible::checkGDALVersion()
}

#' \code{fastCrop} is a wrapper around \code{velox::VeloxRaster_crop}, though
#' \code{raster::crop} is faster under many tests.
#'
#' @param x Raster to crop
#'
#' @inheritParams raster::crop
#'
#' @export
#' @importFrom raster crop extent
#' @importFrom velox velox
#' @seealso \code{velox::VeloxRaster_crop}
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("raster::crop")

  a <- crop(x, y)
  v1 <- velox::velox(x)
  if (is(y, "Raster")) y <- extent(y)
  v1$crop(y)
  if (length(names(x)) > 1) {
    a <- v1$as.RasterStack()
  } else {
    a <- v1$as.RasterLayer(band = 1)
  }
  a
}

#' @export
#' @rdname deprecated
fastMask <- function(x, y) {
  .Deprecated("reproducible::fastMask")
  reproducible::fastMask(x, y)
}

#' @export
#' @rdname deprecated
getGDALVersion <- function() {
  .Deprecated("reproducible::getGDALVersion")
  reproducible::getGDALVersion()
}
