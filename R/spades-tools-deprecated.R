#' \code{fastCrop} is a wrapper around \code{velox::VeloxRaster_crop}, though
#' \code{raster::crop} is faster under many tests.
#'
#' @param x Raster to crop
#'
#' @inheritParams raster::crop
#'
#' @export
#' @importFrom raster crop extent
#' @seealso \code{velox::VeloxRaster_crop}
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("raster::crop")
}
