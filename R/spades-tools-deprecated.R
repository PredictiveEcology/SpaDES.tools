#' `fastCrop` is a wrapper around `velox::VeloxRaster_crop`, though
#' `raster::crop` is faster under many tests.
#'
#' @param x Raster to crop
#'
#' @inheritParams raster::crop
#'
#' @export
#' @importFrom raster crop extent
#' @seealso `velox::VeloxRaster_crop`
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("raster::crop")
}
