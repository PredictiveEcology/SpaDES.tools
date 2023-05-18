#' `fastCrop` is deprecated.
#'
#' @param x Raster to crop
#'
#' @export
#' @seealso `velox::VeloxRaster_crop`
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("terra::crop")
}
