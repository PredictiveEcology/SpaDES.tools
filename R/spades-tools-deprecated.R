#' `fastCrop` is deprecated.
#'
#' @param x Raster to crop
#' @param y Raster to crop with
#' @param ... other
#'
#' @return None. Called for its side effect of signalling the deprecation
#'         condition; use [terra::crop()] instead.
#'
#' @export
#' @seealso `velox::VeloxRaster_crop`
#'
#' @rdname deprecated
fastCrop <- function(x, y, ...) {
  .Deprecated("terra::crop")
}
