#' Faster operations on rasters
#'
#' This alternative to \code{mask} is included here, but it is
#' simply passing the arguments to raster::mask for now. We are
#' waiting for the fasterize pacakge to be submitted to CRAN.
#'
#' @param x        A \code{Raster*} object.
#'
#' @param polygon  A \code{SpatialPolygons} object.
#'
#' @return A \code{Raster*} object, masked (i.e., smaller extent and/or
#'         several pixels converted to NA)
#'
#' @author Eliot Mcintire
#' @export
#' @importFrom raster crop extract mask nlayers raster stack
#'
#' @examples
#'\dontrun{
#' library(raster)
#'
#' Sr1 <- Polygon(cbind(c(2, 4, 4, 0.9, 2), c(2, 3, 5, 4, 2)))
#' Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
#' Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
#'
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Srs2 <- Polygons(list(Sr2), "s2")
#' Srs3 <- Polygons(list(Sr3), "s3")
#' shp <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
#' d <- data.frame(vals = 1:3, other = letters[3:1], stringsAsFactors = FALSE)
#' row.names(d) <- names(shp)
#' shp <- SpatialPolygonsDataFrame(shp, data = d)
#' poly <- list()
#' poly[[1]] <- raster(raster::extent(shp), vals = 0, res = c(1, 1))
#' poly[[2]] <- raster(raster::extent(shp), vals = 1, res = c(1, 1))
#' origStack <- stack(poly)
#' # original mask function in raster
#' newStack1 <- mask(origStack, mask = shp)
#' newStack2 <- fastMask(x = origStack, polygon = shp)
#'
#' # test all equal
#' all.equal(newStack1, newStack2)
#'
#' newStack1 <- stack(newStack1)
#' newStack2 <- stack(newStack2)
#'
#' if (interactive()) {
#'   plot(newStack2[[1]])
#'   plot(shp, add = TRUE)
#' }
#'
#' file.remove("newMap.tif")
#' }
#'
fastMask <- function(x, polygon) {
  message("This function will eventually use the fasterize package")
  # if (!requireNamespace("fasterize", quietly = TRUE) |
  #     !requireNamespace("sf", quietly = TRUE)) {
    # message("Using raster::mask, which may be very slow, because 'fasterize' not installed. ",
    #         " To install please try devtools::install_github('ecohealthalliance/fasterize')")
  x <- mask(x, polygon)
  # } else {
  #   numericfield <- names(polygon)[which(unlist(lapply(names(polygon), function(x) {
  #     is.numeric(polygon[[x]])
  #   })))[1]]
  #   a <- fasterize::fasterize(sf::st_as_sf(polygon), raster = x[[1]], field = numericfield)
  #   m <- is.na(a[])
  #   x[m] <- NA
  # }
  if (nlayers(x) > 1) {
    stack(x)
  } else {
    x
  }
}

#' fastCrop
#'
#' This function is a wrapper around \code{velox::VeloxRaster_crop}.
#'
#' @param x Raster to crop
#'
#' @inheritParams raster::crop
#'
#' @export
#' @importFrom raster crop extent
#' @seealso \code{velox::VeloxRaster_crop}
#'
fastCrop <- function(x, y, ...) {
  if (!requireNamespace("velox")) {
    message("Using raster::crop, which may be very slow, because 'velox' not installed. ",
            " To install please try devtools::install.packages('velox')")
    a <- crop(x, y)
  } else {
    v1 <- velox::velox(x) # velox package is much faster than raster package for rasterize function,
    # but not as fast as gdal_rasterize for large polygons
    if (is(y, "Raster")) y <- extent(y)
    v1$crop(y)
    if (length(names(x)) > 1) {
      a <- v1$as.RasterStack()
    } else {
      a <- v1$as.RasterLayer(band = 1)
    }
  }
  a
}
