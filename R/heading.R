################################################################################
#' Heading between spatial points.
#'
#' Determines the heading between spatial points.
#'
#' @param from The starting position; an object of class SpatialPoints.
#'
#' @param to The ending position;  an object of class SpatialPoints.
#'
#' @return The heading between the points, in degrees.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom CircStats deg
#' @importFrom sp SpatialPoints
#' @rdname heading
#'
#' @examples
#' library(sp)
#' N <- 10L                # number of agents
#' x1 <- stats::runif(N, -50, 50) # previous X location
#' y1 <- stats::runif(N, -50, 50) # previous Y location
#' x0 <- stats::rnorm(N, x1, 5)   # current X location
#' y0 <- stats::rnorm(N, y1, 5)   # current Y location
#'
#' # using SpatialPoints
#' prev <- SpatialPoints(cbind(x = x1, y = y1))
#' curr <- SpatialPoints(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
#' # using matrix
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' #using both
#' prev <- SpatialPoints(cbind(x = x1, y = y1))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- SpatialPoints(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
heading <- function(from, to) {
  from <- coords(from)
  to <- coords(to)
  ys <- to[, 2] - from[, 2]
  xs <- to[, 1] - from[, 1]
  heading <- deg(atan(xs / ys))
  ys <- (ys < 0)
  heading[(ys) & (xs) < 0] <- heading[(ys) & (xs) < 0] - 180
  heading[(ys) & (xs) > 0] <- heading[(ys) & (xs) > 0] + 180
  return(heading %% 360)
}


coords <- function(crds) {
  if (inherits(crds, "SpatVector")) {
    if (!requireNamespace("terra")) stop("Need terra package installed")
    crds <- terra::crds(crds)
  } else if (inherits(crds, "sf")) {
    if (!requireNamespace("sf")) stop("Need sf package installed")
    crds <- sf::st_coordinates(crds)
  } else if (inherits(crds, "Spatial")) {
    if (!requireNamespace("sp")) stop("Need sp package installed")
    crds <- sp::coordinates(crds)
  }

crds
}

#' @importFrom reproducible .requireNamespace
extnt <- function(obj) {
  if (inherits(obj, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    obj <- raster::extent(obj)
  } else if  (inherits(obj, "SpatRaster") || inherits(obj, "sf")) {
    if (inherits(obj, "SpatRaster")) .requireNamespace("terra", stopOnFALSE = TRUE)
    if (inherits(obj, "sf")) .requireNamespace("sf", stopOnFALSE = TRUE)
    obj <- terra::ext(obj)
  }
  obj
}

#' @importFrom reproducible .requireNamespace
`coords<-` <- function(obj, value) {
  if (inherits(obj, "SpatVector")) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    obj <- terra::vect(data.frame(as.data.frame(obj), value), geom = c("x", "y"))
  } else if (inherits(obj, "sf")) {
    .requireNamespace("sf", stopOnFALSE = TRUE)
    obj2 <- st_as_sfc(st_as_sf(as.data.frame(value), coords = c("x", "y")))
    obj <- st_set_geometry(obj, value = obj2)
    obj
  } else if (inherits(obj, "Spatial")) {
    .requireNamespace("sp", stopOnFALSE = TRUE)
    obj@coords <- value
  }

  obj
}


