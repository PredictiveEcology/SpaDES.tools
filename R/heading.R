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
  if (!requireNamespace("CircStats")) stop("Need to install.packages('CirsStats')")
  from <- coords(from)
  to <- coords(to)
  ys <- to[, 2] - from[, 2]
  xs <- to[, 1] - from[, 1]
  heading <- CircStats::deg(atan(xs / ys)) ## 0/0 produces NaN; correct this below
  heading[xs == 0 & ys == 0] <- 0
  ys <- (ys < 0)
  heading[(ys) & (xs) < 0] <- heading[(ys) & (xs) < 0] - 180
  heading[(ys) & (xs) > 0] <- heading[(ys) & (xs) > 0] + 180
  return(heading %% 360)
}

#' @importFrom reproducible .requireNamespace
coords <- function(crds) {
  if (inherits(crds, "SpatVector")) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    crds <- terra::crds(crds)
  } else if (inherits(crds, "sf")) {
    .requireNamespace("sf", stopOnFALSE = TRUE)
    crds <- sf::st_coordinates(crds)
  } else if (inherits(crds, "Spatial")) {
    .requireNamespace("sp", stopOnFALSE = TRUE)
    crds <- sp::coordinates(crds)
  }

 crds
}

#' @importFrom reproducible .requireNamespace
`coords<-` <- function(obj, value) {
  if (inherits(obj, "SpatVector")) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    crdsdf <- data.frame(value, as.data.frame(coords(obj)))
    colnames(crdsdf) <- c("x", "y", "x1", "y1")
    obj <- terra::vect(crdsdf, geom = c("x", "y"))
  } else if (inherits(obj, "sf")) {
    .requireNamespace("sf", stopOnFALSE = TRUE)
    obj2 <- sf::st_as_sfc(sf::st_as_sf(as.data.frame(value), coords = c("x", "y")))
    obj <- sf::st_set_geometry(obj, value = obj2)
    obj
  } else if (inherits(obj, "Spatial")) {
    .requireNamespace("sp", stopOnFALSE = TRUE)
    obj@coords <- value
  }

  obj
}
