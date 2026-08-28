#' @keywords internal
.requireNamespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Need to install.packages('", pkg, "')"))
  }
}

# NA-aware comparison of two vectors
# Copied from http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/.
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

extnt <- function(x, ...) {
  if (inherits(x, "Extent") || inherits(x, "SpatExtent")) {
    return(x)
  } else if (inherits(x, "Raster")) {
    .requireNamespace("raster")
    return(raster::extent(x))
  } else if (inherits(x, "SpatRaster") || inherits(x, "sf")) {
    if (inherits(x, "SpatRaster")) .requireNamespace("terra")
    if (inherits(x, "sf")) .requireNamespace("sf")
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

`extnt<-` <- function(x, value) {
  if (inherits(x, "Raster")) {
    .requireNamespace("raster")
    if (inherits(value, "Extent")) {
      raster::extent(x) <- value
    } else if (inherits(value, "SpatExtent")) {
      .requireNamespace("terra")
      x <- terra::rast(x)
      terra::ext(x) <- value
      x <- raster::raster(x)
    }
  } else if (inherits(x, "SpatRaster")) {
    .requireNamespace("terra")
    if (inherits(value, "Extent")) {
      .requireNamespace("raster")
      terra::ext(x) <- terra::ext(value)
    } else if (inherits(value, "SpatExtent")) {
      terra::ext(x) <- value
    }
  }

  return(x)
}


deg2 <- function(radian) (radian * 180)/pi
rad2 <- function (degree) (degree * pi)/180

## Should spread() take its dqrng fast path?
##
## This exists so tests can turn the fast path off. dqrng's xoroshiro state is
## separate from R's, and spread()'s mid-function reseeding does not fully
## neutralize it, so the seeded-determinism and baseline-equivalence tests need
## the base R sample.int branch. testthat::local_mocked_bindings() can rebind a
## function in this package's namespace but not base::requireNamespace, which
## those tests otherwise had to patch by hand with unlockBinding().
.useDqrng <- function() {
  requireNamespace("dqrng", quietly = TRUE)
}
