# NA-aware comparison of two vectors
# Copied from http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/.
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

#' @importFrom reproducible .requireNamespace
extnt <- function(x, ...) {
  if (inherits(x, "Extent") || inherits(x, "SpatExtent")) {
    return(x)
  } else if (inherits(x, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    return(raster::extent(x))
  } else if (inherits(x, "SpatRaster") || inherits(x, "sf")) {
    if (inherits(x, "SpatRaster")) .requireNamespace("terra", stopOnFALSE = TRUE)
    if (inherits(x, "sf")) .requireNamespace("sf", stopOnFALSE = TRUE)
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

#' @importFrom reproducible .requireNamespace
`extnt<-` <- function(x, value) {
  if (inherits(x, "Raster")) {
    .requireNamespace("raster", stopOnFALSE = TRUE)
    if (inherits(value, "Extent")) {
      raster::extent(x) <- value
    } else if (inherits(value, "SpatExtent")) {
      .requireNamespace("terra", stopOnFALSE = TRUE)
      x <- terra::rast(x)
      terra::ext(x) <- value
      x <- raster::raster(x)
    }
  } else if (inherits(x, "SpatRaster")) {
    .requireNamespace("terra", stopOnFALSE = TRUE)
    if (inherits(value, "Extent")) {
      .requireNamespace("raster", stopOnFALSE = TRUE)
      terra::ext(x) <- terra::ext(value)
    } else if (inherits(value, "SpatExtent")) {
      terra::ext(x) <- value
    }
  }

  return(x)
}

