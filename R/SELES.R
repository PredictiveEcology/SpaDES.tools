################################################################################
#' `SELES` - Transitioning to next time step
#'
#' @description
#' Describes the probability of an agent successfully persisting until next
#' time step. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param p realized probability of persisting (i.e., either 0 or 1).
#'
#' @param agent `SpatialPoints*` object.
#'
#' @return Returns new `SpatialPoints*` object with potentially fewer agents.
#'
#' @export
#' @importFrom sp 'coordinates<-'
#' @include heading.R
#' @rdname SELEStransitions
#'
#' @author Eliot McIntire
transitions <- function(p, agent) {
    coordinates(agent)[which(p == 0), ] <- NA
    return(agent)
}

##############################################################
#' SELES - Number of Agents to initiate
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param N         Number of agents to initiate (integer scalar).
#' @param probInit  Probability of initializing an agent at the location.
#'
#' @return A numeric, indicating number of agents to start
#'
#' @author Eliot McIntire
#' @export
#' @include heading.R
#' @rdname SELESnumAgents
#'
numAgents <- function(N, probInit) {
  stopifnot(length(N) == 1, is.numeric(N))
  return(N)
}

##############################################################
#' `SELES` - Initiate agents
#'
#' @description
#' Sets the the number of agents to initiate. THIS IS NOT FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map RasterLayer with extent and resolution of desired return object
#'
#' @param numAgents numeric resulting from a call to [numAgents()]
#'
#' @param probInit a Raster resulting from a [probInit()] call
#'
#' @param asSpatialPoints logical. Should returned object be `RasterLayer`
#'                        or `SpatialPointsDataFrame` (default)
#'
#' @param indices numeric. Indices of where agents should start
#'
#' @return A SpatialPointsDataFrame, with each row representing an individual agent
#'
#' @author Eliot McIntire
#' @export
#' @include heading.R
#' @importFrom raster getValues ncell raster xyFromCell
#' @importFrom stats runif
#' @rdname initiateAgents
#'
#' @example inst/examples/example_initiateAgents.R
#'
setGeneric(
  "initiateAgents",
  function(map, numAgents, probInit, asSpatialPoints = TRUE, indices) {
    standardGeneric("initiateAgents")
})

#' @export
#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "missing", "missing", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints) {
    initiateAgents(map, indices = 1:ncell(map), asSpatialPoints = asSpatialPoints)
})

#' @export
#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "missing", "Raster", "ANY", "missing"),
  function(map, probInit, asSpatialPoints) {
    wh <- which(runif(ncell(probInit)) < getValues(probInit))
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @export
#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "numeric", "missing", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints, indices) {
    wh <- sample(1:ncell(map), size = numAgents, replace = asSpatialPoints)
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @export
#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "numeric", "Raster", "ANY", "missing"),
  function(map, numAgents, probInit, asSpatialPoints) {
    vals <- getValues(probInit)
    wh <- sample(1:ncell(probInit), numAgents, replace = asSpatialPoints,
                 prob = vals / sum(vals))
    initiateAgents(map, indices = wh, asSpatialPoints = asSpatialPoints)
})

#' @export
#' @rdname initiateAgents
setMethod(
  "initiateAgents",
  signature = c("Raster", "missing", "missing", "ANY", "numeric"),
  function(map, numAgents, probInit, asSpatialPoints, indices) {
    if (asSpatialPoints) {
      if (length(indices > 0)) {
        xys <- xyFromCell(map, indices, spatial = asSpatialPoints)
        xys@coords <- xys@coords + cbind(runif(length(indices), -res(map)[1] / 2, res(map)[1] / 2),
                                         runif(length(indices), -res(map)[2] / 2, res(map)[2] / 2))
        xys@bbox <- cbind(apply(coordinates(xys), 2, min), apply(coordinates(xys), 2, max))
        return(xys)
      } else {
        stop("uh oh! no indices specified.") # TODO: need error msg
      }
    } else {
      tmp <- raster(extent(map), res = res(map), vals = 0)
      tmp[indices] <- 1
      return(tmp)
    }
})

################################################################################
#' `SELES` - Agent Location at initiation
#'
#' @description
#' Sets the the location of the initiating agents. NOT YET FULLY IMPLEMENTED.
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A `SpatialPoints*`, `SpatialPolygons*`, or `Raster*` object.
#'
#' @return Object of same class as provided as input.
#'          If a `Raster*`, then zeros are converted to `NA`.
#'
#' @author Eliot McIntire
#' @include heading.R
#' @export
#' @rdname SELESagentLocation
agentLocation <- function(map) {
  if (length(grep(pattern = "Raster", class(map))) == 1) {
    map[map == 0] <- NA
  } else if (length(grep(pattern = "SpatialPoints", class(map))) == 1) {
    map
  } else if (!is.na(pmatch("SpatialPolygons", class(map)))) {
    map
  } else {
    stop("only raster, Spatialpoints or SpatialPolygons implemented")
  }
  return(map)
}

##############################################################
#' `SELES` - Probability of Initiation
#'
#' @description
#' Describes the probability of initiation of agents or events.
#' *THIS IS NOT FULLY IMPLEMENTED.*
#'
#' A `SELES`-like function to maintain conceptual backwards compatibility
#' with that simulation tool. This is intended to ease transitions from
#' [SELES](http://www.gowlland.ca/).
#'
#' You must know how to use SELES for these to be useful.
#'
#' @param map A `spatialObjects` object. Currently, only provides CRS and, if p is not
#' a raster, then all the raster dimensions.
#'
#' @param p probability, provided as a numeric or raster
#'
#' @param absolute logical. Is `p` absolute probabilities or relative?
#'
#' @return A `RasterLayer` with probabilities of initialization.
#'        There are several combinations of inputs possible and they each result
#'        in different behaviours.
#'
#' If `p` is numeric or `Raster` and between 0 and 1, it is treated as an
#' absolute probability, and a map will be produced with the p value(s) everywhere.
#'
#' If `p` is numeric or `Raster` and not between 0 and 1, it is treated as a
#' relative probability, and a map will be produced with `p/max(p)` value(s) everywhere.
#'
#' If `absolute` is provided, it will override the previous statements, unless
#' `absolute = TRUE` and p is not between 0 and 1 (i.e., is not a probability).
#'
#' @author Eliot McIntire
#' @export
#' @importFrom raster cellStats crs extent setValues raster
#' @include heading.R
#' @rdname SELESprobInit
#'
probInit <- function(map, p = NULL, absolute = NULL) {
  if (all(inRange(p, 0, 1))) {
    if (is.null(absolute)) {
      absolute <- TRUE
    }
  } else {
    absolute <- FALSE
  }
  if (is.numeric(p)) {
    probInit <- raster(extent(map), nrows = nrow(map), ncols = ncol(map), crs = crs(map))
    p <- rep(p, length.out = ncell(map))
    probInit <- setValues(probInit, p / (sum(p) * (1 - absolute) + 1 * (absolute)))
  } else if (is(p, "RasterLayer")) {
    probInit <- p / (cellStats(p, sum) * (1 - absolute) + 1 * (absolute))
  } else if (is(map, "SpatialPolygonsDataFrame")) {
    probInit <- p / sum(p)
  } else {
    stop("Error initializing probability map: bad inputs")
  }
  return(probInit)
}

#' Patch size
#'
#' @param patches Number of patches.
#'
#' @importFrom raster freq
#'
patchSize <- function(patches) {
  return(freq(patches))
}
