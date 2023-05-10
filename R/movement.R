################################################################################
#' Move
#'
#' Wrapper for selecting different animal movement methods.
#'
#' @param hypothesis  Character vector, length one, indicating which movement
#'                    hypothesis/method to test/use. Currently defaults to
#'                    'crw' (correlated random walk) using `crw`.
#'
#' @param ... arguments passed to the function in `hypothesis`
#'
#' @author Eliot McIntire
#' @export
#' @rdname crw
#'
move <- function(hypothesis = "crw", ...) {
     if (hypothesis == "crw") move <- "crw"
     if (is.null(hypothesis)) stop("Must specify a movement hypothesis")
     get(move)(...)
 }

################################################################################
#' Simple Correlated Random Walk
#'
#' This version uses just turn angles and step lengths to define the correlated random walk.
#'
#' This simple version of a correlated random walk is largely the version that
#' was presented in Turchin 1998, but it was also used with bias modifications
#' in McIntire, Schultz, Crone 2007.
#'
#' @param agent       A `SpatialPoints*` object.
#'                    If a `SpatialPointsDataFrame`, 2 of the columns must
#'                    be `x1` and `y1`, indicating the previous location.
#'                    If a `SpatialPoints` object, then `x1` and
#'                    `y1` will be assigned randomly.
#'
#' @param stepLength  Numeric vector of length 1 or number of agents describing
#'                    step length.
#'
#' @param extent      An optional `Extent` object that will be used for `torus`.
#'
#' @param torus       Logical. Should the movement be wrapped to the opposite
#'                    side of the map, as determined by the `extent` argument.
#'                    Default `FALSE`.
#'
#' @param stddev      Numeric vector of length 1 or number of agents describing
#'                    standard deviation of wrapped normal turn angles.
#'
#' @param lonlat      Logical. If `TRUE`, coordinates should be in degrees.
#'                    If `FALSE` coordinates represent planar ('Euclidean')
#'                    space (e.g. units of meters)
#'
#' @return A SpatialPointsDataFrame object with updated spatial position defined
#'         by a single occurrence of step length(s) and turn angle(s).
#'
#' @seealso [pointDistance()]
#'
#' @references Turchin, P. 1998. Quantitative analysis of movement: measuring and
#'             modeling population redistribution in animals and plants.
#'             Sinauer Associates, Sunderland, MA.
#'
#' @references McIntire, E. J. B., C. B. Schultz, and E. E. Crone. 2007.
#'             Designing a network for butterfly habitat restoration: where
#'             individuals, populations and landscapes interact.
#'             Journal of Applied Ecology 44:725-736.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom CircStats rad
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom stats rnorm
#' @rdname crw
crw <- function(agent, extent, stepLength, stddev, lonlat, torus = FALSE) {
  if (!any(vapply(c("SpatialPoints", "SpatVector"), inherits, x = agent, FUN.VALUE = logical(1)))) {
    if (is(agent, "SpatVector"))
      if (!identical("points", geomtype(agent)))
        stop("crs can only take SpatialPoints* or SpatVector points geometry")
  }

  if (inherits(agent, "SpatialPoints") || (inherits(agent, "SpatVect"))) {
    n <- length(agent)
    agent <- SpatialPointsDataFrame(agent, data = data.frame(
      x1 = runif(n, -180, 180), y1 = runif(n, -180, 180)
    ))
    names(agent) <- c("x1", "y1")
    agent <- crw(agent, extent = extent, stepLength = stepLength,
                 stddev = stddev, lonlat = lonlat, torus = torus)

  }
  # signature(agent = "SpatialPointsDataFrame"),
  # definition = function(agent, extent, stepLength, stddev, lonlat, torus = FALSE) {
  if (is.null(lonlat) || !is.logical(lonlat)) {
    stop("you must provide a 'lonlat' argument (TRUE/FALSE)")
  }
  hasNames <- names(agent) %in% c("x1", "y1")
  n <- NROW(agent)

  if (sum(hasNames) < 2) {
    stop("SpatialPointsDataFrame/SpatVector needs x1 and y1 columns with previous location")
  }

  agentHeading <- heading(cbind(x = agent$x1, y = agent$y1), agent)
  rndDir <- rnorm(n, agentHeading, stddev)
  rndDir[rndDir > 180] <- rndDir[rndDir > 180] - 360
  rndDir[rndDir <= 180 & rndDir < (-180)] <- 360 + rndDir[rndDir <= 180 & rndDir < (-180)]

  crds <- coords(agent)
  # move current coordinates to previous coordinates
  agent[, c("x1", "y1")] <- crds
  # update current coordinates to be those after the move
  newCoords <- cbind(
    x = crds[, 1] + sin(rad(rndDir)) * stepLength,
    y = crds[, 2] + cos(rad(rndDir)) * stepLength
  )
  # for Spatial -- this was used: agent@coords <-
  coords(agent) <- newCoords
  # agent$geometry <- newCoords

  if (torus) {
    return(wrap(X = agent, bounds = extent, withHeading = TRUE))
  } else {
    return(agent)
  }
}
