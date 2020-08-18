#' Calculate distances and directions between many points and many grid cells
#'
#' This is a modification of \code{\link[raster]{distanceFromPoints}} for the case of many points.
#' This version can often be faster for a single point because it does not return a RasterLayer.
#' This is different than \code{\link[raster]{distanceFromPoints}} because it does not take the
#' minimum distance from the set of points to all cells.
#' Rather this returns the every pair-wise point distance.
#' As a result, this can be used for doing inverse distance weightings, seed rain,
#' cumulative effects of distance-based processes etc.
#' If memory limitation is an issue, maxDistance will keep memory use down,
#' but with the consequences that there will be a maximum distance returned.
#' This function has the potential to use a lot of memory if there are a lot of
#' \code{from} and \code{to} points.
#'
#' This function is cluster aware. If there is a cluster running, it will use it.
#' To start a cluster use \code{\link[raster:cluster]{beginCluster}}, with \code{N} being
#' the number of cores to use. See examples in \code{SpaDES.core::experiment}.
#'
#' @param from Numeric matrix with 2 or 3 or more columns. They must include x and y,
#'             representing x and y coordinates of "from" cell. If there is a column
#'             named "id", it will be "id" from \code{to}, i.e,. specific pair distances.
#'             All other columns will be included in the return value of the function.
#'
#' @param to Numeric matrix with 2  or 3 columns (or optionally more, all of which
#'           will be returned),
#'           x and y, representing x and y coordinates of "to" cells, and
#'           optional "id" which will be matched with "id" from \code{from}. Default is all cells.
#'
#' @param landscape RasterLayer. optional. This is only used if \code{to} is NULL, in which case
#'                  all cells are considered \code{to}.
#'
#' @param angles Logical. If \code{TRUE}, then the function will return angles in radians,
#'               as well as distances.
#'
#' @param maxDistance Numeric in units of number of cells. The algorithm will build
#'                    the whole surface (from \code{from} to \code{to}), but will
#'                    remove all distances that are above this distance.
#'                    Using this will keep memory use down.
#'
#' @param cumulativeFn A function that can be used to incrementally accumulate
#'                     values in each \code{to} location, as the function iterates
#'                     through each \code{from}. See Details.
#'
#' @param distFn A function. This can be a function of \code{landscape},
#'               \code{fromCells}, \code{toCells}, and \code{dist}.
#'               If \code{cumulativeFn} is supplied, this will be used to convert
#'               the distances to some other set of units that will be accumulated
#'               by the \code{cumulativeFn}. See Details and examples.
#'
#' @param ... Any additional objects needed for \code{distFn}.
#'
#' @inheritParams splitRaster
#'
#' @return A sorted matrix on \code{id} with same number of rows as \code{to},
#'         but with one extra column, \code{"dists"}, indicating the distance
#'         between \code{from} and \code{to}.
#'
#' @seealso \code{\link{rings}}, \code{\link{cir}}, \code{\link[raster]{distanceFromPoints}},
#' which can all be made to do the same thing, under specific combinations of arguments.
#' But each has different primary use cases. Each is also faster under different conditions.
#' For instance, if \code{maxDistance} is relatively small compared to the number of cells
#' in the \code{landscape}, then \code{\link{cir}} will likely be faster. If a minimum
#' distance from all cells in the \code{landscape} to any cell in \code{from}, then
#' \code{distanceFromPoints} will be fastest. This function scales best when there are
#' many \code{to} points or all cells are used \code{to = NULL} (which is default).
#'
#' @details
#'
#' If the user requires an id (indicating the \code{from} cell for each \code{to} cell)
#' to be returned with the function, the user must add an identifier to the
#' \code{from} matrix, such as \code{"id"}.
#' Otherwise, the function will only return the coordinates and distances.
#'
#' \code{distanceFromEachPoint} calls \code{.pointDistance}, which is not intended to be called
#' directly by the user.
#'
#' This function has the potential to return a very large object, as it is doing pairwise
#' distances (and optionally directions) between from and to. If there are memory
#' limitations because there are many
#' \code{from} and many \code{to} points, then \code{cumulativeFn} and \code{distFn} can be used.
#' These two functions together will be used iteratively through the \code{from} points. The
#' \code{distFn} should be a transformation of distances to be used by the
#' \code{cumulativeFn} function. For example, if \code{distFn} is \code{1 / (1+x)},
#' the default, and \code{cumulativeFn} is \code{`+`}, then it will do a sum of
#' inverse distance weights.
#' See examples.
#'
#' @export
#' @importFrom raster getCluster ncell returnCluster xyFromCell
#' @importFrom parallel clusterApply
#'
#' @example inst/examples/example_distanceFromEachPoint.R
#'
distanceFromEachPoint <- function(from, to = NULL, landscape, angles = NA_real_,
                                  maxDistance = NA_real_, cumulativeFn = NULL,
                                  distFn = function(dist) 1 / (1 + dist), cl, ...) {
  matched <- FALSE
  fromColNames <- colnames(from)
  otherFromCols <- is.na(match(fromColNames, c("x", "y", "id")))

  if ("id" %in% fromColNames) {
    ids <- unique(from[, "id"])
  }
  if ("id" %in% colnames(to)) {
    matched <- TRUE
  }
  if (is.null(to)) {
    to <- xyFromCell(landscape, 1:ncell(landscape))
  }
  if (!is.null(cumulativeFn)) {
    forms <- names(formals(distFn))
    fromC <- "fromCell" %fin% forms
    if (fromC) fromCell <- cellFromXY(landscape, from[, c("x", "y")])
    toC <- "toCell" %fin% forms
    if (toC) toCell <- cellFromXY(landscape, to[, c("x", "y")])
    land <- "landscape" %fin% forms
    distFnArgs <- if (land) list(landscape = landscape[]) else NULL
    if (length(list(...)) > 0) distFnArgs <- append(distFnArgs, list(...))
    xDist <- "dist" %fin% forms
  }

  if (!matched) {
    nrowFrom <- NROW(from)
    if (nrowFrom > 1) {
      if (is.null(cumulativeFn)) {
        if (any(otherFromCols) | isTRUE(angles)) {
          out <- lapply(seq_len(nrowFrom), function(k) {
            out <- .pointDistance(from = from[k, , drop = FALSE], to = to,
                                  angles = angles, maxDistance = maxDistance,
                                  otherFromCols = otherFromCols)
          })
          out <- do.call(rbind, out)
        } else {
          maxDistance2 <- if (is.na(maxDistance)) Inf else maxDistance
          out <- pointDistance3(fromX = from[, "x"], toX = to[, "x"],
                                fromY = from[, "y"], toY = to[, "y"],
                                maxDistance = maxDistance2)
        }
      } else {
        # if there is a cluster, then there are two levels of cumulative function,
        #  inside each cluster and outside, or "within and between clusters".
        #  This is the outer one.
        #  The inner one is the one defined by the user argument.
        outerCumFun <- function(x, from, fromCell, landscape, to, angles, maxDistance,
                                distFnArgs, fromC, toC, xDist, cumulativeFn, distFn) {

          cumVal <- rep_len(0, NROW(to))

          for (k in seq_len(nrowFrom)) {
            out <- .pointDistance(from = from[k, , drop = FALSE], to = to,
                                  angles = angles, maxDistance = maxDistance,
                                  otherFromCols = otherFromCols)
            indices <- cellFromXY(landscape, out[, c("x", "y")])
            if (k == 1) {
              if (fromC) distFnArgs <- append(distFnArgs, list(fromCell = fromCell[k]))
              if (toC) distFnArgs <- append(distFnArgs, list(toCell = toCell[indices]))
              if (xDist) distFnArgs <- append(distFnArgs, list(dist = out[, "dists", drop = FALSE]))
            } else {
              if (fromC) distFnArgs[["fromCell"]] <- fromCell[k]
              if (toC) distFnArgs[["toCell"]] <- toCell[indices]
              if (xDist) distFnArgs[["dist"]] <- out[, "dists"]
            }

            # call inner cumulative function
            if (length(indices) < ncell(landscape)) {
              cumVal <- do.call(
                cumulativeFn, args = list(cumVal, do.call(distFn, args = distFnArgs))
              )
            } else {
              cumVal[indices] <- do.call(
                cumulativeFn, args = list(
                  cumVal[indices], do.call(distFn, args = distFnArgs)
                )
              )
            }
          }
          return(cumVal)
        }

        if (missing(cl)) {
          cl <- tryCatch(getCluster(), error = function(e) NULL)
          on.exit(if (!is.null(cl)) returnCluster(), add = TRUE)
        }

        outerCumFunArgs <- list(landscape = landscape, to = to, angles = angles,
                                maxDistance = maxDistance, distFnArgs = distFnArgs,
                                fromC = fromC, toC = toC, xDist = xDist,
                                cumulativeFn = cumulativeFn, distFn = distFn)

        parFunFun <- function(x) {
          # this is a slightly tweaked version of outerCumFun, doing all calls
          do.call(outerCumFun, append(list(x = x, from = fromList[[x]],
                                           if (fromC) fromCell = fromCellList[[x]]), # nolint
                                      outerCumFunArgs))
        }

        if (!is.null(cl)) {
          parFun <- "clusterApply"
          seqLen <- seq_len(min(nrowFrom, length(cl)))
          inds <- rep(seq_along(cl), length.out = nrowFrom)
          fromList <- lapply(seqLen, function(ind) {
            from[inds == ind, , drop = FALSE]
          })

          if (fromC) fromCellList <- lapply(seqLen, function(ind) {
            fromCell[inds == ind]
          })
          parFunArgs <- list(cl = cl, x = seqLen, fun = parFunFun)
        } else {
          parFun <- "lapply"
          fromList <- list(from)
          if (fromC) fromCellList <- list(fromCell)
          parFunArgs <- list(X = 1, FUN = parFunFun)
        }

        # The actual call
        cumVal <- do.call(get(parFun), args = parFunArgs)

        # must cumulativeFn the separate cluster results
        while (length(cumVal) > 1) {
          cumVal[[2]] <- do.call(cumulativeFn, cumVal[1:2])
          cumVal[[1]] <- NULL
        }

        cumVal <- cumVal[[1]]

        if (is.null(to)) {
          out <- cbind(to, val = cumVal)
        } else {
          out <- cbind(to, val = cumVal[!is.na(cumVal)])
        }
      }
    } else {
      out <- .pointDistance(from = from, to = to, angles = angles,
                            maxDistance = maxDistance, otherFromCols = otherFromCols)
    }
  } else {
    out <- lapply(ids, function(k) {
      .pointDistance(from = from[from[, "id"] == k, , drop = FALSE],
                     to = to[to[, "id"] == k, , drop = FALSE],
                     angles = angles, maxDistance = maxDistance,
                     otherFromCols = otherFromCols)
    })
    out <- do.call(rbind, out)
  }
  return(out)
}

#' @param otherFromCols TODO: description needed
#'
#' @aliases pointDistance
#' @export
#' @name .pointDistance
#' @rdname distances
.pointDistance <- function(from, to, angles = NA, maxDistance = NA_real_,
                           otherFromCols = FALSE) {
  if (!is.na(maxDistance)) {
    to <- to[(abs(to[, "x"] - from[, "x"]) <= maxDistance)  &
               (abs(to[, "y"] - from[, "y"]) <= maxDistance), ]
  }

  dists <- pointDistance2(to, from)

  if (!is.na(maxDistance)) {
    dists <- dists[dists[, "dists"] <= maxDistance, ]
  }
  if (any(otherFromCols)) {
    colNums <- seq_len(ncol(dists))
    dists <- cbind(dists = dists, from[, otherFromCols])
    colnames(dists)[-colNums] <- colnames(from)[otherFromCols]
  }
  return(dists)
}

#' Calculate matched point directions
#'
#' Internal function
#'
#' @aliases matchedPointDirection
#' @keywords internal
#' @name .matchedPointDirection
#' @rdname matchedPointDirection
.matchedPointDirection <- function(to, from) {
  ids <- unique(from[, "id"])
  orig <- order(to[, "id", drop = FALSE], to[, "to", drop = FALSE])
  to <- to[orig, , drop = FALSE]
  angls <- lapply(ids, function(i) {
    m1 <- to[to[, "id"] == i, c("x", "y"), drop = FALSE]
    m2 <- from[from[, "id"] == i, c("x", "y"), drop = FALSE]
    .pointDirection(m2, m1)
  })
  do.call(rbind, angls)
}

#' Calculate distances and directions between many points and many grid cells
#'
#' This is a modification of \code{\link[raster]{distanceFromPoints}} for the case
#' of many points.
#' This version can often be faster for a single point because it does not return
#' a \code{RasterLayer}.
#' This is different than \code{\link[raster]{distanceFromPoints}} because it does
#' not take the minimum distance from the set of points to all cells.
#' Rather this returns the every pair-wise point distance.
#' As a result, this can be used for doing inverse distance weightings, seed rain,
#' cumulative effects of distance-based processes etc.
#' If memory limitation is an issue, \code{maxDistance} will keep memory use down,
#' but with the consequences that there will be a maximum distance returned.
#' This function has the potential to use a lot of memory if there are a lot of
#' \code{from} and \code{to} points.
#'
#' \code{directionFromEachPoint} calls \code{.pointDirection}, which is
#' not intended to be called directly by the user.
#'
#' If knowing the which from cell matches with which to cell is important,
#' put a column "id" (e.g., starting cell) in the \code{from} matrix.
#'
#' @param from matrix with 2 or 3 columns, x and y, representing x and y coordinates
#'             of \code{from} cell, and optional \code{id}, which will be returned,
#'             and if \code{id} column is in \code{to}, it will be matched with that.
#' @param to matrix with 2  or 3 columns (or optionally more, all of which will be returned),
#'           x and y, representing x and y coordinates of \code{to} cells, and
#'           optional \code{id} which will be matched with \code{id} from \code{from}.
#'           It makes no sense to have \code{id} column here with no \code{id} column
#'           in \code{from}.
#' @param landscape (optional) \code{RasterLayer}. This is only used if \code{to = NULL},
#'                  in which case all cells are considered \code{to}.
#'
#' @return A sorted matrix on \code{id} with same number of rows as \code{to},
#'         but with one extra column, \code{angles} indicating the angle in radians
#'         between from and to. For speed, this angle will be between \code{-pi/2}
#'         and \code{3*pi/2}.
#'         If the user wants this between say, \code{0} and \code{2*pi},
#'         then \code{angles \%\% (2*pi)} will do the trick. See example.
#'
#' @export
#' @rdname directions
#' @seealso \code{\link{distanceFromEachPoint}}, which will also return directions
#'          if \code{angles = TRUE}.
#'
#' @examples
#' library(raster)
#' library(quickPlot)
#'
#' N <- 2
#' dirRas <- raster(extent(0,40,0,40), res = 1)
#' coords <- cbind(x = round(runif(N, xmin(dirRas), xmax(dirRas))) + 0.5,
#'                 y = round(runif(N, xmin(dirRas), xmax(dirRas))) + 0.5,
#'                 id = 1:N)
#'
#' dirs1 <- directionFromEachPoint(from = coords, landscape = dirRas)
#' library(CircStats)
#' dirs1[, "angles"] <- deg(dirs1[,"angles"] %% (2*pi))
#' indices <- cellFromXY(dirRas,dirs1[, c("x", "y")])
#' minDir <- tapply(dirs1[, "angles"], indices, function(x) min(x)) # minimum angle
#' dirRas[] <- as.vector(minDir)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(dirRas)
#'   library(sp)
#'   start <- SpatialPoints(coords[, c("x", "y"), drop = FALSE])
#'   Plot(start, addTo = "dirRas")
#' }
directionFromEachPoint <- function(from, to = NULL, landscape) {
  matched <- FALSE
  nrowFrom <- NROW(from)
  if ("id" %in% colnames(from)) {
    ids <- unique(from[, "id"])
  } else if (nrowFrom > 1) {
    ids <- seq_len(nrowFrom)
  }

  if ("id" %in% colnames(to)) {
    matched <- TRUE
  }
  if (is.null(to))
    to <- xyFromCell(landscape, 1:ncell(landscape))
  if (!matched) {
    if (nrowFrom > 1) {
      out <- lapply(seq_len(nrowFrom), function(k) {
        out <- .pointDirection(from = from[k, , drop = FALSE], to = to)
        cbind(out, id = ids[k])
      })

      out <- do.call(rbind, out)
    } else {
      out <- .pointDirection(from = from, to = to)
    }
  } else {
    out <- lapply(ids, function(k) {
      .pointDirection(from = from[from[, "id"] == k, , drop = FALSE],
                      to = to[to[, "id"] == k, , drop = FALSE])
    })
    out <- do.call(rbind, out)
  }
}

#' Calculate the direction from a point to a set of points
#'
#' Internal function.
#'
#' @author Eliot McIntire
#' @keywords internal
#' @rdname directions
.pointDirection <- function(from, to) {
  rise <- to[, "y"] - from[, "y"]
  run <- to[, "x"] - from[, "x"]
  angls <- pi / 2 - atan2(rise, run) # Convert to geographic 0 = North
  cbind(to, angles = angls)
}
