if (getRversion() >= "3.1.0") {
#  utils::globalVariables(c("indices", "id", "initialLocus", "dists", "dup", ".I"))
}

#' Identifies all cells within a ring around the focal cells
#'
#' Identifies the cell numbers of all cells within a ring defined by  minimum
#' and maximum distances from focal cells.
#' Uses \code{\link{spread}} under the hood, with specific values set.
#' Under many situations, this will be faster than using \code{rgeos::gBuffer}
#' twice (once for smaller ring and once for larger ring, then removing the
#' smaller ring cells).
#'
#' @export
#' @docType methods
#' @return This will return  a \code{data.table} with columns as described in
#'         \code{spread} when \code{returnIndices = TRUE}.
#'
#' @author Eliot McIntire
#' @inheritParams spread
#'
#' @param minRadius Numeric. Minimum radius to be included in the ring. Note:
#'             this is inclusive, i.e., >=
#' @param maxRadius Numeric. Maximum radius to be included in the ring. Note:
#'             this is inclusive, i.e., <=
#' @param ... Any other argument passed to \code{spread}
#'
#' @name rings
#' @aliases rings
#' @rdname rings
#' @seealso \code{\link{cir}} which uses a different algorithm.
#' \code{cir} tends to be faster when there are few starting points, \code{rings}
#' tends to be faster when there are many starting points. Another difference
#' between the two functions is that \code{rings} takes the centre of the pixel
#' as the centre of a circle, whereas \code{cir} takes the exact coordinates.
#' See example.
#'
#' @seealso \code{rgeos::gBuffer}
#'
#' @example inst/examples/example_rings.R
#'
setGeneric(
  "rings",
  function(landscape, loci = NA_real_, id = FALSE, minRadius = 2, maxRadius = 5,
           allowOverlap = FALSE, returnIndices = FALSE, returnDistances = TRUE, ...) {
    standardGeneric("rings")
})

#' @importFrom fpCompare %<=% %>=%
#' @rdname rings
setMethod(
  "rings",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, id, minRadius, maxRadius, allowOverlap,
                        returnIndices, returnDistances, ...) {
    spreadEvents <- spread(landscape, loci = loci, circle = TRUE,
                           circleMaxRadius = maxRadius, spreadProb = 1, id = TRUE,
                           returnDistances = TRUE, returnIndices = TRUE,
                           allowOverlap = allowOverlap, ...)
    if (length(minRadius) > 1 | length(maxRadius) > 1) {
      len <- length(loci)
      if (!(length(minRadius) == len | length(maxRadius) == len)) {
        warning("minRadius and maxRadius should be length 1 or same length as loci. ",
                "Recycling values which may not produce desired effects.")
      }
      minRadius <- rep(minRadius, length.out = len)
      maxRadius <- rep(maxRadius, length.out = len)
      out <- rbindlist(lapply(seq_along(loci), function(j) {
        spreadEvents[id == j & (dists %>=% minRadius[j] & dists %<=% maxRadius[j])]
      }))
    } else {
      out <- spreadEvents[(dists %>=% minRadius)]
    }

    if (!returnIndices) {
      outRas <- numeric(ncell(landscape))
      if (returnDistances)
        outRas[] <- NA_real_
      else
        outRas[] <- 0

      if (allowOverlap) {
        if (returnDistances) {
          out2 <- out[, list(mDists = mean(dists)), by = indices]
          outRas[out2$indices] <- out2$mDists
        } else {
          out2 <- out[, list(sumID = sum(id)), by = indices]
          outRas[out2$indices] <- out2$sumID
        }
      } else {
        if (returnDistances)
          outRas[out$indices] <- out$dists
        else
          outRas[out$indices] <- out$dists
      }
      outRas <- raster(extent(landscape), res = res(landscape), vals = outRas)
      return(outRas)
    }
    #if(!allowOverlap) {
    #   setkey(out, indices)
    #   out[,dup:=duplicated(indices),by=indices]
    # }
    return(out)
})
