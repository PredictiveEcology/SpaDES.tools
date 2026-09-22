utils::globalVariables(c("id"))

#' Stochastic outward spread, implemented in C++
#'
#' A fast, deliberately narrow alternative to [spread()] for the common case: a
#' per-cell probability of being spread to, one or more starting cells, an
#' optional per-fire maximum size, and cell indices back.
#'
#' @section How it differs from `spread()`:
#'
#' `spreadCpp()` is **not** a drop-in reimplementation. It follows the same
#' rules but makes its own random draws, so for a given seed it does not
#' reproduce [spread()]'s cells. Use it where the spread is a stochastic model
#' component, not where you need to reproduce an earlier `spread()` result.
#'
#' The rules it does follow:
#'
#' * Growth happens in generations. The cells that caught in one generation are
#'   the only ones that try to spread in the next, so fires grow outwards.
#' * Every (burning cell, unburned neighbour) pair gets one draw, compared
#'   against `spreadProb` **of the neighbour**. A cell with `k` burning
#'   neighbours has probability `1 - (1 - p)^k` of catching in that generation,
#'   as in [spread()].
#' * A cell belongs to exactly one fire. Burning cells are visited in a random
#'   order within a generation, so when two fires reach the same cell neither is
#'   systematically favoured.
#' * A fire stops at its own `maxSize` and never exceeds it. [spread()] adds a
#'   whole generation and then randomly drops the excess; this stops on the
#'   boundary, which lands on `maxSize` exactly.
#' * `NA` in `spreadProb` means unburnable, as in [spread()].
#'
#' Everything else `spread()` offers -- `allowOverlap`, `returnDistances`,
#' `circle`, `asymmetry`, `neighProbs`, `relativeSpreadProb`, `stopRule`,
#' `persistence`, `mask`, continuing from a `spreadState`, torus wrapping -- is
#' out of scope and not accepted here.
#'
#' @param landscape A `SpatRaster`; only its geometry (number of columns and
#'   cells) is used.
#' @param loci Integer vector of starting cell indices, one per fire.
#' @param spreadProb Numeric of length 1 or `terra::ncell(landscape)`: the
#'   probability that a cell is spread to.
#' @param maxSize Numeric of length 1 or `length(loci)`: the largest number of
#'   cells each fire may reach. `Inf` (the default) means no limit.
#' @param directions 4 or 8. Default 8.
#' @param iterations Maximum number of generations. Default `Inf`.
#'
#' @return A `data.table` keyed on `id`, with integer columns `id`,
#'   `initialLocus` and `indices`, and a logical `active` (always `FALSE`, since
#'   the spread has finished) -- the same shape [spread()] returns for
#'   `returnIndices = TRUE`.
#'
#' @seealso [spread()], which is slower but far more general.
#' @export
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1, vals = 0)
#'   set.seed(1)
#'   out <- spreadCpp(r, loci = c(2500L, 7500L), spreadProb = 0.23,
#'                    maxSize = c(50, 100))
#'   out[, .N, by = "id"]
#' }
spreadCpp <- function(landscape, loci, spreadProb, maxSize = Inf,
                      directions = 8L, iterations = Inf) {
  numCell <- as.integer(terra::ncell(landscape))
  numCol <- as.integer(terra::ncol(landscape))

  loci <- as.integer(loci)
  if (anyNA(loci)) stop("`loci` must not contain NA.")
  spreadProb <- as.numeric(spreadProb)
  if (!(length(spreadProb) == 1L || length(spreadProb) == numCell)) {
    stop("`spreadProb` must be length 1 or terra::ncell(landscape).")
  }
  maxSize <- as.numeric(maxSize)
  if (!(length(maxSize) == 1L || length(maxSize) == length(loci))) {
    stop("`maxSize` must be length 1 or length(loci).")
  }

  out <- spreadCppEngine(numCol = numCol, numCell = numCell,
                         directions = as.integer(directions),
                         loci = loci, spreadProb = spreadProb,
                         maxSize = maxSize, iterations = as.numeric(iterations))

  dt <- data.table::data.table(id = out$id, initialLocus = out$initialLocus,
                               indices = out$indices, active = FALSE)
  data.table::setkeyv(dt, "id")
  dt[]
}
