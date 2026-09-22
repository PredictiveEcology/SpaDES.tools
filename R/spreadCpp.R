utils::globalVariables(c("id"))

#' Stochastic outward spread, implemented in C++
#'
#' A fast, deliberately narrow alternative to [spread()] for the common case: a
#' per-cell probability of being spread to, one or more starting cells, an
#' optional per-fire maximum size, and cell indices back.
#'
#' @section The rules it follows:
#'
#' * **Growth happens in generations.** The cells that caught in one generation
#'   are the only ones that try to spread in the next, so fires grow outwards
#'   and a fire's extent is tied to how many generations it has had.
#' * **One draw per (burning cell, unburned neighbour) pair**, compared against
#'   `spreadProb` **of the neighbour**, not of the cell doing the spreading. A
#'   cell with `k` burning neighbours therefore catches with probability
#'   `1 - (1 - p)^k` in that generation, exactly as in [spread()].
#' * **A cell belongs to exactly one fire, and is never spread to twice.** Once
#'   a cell has caught it is never reconsidered. Burning cells are visited in a
#'   random order within a generation, so when two fires reach the same cell
#'   neither is systematically favoured.
#' * **A fire stops at its own `maxSize` and never exceeds it.** [spread()] adds
#'   a whole generation and then randomly drops the excess; `spreadCpp()` stops
#'   on the boundary, which lands on `maxSize` exactly.
#' * **`NA` in `spreadProb` means unburnable**, as in [spread()].
#'
#' @section Limitations:
#'
#' `spreadCpp()` is deliberately narrow. It covers one case -- a per-cell
#' probability of being spread to, one or more ignitions, an optional per-fire
#' size cap -- and takes only the arguments listed above. Every other argument
#' [spread()] accepts is **unsupported, and passing one is an error rather than
#' being silently ignored**:
#'
#' \describe{
#'   \item{`allowOverlap`}{Not supported. Fires never overlap; a cell belongs to
#'     one fire.}
#'   \item{`returnDistances`, `circle`, `circleMaxRadius`}{Not supported. No
#'     distances are computed and spread is not constrained to a disc.}
#'   \item{`asymmetry`, `asymmetryAngle`}{Not supported. Spread is isotropic;
#'     there is no directional bias.}
#'   \item{`neighProbs`, `relativeSpreadProb`}{Not supported. Every eligible
#'     neighbour is drawn for independently; the number of neighbours a cell
#'     spreads to is not itself drawn from a distribution, and probabilities are
#'     absolute rather than rescaled within a cell's neighbourhood.}
#'   \item{`stopRule`, `stopRuleBehavior`}{Not supported. The only stopping
#'     conditions are `maxSize`, `iterations`, and having nowhere left to go.}
#'   \item{`exactSizes`}{Not supported, though `maxSize` is exact in the sense
#'     that a fire which reaches its cap stops there.}
#'   \item{`persistence`}{Not supported. A burned cell stays burned and does not
#'     re-burn.}
#'   \item{`mask`}{Not supported. Use `NA` (or 0) in `spreadProb` to make cells
#'     unburnable.}
#'   \item{`spreadState`}{Not supported. A spread cannot be continued from an
#'     earlier one; each call starts from `loci`.}
#'   \item{`spreadProbLater`}{Not supported. One `spreadProb` applies to every
#'     generation.}
#'   \item{`torus`}{Not supported. The landscape does not wrap; spread stops at
#'     the edges.}
#'   \item{`plot.it`, `id`, `returnIndices`}{Not supported as options. Nothing
#'     is plotted, and the return value is always the indices form.}
#'   \item{Raster `spreadProb`}{Not supported. Pass a numeric vector of length
#'     `terra::ncell(landscape)`, e.g. `terra::values(x)`.}
#' }
#'
#' Use [spread()] when any of these are needed.
#'
#' @section Reproducibility:
#'
#' `spreadCpp()` is **not** a drop-in reimplementation of [spread()]. It makes
#' its own random draws, so for a given seed it does not reproduce [spread()]'s
#' cells, and swapping one for the other changes results and invalidates cached
#' output. It is reproducible in the ordinary sense: the same seed and the same
#' inputs give the same answer.
#'
#' What the two do agree on is behaviour in aggregate. Over 300 seeds on a
#' 1.44M-cell landscape the two produce the same total burned area (Wilcoxon
#' `p = 0.34`) and the same fire-size distribution (Kolmogorov-Smirnov
#' `p = 0.56`), with quantiles within 7% of each other from the 10th percentile
#' to the 99th.
#'
#' @section Performance:
#'
#' Roughly 2-3x faster than [spread()] on the same problem: 2.0x median over
#' nine scenarios spanning 596 to 188,762 burned cells, and 1.9-3.1x on
#' landscapes from 176k to 1.44M cells with 80 fires.
#'
#' Cost grows with the size of the `landscape`, not just with how much burns,
#' because the per-cell state is allocated once per call and because scattered
#' fires on a wide landscape have poorer memory locality. Holding the fires
#' fixed, going from 1.44M to 9M cells costs an extra 0.035 s per call. If the
#' fires occupy a small part of a large landscape, crop first and map the
#' indices back; it is worth about 30% going from 1.44M to 300k cells.
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
