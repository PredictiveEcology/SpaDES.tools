## Snapshot generator for spread() / spread2() drop-in equivalence tests.
##
## Run this from a worktree at the pre-Rcpp baseline (commit 21b5e0c^,
## i.e. 1add983) to produce the .rds golden files in this directory. The
## test file tests/testthat/test-spread-snapshots.R then asserts that the
## current (post-Rcpp) code reproduces these byte-for-byte.
##
## Usage (from a baseline worktree):
##   R CMD INSTALL --no-docs --no-multiarch .
##   Rscript tests/testthat/_spread_snapshots/_generate.R <output_dir>
##
## All scenarios are seeded; the dqrng path is bypassed via a
## requireNamespace shim because dqrng's RNG state carries non-
## determinism across spread()'s internal reseeding (already documented
## in ?spread).

local({
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1) args[1] else
    "tests/testthat/_spread_snapshots"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  suppressPackageStartupMessages({
    library(SpaDES.tools)
    library(terra)
    library(withr)
  })

  ## Force base R sample.int branch in spread() — same shim used by the
  ## existing seeded-grid test in test-spread.R.
  if (requireNamespace("dqrng", quietly = TRUE)) {
    rN_orig <- base::requireNamespace
    rN_mock <- function(package, ...) {
      if (identical(package, "dqrng")) return(FALSE)
      rN_orig(package, ...)
    }
    base_env <- asNamespace("base")
    unlockBinding("requireNamespace", base_env)
    assign("requireNamespace", rN_mock, envir = base_env)
    lockBinding("requireNamespace", base_env)
  }

  ras <- terra::rast(terra::ext(0, 80, 0, 80), resolution = 1, vals = 1)
  with_seed(7L,   spsRas <- terra::rast(ras, vals = stats::runif(terra::ncell(ras), 0.10, 0.40)))
  with_seed(11L,  spRel  <- terra::rast(ras, vals = stats::runif(terra::ncell(ras))))
  spRelAbs <- spRel * 100 + 1
  with_seed(101L, starts <- sort(sample.int(terra::ncell(ras), 5L)))

  ## SpatRaster doesn't survive readRDS as identical() — its C++ pointer
  ## changes — so always extract values for the comparison.
  cmpForm <- function(x) {
    if (inherits(x, "SpatRaster")) terra::values(x)
    else x
  }

  spread_scenarios <- list(
    list(name = "rare_branch",          args = list(loci = starts, spreadProb = 0.225)),
    list(name = "id_TRUE",               args = list(loci = starts, spreadProb = 0.225, id = TRUE)),
    list(name = "returnIndices_1",       args = list(loci = starts, spreadProb = 0.225, returnIndices = 1L)),
    list(name = "returnIndices_2",       args = list(loci = starts, spreadProb = 0.225, returnIndices = 2L)),
    list(name = "directions_4",          args = list(loci = starts, spreadProb = 0.30, directions = 4L, returnIndices = 1L)),
    list(name = "directions_8",          args = list(loci = starts, spreadProb = 0.30, directions = 8L, returnIndices = 1L)),
    list(name = "maxSize_scalar",        args = list(loci = starts, spreadProb = 0.30, maxSize = 25L, returnIndices = 1L)),
    list(name = "maxSize_vector_id",     args = list(loci = starts, spreadProb = 0.30,
                                                     maxSize = c(15L, 20L, 25L, 30L, 35L),
                                                     id = TRUE, returnIndices = 1L)),
    list(name = "iterations_finite",     args = list(loci = starts, spreadProb = 0.30, iterations = 5L, returnIndices = 1L)),
    list(name = "neighProbs",            args = list(loci = starts, spreadProb = 0.30, neighProbs = c(0.7, 0.3), returnIndices = 1L)),
    list(name = "spreadProb_raster",     args = list(loci = starts, spreadProb = spsRas, returnIndices = 1L)),
    list(name = "allowOverlap",          args = list(loci = starts, spreadProb = 0.225, allowOverlap = TRUE, returnIndices = 1L)),
    list(name = "relativeSpreadProb",    args = list(loci = starts, spreadProb = spRelAbs,
                                                     neighProbs = c(0, 1), maxSize = 30L,
                                                     exactSizes = TRUE, returnIndices = 1L)),
    list(name = "circle_maxRadius",      args = list(loci = starts, spreadProb = 0.30,
                                                     circle = TRUE, circleMaxRadius = 10, returnIndices = 1L))
  )

  spread2_scenarios <- list(
    list(name = "default_dt",            args = list(start = starts, spreadProb = 0.225, asRaster = FALSE)),
    list(name = "default_raster",        args = list(start = starts, spreadProb = 0.225, asRaster = TRUE)),
    list(name = "directions_4",          args = list(start = starts, spreadProb = 0.30, directions = 4L, asRaster = FALSE)),
    list(name = "directions_8",          args = list(start = starts, spreadProb = 0.30, directions = 8L, asRaster = FALSE)),
    list(name = "maxSize_scalar",        args = list(start = starts, spreadProb = 0.30, maxSize = 25L, asRaster = FALSE)),
    list(name = "maxSize_vector",        args = list(start = starts, spreadProb = 0.30,
                                                     maxSize = c(15, 20, 25, 30, 35), asRaster = FALSE)),
    list(name = "iterations_finite",     args = list(start = starts, spreadProb = 0.30, iterations = 5L, asRaster = FALSE)),
    list(name = "neighProbs",            args = list(start = starts, spreadProb = 0.30, neighProbs = c(0.7, 0.3), asRaster = FALSE)),
    list(name = "persistProb_scalar",    args = list(start = starts, spreadProb = 0.225, persistProb = 0.5,
                                                     asRaster = FALSE, iterations = 5L)),
    list(name = "spreadProb_raster",     args = list(start = starts, spreadProb = spsRas, asRaster = FALSE)),
    list(name = "spreadProbRel",         args = list(start = starts, spreadProb = 0.225, spreadProbRel = spRel, asRaster = FALSE)),
    list(name = "returnDistances",       args = list(start = starts, spreadProb = 0.225, returnDistances = TRUE, asRaster = FALSE)),
    list(name = "returnDirections",      args = list(start = starts, spreadProb = 0.225, returnDirections = TRUE, asRaster = FALSE)),
    list(name = "returnFrom",            args = list(start = starts, spreadProb = 0.225, returnFrom = TRUE, asRaster = FALSE)),
    list(name = "allowOverlap_TRUE",     args = list(start = starts, spreadProb = 0.225, allowOverlap = TRUE, asRaster = FALSE)),
    list(name = "circle_TRUE",           args = list(start = starts, spreadProb = 0.30, circle = TRUE, asRaster = FALSE))
  )

  seeds <- c(1L, 17L, 1234L)

  emit <- function(fn_name, fn, scenarios) {
    for (sc in scenarios) {
      for (sd in seeds) {
        set.seed(sd)
        out <- do.call(fn, c(list(landscape = ras), sc$args))
        out <- cmpForm(out)
        f <- file.path(out_dir, sprintf("%s__%s__seed%d.rds", fn_name, sc$name, sd))
        saveRDS(out, f, version = 2)
        cat(sprintf("wrote %s\n", basename(f)))
      }
    }
  }

  emit("spread",  spread,  spread_scenarios)
  emit("spread2", spread2, spread2_scenarios)
})
