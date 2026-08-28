## Drop-in equivalence: assert that the post-Rcpp spread() / spread2()
## produce baseline-equivalent output to the pre-Rcpp implementation
## (commit 21b5e0c^) for every seeded scenario. Snapshots in
## _spread_snapshots/ were generated from that baseline by
## _spread_snapshots/_generate.R.
##
## SpatRaster output is compared via terra::values() (raw matrix), and
## data.tables via expect_equal() — the Rcpp helpers return integer
## indices where the original R path yielded numeric, which does not
## change values but does change storage type. Use expect_identical()
## for everything else (matrices, lists, vectors).
##
## A failure here means the C++ helpers are no longer drop-in — either
## the emit order changed, an edge filter regressed, or someone moved
## the R-side sample.int shuffle.

cmpToBaseline <- function(actual, expected, info) {
  ## SpatRaster values come pre-extracted by cmpForm (an integer/numeric matrix).
  if (data.table::is.data.table(actual)) {
    expect_equal(actual, expected, info = info)
  } else {
    expect_identical(actual, expected, info = info)
  }
}

test_that("spread() output matches pre-Rcpp baseline (seeded grid)", {
  ## testInit() chdir's into a tempdir, so capture the snapshot dir as an
  ## absolute path *before* calling it.
  snapDir <- normalizePath(testthat::test_path("_spread_snapshots"), mustWork = FALSE)
  testInit(c("terra", "withr"))

  ## Force the base R sample.int branch — same shim as the existing
  ## bit-identical-with-self test. dqrng's RNG state carries non-determinism
  ## across spread()'s internal reseeding, so we compare on the deterministic
  ## branch.
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
    withr::defer({
      unlockBinding("requireNamespace", base_env)
      assign("requireNamespace", rN_orig, envir = base_env)
      lockBinding("requireNamespace", base_env)
    })
  }

  ## Fail rather than skip: the snapshots ship in the tarball, so a missing
  ## directory means the baseline was lost, not that this environment cannot
  ## run the test. Skipping here would pass green while asserting nothing.
  expect_true(dir.exists(snapDir))

  ras <- terra::rast(terra::ext(0, 80, 0, 80), resolution = 1, vals = 1)
  withr::with_seed(7L,   spsRas <- terra::rast(ras, vals = stats::runif(terra::ncell(ras), 0.10, 0.40)))
  withr::with_seed(11L,  spRel  <- terra::rast(ras, vals = stats::runif(terra::ncell(ras))))
  spRelAbs <- spRel * 100 + 1
  withr::with_seed(101L, starts <- sort(sample.int(terra::ncell(ras), 5L)))

  cmpForm <- function(x) {
    if (inherits(x, "SpatRaster")) terra::values(x) else x
  }

  scenarios <- list(
    list(name = "rare_branch",        args = list(loci = starts, spreadProb = 0.225)),
    list(name = "id_TRUE",             args = list(loci = starts, spreadProb = 0.225, id = TRUE)),
    list(name = "returnIndices_1",     args = list(loci = starts, spreadProb = 0.225, returnIndices = 1L)),
    list(name = "returnIndices_2",     args = list(loci = starts, spreadProb = 0.225, returnIndices = 2L)),
    list(name = "directions_4",        args = list(loci = starts, spreadProb = 0.30, directions = 4L, returnIndices = 1L)),
    list(name = "directions_8",        args = list(loci = starts, spreadProb = 0.30, directions = 8L, returnIndices = 1L)),
    list(name = "maxSize_scalar",      args = list(loci = starts, spreadProb = 0.30, maxSize = 25L, returnIndices = 1L)),
    list(name = "maxSize_vector_id",   args = list(loci = starts, spreadProb = 0.30,
                                                   maxSize = c(15L, 20L, 25L, 30L, 35L),
                                                   id = TRUE, returnIndices = 1L)),
    list(name = "iterations_finite",   args = list(loci = starts, spreadProb = 0.30, iterations = 5L, returnIndices = 1L)),
    list(name = "neighProbs",          args = list(loci = starts, spreadProb = 0.30, neighProbs = c(0.7, 0.3), returnIndices = 1L)),
    list(name = "spreadProb_raster",   args = list(loci = starts, spreadProb = spsRas, returnIndices = 1L)),
    list(name = "allowOverlap",        args = list(loci = starts, spreadProb = 0.225, allowOverlap = TRUE, returnIndices = 1L)),
    list(name = "relativeSpreadProb",  args = list(loci = starts, spreadProb = spRelAbs,
                                                   neighProbs = c(0, 1), maxSize = 30L,
                                                   exactSizes = TRUE, returnIndices = 1L)),
    list(name = "circle_maxRadius",    args = list(loci = starts, spreadProb = 0.30,
                                                   circle = TRUE, circleMaxRadius = 10, returnIndices = 1L))
  )

  for (sc in scenarios) {
    for (sd in c(1L, 17L, 1234L)) {
      f <- file.path(snapDir, sprintf("spread__%s__seed%d.rds", sc$name, sd))
      expect_true(file.exists(f), info = sprintf("missing snapshot: %s", basename(f)))
      set.seed(sd)
      out <- cmpForm(do.call(spread, c(list(landscape = ras), sc$args)))
      cmpToBaseline(out, readRDS(f),
                    info = sprintf("scenario=%s seed=%d", sc$name, sd))
    }
  }
})

test_that("spread2() output matches pre-Rcpp baseline (seeded grid)", {
  snapDir <- normalizePath(testthat::test_path("_spread_snapshots"), mustWork = FALSE)
  testInit(c("terra", "withr"))

  ## Fail rather than skip: the snapshots ship in the tarball, so a missing
  ## directory means the baseline was lost, not that this environment cannot
  ## run the test. Skipping here would pass green while asserting nothing.
  expect_true(dir.exists(snapDir))
  ## The baseline snapshots were captured with {bit} installed, so
  ## spread2()'s spreadState$notAvailable attribute is a <booltype/bit>
  ## integer vector. Without {bit}, spread2() falls back to a plain
  ## logical vector — values match but expect_equal() fails on type.
  ## Skip rather than carry a parallel set of bit-less snapshots.
  skip_if_not_installed("bit")

  ras <- terra::rast(terra::ext(0, 80, 0, 80), resolution = 1, vals = 1)
  withr::with_seed(7L,   spsRas <- terra::rast(ras, vals = stats::runif(terra::ncell(ras), 0.10, 0.40)))
  withr::with_seed(11L,  spRel  <- terra::rast(ras, vals = stats::runif(terra::ncell(ras))))
  withr::with_seed(101L, starts <- sort(sample.int(terra::ncell(ras), 5L)))

  cmpForm <- function(x) {
    if (inherits(x, "SpatRaster")) terra::values(x) else x
  }

  scenarios <- list(
    list(name = "default_dt",         args = list(start = starts, spreadProb = 0.225, asRaster = FALSE)),
    list(name = "default_raster",     args = list(start = starts, spreadProb = 0.225, asRaster = TRUE)),
    list(name = "directions_4",       args = list(start = starts, spreadProb = 0.30, directions = 4L, asRaster = FALSE)),
    list(name = "directions_8",       args = list(start = starts, spreadProb = 0.30, directions = 8L, asRaster = FALSE)),
    list(name = "maxSize_scalar",     args = list(start = starts, spreadProb = 0.30, maxSize = 25L, asRaster = FALSE)),
    list(name = "maxSize_vector",     args = list(start = starts, spreadProb = 0.30,
                                                  maxSize = c(15, 20, 25, 30, 35), asRaster = FALSE)),
    list(name = "iterations_finite",  args = list(start = starts, spreadProb = 0.30, iterations = 5L, asRaster = FALSE)),
    list(name = "neighProbs",         args = list(start = starts, spreadProb = 0.30, neighProbs = c(0.7, 0.3), asRaster = FALSE)),
    list(name = "persistProb_scalar", args = list(start = starts, spreadProb = 0.225, persistProb = 0.5,
                                                  asRaster = FALSE, iterations = 5L)),
    list(name = "spreadProb_raster",  args = list(start = starts, spreadProb = spsRas, asRaster = FALSE)),
    list(name = "spreadProbRel",      args = list(start = starts, spreadProb = 0.225, spreadProbRel = spRel, asRaster = FALSE)),
    list(name = "returnDistances",    args = list(start = starts, spreadProb = 0.225, returnDistances = TRUE, asRaster = FALSE)),
    list(name = "returnDirections",   args = list(start = starts, spreadProb = 0.225, returnDirections = TRUE, asRaster = FALSE)),
    list(name = "returnFrom",         args = list(start = starts, spreadProb = 0.225, returnFrom = TRUE, asRaster = FALSE)),
    list(name = "allowOverlap_TRUE",  args = list(start = starts, spreadProb = 0.225, allowOverlap = TRUE, asRaster = FALSE)),
    list(name = "circle_TRUE",        args = list(start = starts, spreadProb = 0.30, circle = TRUE, asRaster = FALSE))
  )

  for (sc in scenarios) {
    for (sd in c(1L, 17L, 1234L)) {
      f <- file.path(snapDir, sprintf("spread2__%s__seed%d.rds", sc$name, sd))
      expect_true(file.exists(f), info = sprintf("missing snapshot: %s", basename(f)))
      set.seed(sd)
      out <- cmpForm(do.call(spread2, c(list(landscape = ras), sc$args)))
      cmpToBaseline(out, readRDS(f),
                    info = sprintf("scenario=%s seed=%d", sc$name, sd))
    }
  }
})
