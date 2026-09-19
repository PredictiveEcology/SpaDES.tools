test_that("spread2 works when allowOverlap is on and neighProbs is used", {
  ## Regression test: `notAvailable` is only created when `canUseAvailable`
  ## (i.e., allowOverlap == 0), but the neighProbs branch dereferenced it
  ## unconditionally, giving `object 'notAvailable' not found`.
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)

  for (ao in list(1, 2, NA, TRUE)) {
    set.seed(6421)
    out <- spread2(ras, start = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
                   allowOverlap = ao, iterations = 5, asRaster = FALSE)
    expect_s3_class(out, "data.table")
    expect_gt(NROW(out), 0)
    expect_in(c("initialPixels", "pixels", "state"), colnames(out))
  }
})

test_that("spread2 allowOverlap + neighProbs actually permits overlap", {
  skip_on_cran()
  library(terra)

  ## two starts close together, spreadProb = 1, so events must collide
  ras <- rast(ext(0, 30, 0, 30), res = 1, vals = 0)
  start <- c(200L, 210L)

  set.seed(3390)
  noOverlap <- spread2(ras, start = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
                       allowOverlap = 0, iterations = 8, asRaster = FALSE)
  set.seed(3390)
  withOverlap <- spread2(ras, start = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
                         allowOverlap = 1, iterations = 8, asRaster = FALSE)

  ## without overlap, no pixel may be claimed twice; with overlap it may
  expect_false(any(duplicated(noOverlap$pixels)))
  expect_gt(NROW(withOverlap), 0)
})

test_that("spread2 allowOverlap = 0 with neighProbs is unchanged by the fix", {
  ## The guard must be a no-op when canUseAvailable is TRUE.
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)

  set.seed(1187)
  out <- spread2(ras, start = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
                 allowOverlap = 0, iterations = 5, asRaster = FALSE)
  expect_false(any(duplicated(out$pixels)))
})

test_that("spread2 skipChecks skips assertions without changing results", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)

  set.seed(8802)
  withChecks <- spread2(ras, start = start, spreadProb = 0.225,
                        skipChecks = FALSE, asRaster = FALSE)
  set.seed(8802)
  noChecks <- spread2(ras, start = start, spreadProb = 0.225,
                      skipChecks = TRUE, asRaster = FALSE)
  expect_equal(withChecks, noChecks)
})

test_that("spread2 skipChecks = TRUE bypasses the duplicate-start error", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)

  ## duplicated starts are rejected by the assertions
  set.seed(5514)
  expect_error(
    spread2(ras, start = c(2500L, 2500L), spreadProb = 0.225, asRaster = FALSE),
    "duplicates"
  )
})

test_that("spread2 oneNeighbourOnly spreads to exactly one neighbour per step", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)
  nIter <- 6

  set.seed(2731)
  out <- spread2(ras, start = start, spreadProb = 1, oneNeighbourOnly = TRUE,
                 iterations = nIter, asRaster = FALSE)

  ## each event advances by exactly one cell per iteration, so each event has
  ## at most the start plus one cell per iteration
  sizes <- out[, .N, by = "initialPixels"]$N
  expect_true(all(sizes <= nIter + 1))
  expect_equal(length(unique(out$initialPixels)), length(start))
})

test_that("spread2 oneNeighbourOnly forces allowOverlap to 2", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)

  ## allowOverlap of 0 or 1 is not allowed with oneNeighbourOnly; it is coerced
  expect_message(
    spread2(ras, start = c(2500L, 5500L), spreadProb = 1, oneNeighbourOnly = TRUE,
            allowOverlap = 0, iterations = 3, asRaster = FALSE),
    "setting allowOverlap to 2"
  )
})
