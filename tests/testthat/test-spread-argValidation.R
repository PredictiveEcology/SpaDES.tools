test_that("spread refuses argument combinations it does not implement", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)
  spRas <- setValues(ras, runif(ncell(ras), 0.1, 0.3))

  ## These used to fail deep inside the algorithm with opaque errors
  ## ("subscript out of bounds", "dims [product 9999] do not match ...").
  expect_error(
    spread(ras, loci = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
           returnDistances = TRUE, returnIndices = TRUE),
    "neighProbs and returnDistances"
  )
  expect_error(
    spread(ras, loci = start, spreadProb = spRas, asymmetry = 2,
           asymmetryAngle = 90, returnIndices = TRUE),
    "asymmetry with a raster or vector spreadProb"
  )
  expect_error(
    spread(ras, loci = start, spreadProb = values(spRas)[, 1], asymmetry = 2,
           asymmetryAngle = 90, returnIndices = TRUE),
    "asymmetry with a raster or vector spreadProb"
  )
})

test_that("spread still allows the neighbouring legal combinations", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)
  spRas <- setValues(ras, runif(ncell(ras), 0.1, 0.3))

  ## each ingredient on its own must keep working
  set.seed(7215)
  expect_s3_class(
    spread(ras, loci = start, spreadProb = 1, neighProbs = c(0.5, 0.5),
           returnIndices = TRUE), "data.table")
  set.seed(7215)
  expect_s3_class(
    spread(ras, loci = start, spreadProb = 0.225, returnDistances = TRUE,
           returnIndices = TRUE), "data.table")
  set.seed(7215)
  expect_s3_class(
    spread(ras, loci = start, spreadProb = 0.225, asymmetry = 2,
           asymmetryAngle = 90, maxSize = 100, returnIndices = TRUE), "data.table")
  ## a raster spreadProb without asymmetry is fine
  set.seed(7215)
  expect_s3_class(
    spread(ras, loci = start, spreadProb = spRas, returnIndices = TRUE), "data.table")
})

test_that("spread exactSizes works without the matrix version of spreads", {
  ## Regression test: the retry branch referenced `spreads`, which only exists
  ## when `useMatrixVersionSpreads` (allowOverlap or returnDistances or
  ## spreadState). Without those, it failed with "object 'spreads' not found".
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)

  set.seed(9634)
  out <- spread(ras, loci = start, spreadProb = 0.225, maxSize = 100,
                exactSizes = TRUE, returnIndices = TRUE)
  expect_s3_class(out, "data.table")
  expect_equal(length(unique(out$id)), length(start))
  ## exactSizes is a cap as well as a target: never exceed it
  expect_true(all(out[, .N, by = "id"]$N <= 100))

  ## and with returnIndices = FALSE (raster return)
  set.seed(9634)
  expect_no_error(
    spread(ras, loci = start, spreadProb = 0.225, maxSize = 100, exactSizes = TRUE)
  )
})

test_that("spread exactSizes reaches the requested size when spreadProb allows", {
  skip_on_cran()
  library(terra)

  ras <- rast(ext(0, 100, 0, 100), res = 1, vals = 0)
  start <- c(2500L, 5500L, 7500L)

  set.seed(4408)
  out <- spread(ras, loci = start, spreadProb = 1, maxSize = 100,
                exactSizes = TRUE, returnIndices = TRUE)
  expect_true(all(out[, .N, by = "id"]$N == 100))
})
