## `spread()` could not be called with a numeric `spreadProbLater` at all. Supplying
## one set `relativeSpreadProb <- TRUE` unconditionally, whatever its values, which
## routed the call into the relative-probability path guarded by
## `if (!is.null(neighProbs) || relativeSpreadProb)`. That path indexes `numNeighs`,
## which stays NULL unless `neighProbs` was supplied, so
## `resample(aaa[[x]], size = numNeighs[x])` received size = NULL and the call died
## with "invalid 'size' argument".
##
## The raster branch of the same validation had always been conditional, which is why
## a `spreadProbLater` given as a SpatRaster worked and the numeric equivalent did not.

lscape <- function(n = 60) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
}

test_that("a scalar numeric spreadProbLater runs", {
  skip_if_not_installed("terra")
  r <- lscape()

  set.seed(7)
  out <- spread(landscape = r, loci = 1830L, spreadProb = 0.3, spreadProbLater = 0.05,
                iterations = 4, returnIndices = TRUE)

  expect_true(NROW(out) >= 1)
})

test_that("a per-cell numeric spreadProbLater runs", {
  skip_if_not_installed("terra")
  r <- lscape()
  nc <- terra::ncell(r)

  set.seed(7)
  out <- spread(landscape = r, loci = 1830L, spreadProb = rep(0.3, nc),
                spreadProbLater = rep(0.05, nc), iterations = 4, returnIndices = TRUE)

  expect_true(NROW(out) >= 1)
})

test_that("numeric and raster spreadProbLater agree, which is what the asymmetry hid", {
  skip_if_not_installed("terra")
  r <- lscape()
  nc <- terra::ncell(r)

  set.seed(11)
  viaNumeric <- spread(landscape = r, loci = 1830L, spreadProb = rep(0.3, nc),
                       spreadProbLater = rep(0.05, nc), iterations = 5,
                       returnIndices = TRUE)
  set.seed(11)
  viaRaster <- spread(landscape = r, loci = 1830L,
                      spreadProb = terra::setValues(terra::rast(r), rep(0.3, nc)),
                      spreadProbLater = terra::setValues(terra::rast(r), rep(0.05, nc)),
                      iterations = 5, returnIndices = TRUE)

  expect_equal(viaNumeric, viaRaster)
})

test_that("an out-of-range spreadProbLater is still rejected", {
  skip_if_not_installed("terra")
  r <- lscape()

  expect_error(
    spread(landscape = r, loci = 1830L, spreadProb = 0.3, spreadProbLater = 2,
           returnIndices = TRUE),
    "spreadProbLater is not a probability"
  )
})

test_that("spreadProbLater does not silently switch on relative spread probabilities", {
  skip_if_not_installed("terra")
  ## The behavioural statement behind the fix: an in-range spreadProbLater is not a
  ## request for relative probabilities, so it must not change what an otherwise
  ## identical call produces before spreadProbLater takes effect at iteration 2.
  r <- lscape()

  set.seed(99)
  withLater <- spread(landscape = r, loci = 1830L, spreadProb = 0.3,
                      spreadProbLater = 0.05, iterations = 1, returnIndices = TRUE)
  set.seed(99)
  without <- spread(landscape = r, loci = 1830L, spreadProb = 0.3,
                    iterations = 1, returnIndices = TRUE)

  expect_equal(withLater, without)
})
