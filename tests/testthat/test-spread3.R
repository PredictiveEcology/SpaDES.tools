## spread3() is documented as experimental and its example is wrapped in
## \donttest with the note "these tests are fairly heavy". The statistical
## assertions below therefore skip on CRAN, but still run under covr and on
## CI, which is where they are useful.

## Build the scenario from the documented example, minus the plotting.
spread3Setup <- function(seed = 123L, nStart = 30L, maxDim = 10000, res = 100) {
  ras <- terra::rast(terra::ext(c(0, maxDim, 0, maxDim)), resolution = res, vals = 0)
  rasQuality <- terra::rast(ras)
  rasQuality[] <- 1
  rasAbundance <- terra::rast(rasQuality)
  rasAbundance[] <- 0
  set.seed(seed)
  rasAbundance[sample(seq(terra::ncell(rasAbundance)), nStart)] <- 1000
  list(rasAbundance = rasAbundance, rasQuality = rasQuality)
}

## circular mean of angles in radians, returned in degrees on [0, 360)
circMeanDeg <- function(rad) {
  m <- atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
  (m + 360) %% 360
}

test_that("spread3 returns a dispersal table with the documented columns", {
  testInit(c("terra", "data.table"))

  s <- spread3Setup(nStart = 10L, maxDim = 4000)
  set.seed(1)
  out <- spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
                 advectionDir = 70, advectionMag = 4 * 100, meanDist = 1200,
                 verbose = 0, plot.it = FALSE)

  expect_s3_class(out, "data.table")
  expect_true(all(c("initialPixels", "from", "pixels", "state", "distance",
                    "abundActive", "abundSettled", "direction") %in% names(out)))
  expect_gt(nrow(out), 0L)

  ## distances are non-negative, and the starting cells are at distance 0
  expect_true(all(out$distance >= 0))
  expect_true(any(out$distance == 0))

  ## every pixel referenced is a real cell of the input raster
  expect_true(all(out$pixels >= 1 & out$pixels <= terra::ncell(s$rasAbundance)))

  ## nothing settles more than was released
  expect_lte(sum(out$abundSettled, na.rm = TRUE),
             sum(terra::values(s$rasAbundance), na.rm = TRUE))
})

test_that("spread3 is reproducible under a seed", {
  testInit(c("terra", "data.table"))

  s <- spread3Setup(nStart = 10L, maxDim = 4000)
  run <- function() {
    set.seed(99)
    spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
            advectionDir = 70, advectionMag = 400, meanDist = 1200,
            verbose = 0, plot.it = FALSE)
  }
  a <- run()
  b <- run()
  expect_identical(a$pixels, b$pixels)
  expect_equal(a$distance, b$distance)
  expect_equal(a$abundSettled, b$abundSettled)
})

test_that("spread3 settles ~63% of dispersers within advectionMag + meanDist", {
  skip_on_cran()
  testInit(c("terra", "data.table"))

  ## This is the check the documented example makes by eye: its
  ## plotDispersalKernel() draws the cumulative curve, marks
  ## advectionMag + meanDist, and comments "should be 0.63" -- the
  ## 1 - 1/e of an exponential dispersal kernel.
  advectionMag <- 400
  meanDist <- 2600
  s <- spread3Setup(seed = 123L)

  set.seed(123)
  out <- spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
                 advectionDir = 70, advectionMag = advectionMag,
                 meanDist = meanDist, verbose = 0, plot.it = FALSE)

  dt <- data.table::copy(out)
  dt[, disGroup := round(distance / 100) * 100]
  freqs <- dt[, .N, by = "disGroup"][order(disGroup)]
  freqs[, cumSum := cumsum(N)]

  frac <- freqs[disGroup == advectionMag + meanDist, cumSum] /
    freqs[nrow(freqs), cumSum]

  ## stochastic with 30 start points, so a band rather than a point value
  expect_gt(frac, 0.5)
  expect_lt(frac, 0.75)
})

test_that("spread3 disperses in the direction given by advectionDir", {
  skip_on_cran()
  testInit(c("terra", "data.table"))

  ## `direction` is reported in radians
  for (dir in c(70, 200)) {
    s <- spread3Setup(seed = 123L)
    set.seed(123)
    out <- spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
                   advectionDir = dir, advectionMag = 400, meanDist = 2600,
                   verbose = 0, plot.it = FALSE)

    got <- circMeanDeg(out[!is.na(direction)]$direction)
    ## within 20 degrees of the forcing direction, wrapped
    offBy <- min(abs(got - dir), 360 - abs(got - dir))
    expect_lt(offBy, 20)
  }
})

test_that("spread3 travels further when meanDist is larger", {
  skip_on_cran()
  testInit(c("terra", "data.table"))

  s <- spread3Setup(seed = 123L)
  far <- {
    set.seed(7)
    spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
            advectionDir = 70, advectionMag = 400, meanDist = 2600,
            verbose = 0, plot.it = FALSE)
  }
  near <- {
    set.seed(7)
    spread3(rasAbundance = s$rasAbundance, rasQuality = s$rasQuality,
            advectionDir = 70, advectionMag = 400, meanDist = 1000,
            verbose = 0, plot.it = FALSE)
  }
  expect_gt(mean(far$distance), mean(near$distance))
})
