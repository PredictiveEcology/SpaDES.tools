## Two pieces of per-call work in spread() that are proportional to the size of
## the landscape rather than to how much burns:
##
##  * a zero-filled state vector of length ncell, built with rep(0L, n) where
##    integer(n) is already zero-filled and 3.7x cheaper;
##  * for a gridded spreadProb, `spreadProb[]` -- a full copy of the raster --
##    inside the iteration loop, so a 30-iteration fire on 8.3M cells copied
##    66 MB thirty times to read a few thousand cells.
##
## Neither may change the answer, and a gridded spreadProb must keep agreeing
## with the equivalent vector under the same seed.

test_that("a gridded spreadProb gives the same answer as the equivalent vector", {
  skip_if_not_installed("terra")
  n <- 200
  set.seed(1)
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
  probs <- runif(terra::ncell(r), 0.1, 0.3)
  spRas <- terra::rast(r, vals = probs)
  loci <- sample(terra::ncell(r), 12)
  ms <- pmax(2, round(rlnorm(12, log(200), 1)))

  set.seed(42)
  asVector <- SpaDES.tools::spread(landscape = r, maxSize = ms, loci = loci, spreadProb = probs,
                                   returnIndices = TRUE, allowOverlap = FALSE, quick = TRUE)
  set.seed(42)
  asRaster <- SpaDES.tools::spread(landscape = r, maxSize = ms, loci = loci, spreadProb = spRas,
                                   returnIndices = TRUE, allowOverlap = FALSE, quick = TRUE)
  expect_identical(asRaster, asVector)
})

test_that("a gridded spreadProb still spreads over several iterations", {
  ## Guards the hoist: if the vector were taken from the wrong object, or not
  ## refreshed, fires would stop after one ring or spread with wrong values.
  skip_if_not_installed("terra")
  n <- 100
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
  spRas <- terra::rast(r, vals = rep(0.3, terra::ncell(r)))
  set.seed(7)
  out <- SpaDES.tools::spread(landscape = r, maxSize = 400, loci = 5050L, spreadProb = spRas,
                              returnIndices = TRUE, allowOverlap = FALSE, quick = TRUE)
  expect_gt(NROW(out), 50)              # many iterations' worth of cells
  expect_true(all(out$indices %in% seq_len(terra::ncell(r))))
})

test_that("spreadProbLater takes over from iteration 2, gridded or not", {
  skip_if_not_installed("terra")
  n <- 100
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
  first <- rep(0.4, terra::ncell(r))
  later <- rep(0, terra::ncell(r))       # nothing spreads after the first ring
  set.seed(9)
  vecOut <- SpaDES.tools::spread(landscape = r, loci = 5050L, spreadProb = first,
                                 spreadProbLater = later, returnIndices = TRUE, quick = TRUE)
  set.seed(9)
  rasOut <- SpaDES.tools::spread(landscape = r, loci = 5050L,
                                 spreadProb = terra::rast(r, vals = first),
                                 spreadProbLater = terra::rast(r, vals = later),
                                 returnIndices = TRUE, quick = TRUE)
  expect_identical(rasOut, vecOut)
  expect_lt(NROW(vecOut), 12)            # the start plus at most its 8 neighbours
})

test_that("integer(n) is what rep(0L, n) was, so the state vector is unchanged", {
  expect_identical(integer(5), rep(0L, 5))
})
