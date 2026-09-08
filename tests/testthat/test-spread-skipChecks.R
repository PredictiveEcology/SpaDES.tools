## `skipChecks` is what spread2() and spread3() call this. Passed to spread() it
## used to land in `...` and be silently ignored, so callers who thought they had
## switched the checks off paid for them on every call -- costly, because the
## expensive one is proportional to the number of cells in the landscape.

test_that("skipChecks is an argument of spread(), not something `...` swallows", {
  expect_true("skipChecks" %in% names(formals(SpaDES.tools::spread)))
})

test_that("skipChecks and quick have the same effect, and neither changes the answer", {
  skip_if_not_installed("terra")
  n <- 200
  set.seed(1)
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
  loci <- sample(terra::ncell(r), 15)
  sp <- runif(terra::ncell(r), 0.1, 0.3)
  ms <- pmax(2, round(rlnorm(15, log(150), 1)))
  one <- function(...) {
    set.seed(42)
    SpaDES.tools::spread(landscape = r, maxSize = ms, loci = loci, spreadProb = sp,
                         returnIndices = TRUE, allowOverlap = FALSE, ...)
  }
  checked <- one()
  expect_identical(one(quick = TRUE), checked)
  expect_identical(one(skipChecks = TRUE), checked)
})

test_that("an illegal spreadProb is still rejected when the checks are on", {
  skip_if_not_installed("terra")
  n <- 50
  r <- terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
  sp <- rep(0.2, terra::ncell(r)); sp[10] <- 1.5
  expect_error(SpaDES.tools::spread(landscape = r, loci = 100L, spreadProb = sp,
                                    returnIndices = TRUE, quick = FALSE),
               "not a probability")
  ## and is not, when they are off -- which is the bargain the argument offers
  expect_silent(invisible(SpaDES.tools::spread(landscape = r, loci = 100L, spreadProb = sp,
                                               returnIndices = TRUE, skipChecks = TRUE)))
})
