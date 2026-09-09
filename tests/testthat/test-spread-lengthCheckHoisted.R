## The numeric spreadProb length check used to live inside spread()'s iteration
## loop, where it re-called terra::ncell(landscape) -- S4 dispatch plus the
## accessor -- on every iteration of every call. Neither spreadProb nor
## spreadProbLater can change length inside the loop, so it is now checked once.
## These tests pin the behaviour that must survive the move: the errors still
## happen, they name the offending argument, and valid inputs still spread.

testLandscape <- function(n = 60) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n, ymin = 0, ymax = n, vals = 1)
}

test_that("a numeric spreadProb of the wrong length is rejected", {
  skip_if_not_installed("terra")
  r <- testLandscape()

  expect_error(
    spread(landscape = r, loci = 100L, spreadProb = rep(0.2, 5), returnIndices = TRUE),
    "spreadProb must be length 1 or length"
  )
})

test_that("the rejection does not depend on reaching an iteration that needed it", {
  skip_if_not_installed("terra")
  ## Previously the check only fired once the loop reached the numeric branch, so
  ## whether a bad argument was caught depended on how far the spread got. Now it is
  ## checked up front, which is what makes hoisting it safe.
  r <- testLandscape()

  expect_error(
    spread(landscape = r, loci = 100L, spreadProb = rep(0.2, 5), iterations = 1,
           returnIndices = TRUE),
    "spreadProb must be length 1 or length"
  )
})

test_that("a spreadProbLater of the wrong length is rejected, and named", {
  skip_if_not_installed("terra")
  r <- testLandscape()

  expect_error(
    spread(landscape = r, loci = 100L, spreadProb = 0.2,
           spreadProbLater = rep(0.3, 7), returnIndices = TRUE),
    "spreadProbLater must be length 1 or length"
  )
})

test_that("valid spreadProb lengths still spread: length 1 and length ncell", {
  skip_if_not_installed("terra")
  r <- testLandscape()
  nc <- terra::ncell(r)

  set.seed(123)
  scalarRun <- spread(landscape = r, loci = 1830L, spreadProb = 0.23, returnIndices = TRUE)
  set.seed(123)
  perCellRun <- spread(landscape = r, loci = 1830L, spreadProb = rep(0.23, nc),
                       returnIndices = TRUE)

  expect_true(NROW(scalarRun) > 1)
  expect_true(NROW(perCellRun) > 1)
})

## A per-cell spreadProb combined with spreadProbLater is deliberately NOT tested
## here: it cannot run on development at all. The iteration-one branch does
## `rep(spreadProb, NROW(potentials))`, which recycles the whole landscape once per
## candidate cell, and the mismatch surfaces downstream -- as "arguments must have
## same length" from tapply() at R/spread.R:898 on this branch. That is PR #130's
## bug and PR #130's test; hoisting the length check neither causes nor fixes it.

test_that("the check stays on under quick = TRUE, being O(1)", {
  skip_if_not_installed("terra")
  ## `quick` skips the O(ncell) probability scan. A length comparison is O(1), so it
  ## is not what `quick` is for, and a wrong length would otherwise fail later and
  ## less clearly.
  r <- testLandscape()

  expect_error(
    spread(landscape = r, loci = 100L, spreadProb = rep(0.2, 5), quick = TRUE,
           returnIndices = TRUE),
    "spreadProb must be length 1 or length"
  )
})
