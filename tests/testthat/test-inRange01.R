## The question spread() asks of spreadProb before it starts. Expressed as
## all(inRange(na.omit(x))) it copies the vector twice and scans it three times,
## which on a per-cell spreadProb is work proportional to the landscape on every
## call, however little of it burns. allInRange01() is one pass with early exit,
## and must answer identically -- including for the edge cases where "all of
## nothing" is TRUE.

refImpl <- function(x) all(SpaDES.tools::inRange(stats::na.omit(x)))

test_that("allInRange01 agrees with all(inRange(na.omit(.))) on ordinary vectors", {
  set.seed(1)
  expect_identical(allInRange01(runif(1000)), refImpl(runif(1000)))
  expect_true(allInRange01(c(0, 0.5, 1)))
  expect_identical(allInRange01(c(0, 0.5, 1)), refImpl(c(0, 0.5, 1)))
})

test_that("it rejects out-of-range values wherever they sit", {
  x <- runif(1000)
  for (i in c(1L, 500L, 1000L)) {
    hi <- x; hi[i] <- 1.0001
    lo <- x; lo[i] <- -1e-9
    expect_false(allInRange01(hi))
    expect_false(allInRange01(lo))
    expect_identical(allInRange01(hi), refImpl(hi))
    expect_identical(allInRange01(lo), refImpl(lo))
  }
})

test_that("NA and NaN are ignored, as na.omit() ignores them", {
  x <- c(0.1, NA, 0.9, NaN)
  expect_true(allInRange01(x))
  expect_identical(allInRange01(x), refImpl(x))
  bad <- c(0.1, NA, 2, NaN)
  expect_false(allInRange01(bad))
  expect_identical(allInRange01(bad), refImpl(bad))
})

test_that("nothing out of range is TRUE: empty and all-NA vectors", {
  expect_true(allInRange01(numeric(0)))
  expect_identical(allInRange01(numeric(0)), refImpl(numeric(0)))
  expect_true(allInRange01(rep(NA_real_, 5)))
  expect_identical(allInRange01(rep(NA_real_, 5)), refImpl(rep(NA_real_, 5)))
})

test_that("infinities are out of range", {
  expect_false(allInRange01(c(0.5, Inf)))
  expect_false(allInRange01(c(0.5, -Inf)))
  expect_identical(allInRange01(c(0.5, Inf)), refImpl(c(0.5, Inf)))
})

test_that("integer input is handled", {
  expect_true(allInRange01(c(0L, 1L)))
  expect_false(allInRange01(c(0L, 2L)))
})
