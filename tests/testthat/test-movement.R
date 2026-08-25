test_that("crw moves every agent by exactly stepLength", {
  testInit("terra")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  set.seed(2)
  moved <- crw(starts, stepLength = 5, stddev = 10)

  expect_true(is.matrix(moved))
  expect_identical(nrow(moved), N)
  ## the previous position is carried alongside the new one
  expect_identical(colnames(moved), c("x", "y", "x1", "y1"))
  expect_equal(unname(moved[, c("x1", "y1")]), unname(starts))

  stepped <- sqrt((moved[, "x"] - starts[, "x"]) ^ 2 + (moved[, "y"] - starts[, "y"]) ^ 2)
  expect_equal(unname(stepped), rep(5, N))
})

test_that("crw accepts a per-agent stepLength", {
  set.seed(1)
  N <- 30L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))
  lengths <- rep(c(2, 8), N / 2)

  set.seed(5)
  moved <- crw(starts, stepLength = lengths, stddev = 10)
  stepped <- sqrt((moved[, "x"] - starts[, "x"]) ^ 2 + (moved[, "y"] - starts[, "y"]) ^ 2)
  expect_equal(unname(stepped), lengths)
})

test_that("crw is reproducible and agrees across input classes", {
  testInit("terra")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))
  agent <- terra::vect(starts)

  ## same seed, same walk
  set.seed(2); a <- crw(starts, stepLength = 5, stddev = 10)
  set.seed(2); b <- crw(starts, stepLength = 5, stddev = 10)
  expect_equal(a, b)

  ## a SpatVector in gives a SpatVector out, at the same coordinates the
  ## matrix path produces
  set.seed(2)
  movedVect <- crw(agent, stepLength = 5, stddev = 10)
  expect_s4_class(movedVect, "SpatVector")
  expect_identical(terra::geomtype(movedVect), "points")
  expect_equal(unname(terra::crds(movedVect)), unname(a[, c("x", "y")]))

  ## returnMatrix short-circuits the SpatVector round-trip
  set.seed(2)
  movedMat <- crw(agent, stepLength = 5, stddev = 10, returnMatrix = TRUE)
  expect_true(is.matrix(movedMat))
  expect_equal(unname(movedMat[, c("x", "y")]), unname(a[, c("x", "y")]))

  ## walks chain: feeding the output back in keeps it a SpatVector
  set.seed(3)
  again <- crw(movedVect, stepLength = 5, stddev = 10)
  expect_s4_class(again, "SpatVector")
  expect_identical(nrow(terra::crds(again)), N)
})

test_that("crw wraps to the extent when torus is TRUE", {
  testInit("terra")

  set.seed(3)
  N <- 30L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))
  ext <- terra::ext(-50, 50, -50, 50)

  ## a step long enough to leave the extent
  set.seed(4)
  wrapped <- crw(starts, extent = ext, stepLength = 30, stddev = 20, torus = TRUE)
  expect_true(all(wrapped[, "x"] >= -50 & wrapped[, "x"] <= 50))
  expect_true(all(wrapped[, "y"] >= -50 & wrapped[, "y"] <= 50))

  ## without the torus, the same walk leaves it
  set.seed(4)
  free <- crw(starts, stepLength = 30, stddev = 20, torus = FALSE)
  expect_true(any(free[, "x"] < -50 | free[, "x"] > 50 |
                    free[, "y"] < -50 | free[, "y"] > 50))
})
