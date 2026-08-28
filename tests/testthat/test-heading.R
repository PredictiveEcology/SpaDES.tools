test_that("heading returns compass bearings for the cardinal directions", {
  testInit("terra")

  origin <- matrix(0, nrow = 8L, ncol = 2L, dimnames = list(NULL, c("x", "y")))
  to <- rbind(
    N  = c(0,  1), NE = c(1,  1), E  = c(1,  0), SE = c(1, -1),
    S  = c(0, -1), SW = c(-1, -1), W = c(-1, 0), NW = c(-1, 1)
  )
  colnames(to) <- c("x", "y")

  ## Due south previously came back as 0: the old atan(xs / ys) implementation
  ## corrected quadrants by the sign of xs, so xs == 0 & ys < 0 matched
  ## neither correction.
  expect_equal(unname(heading(origin, to)),
               c(0, 45, 90, 135, 180, 225, 270, 315))
})

test_that("heading is 0 for a zero-length move and wraps into [0, 360)", {
  same <- matrix(c(3, 7), ncol = 2L, dimnames = list(NULL, c("x", "y")))
  expect_equal(unname(heading(same, same)), 0)

  set.seed(42)
  n <- 200L
  from <- cbind(x = stats::runif(n, -50, 50), y = stats::runif(n, -50, 50))
  to   <- cbind(x = stats::runif(n, -50, 50), y = stats::runif(n, -50, 50))
  h <- heading(from, to)

  expect_length(h, n)
  expect_true(all(h >= 0 & h < 360))

  ## reversing the move rotates the bearing by 180 degrees
  expect_equal(heading(to, from), (h + 180) %% 360)
})

test_that("heading accepts SpatVector and matrix inputs interchangeably", {
  testInit("terra")

  set.seed(1234)
  N <- 10L
  x1 <- stats::runif(N, -50, 50)
  y1 <- stats::runif(N, -50, 50)
  x0 <- stats::rnorm(N, x1, 5)
  y0 <- stats::rnorm(N, y1, 5)

  prevMat <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x", "y")))
  currMat <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x", "y")))
  prevVec <- terra::vect(cbind(x = x1, y = y1))
  currVec <- terra::vect(cbind(x = x0, y = y0))

  expected <- heading(prevMat, currMat)

  ## all four combinations from the documented example agree
  expect_equal(unname(heading(prevVec, currVec)), unname(expected))
  expect_equal(unname(heading(prevVec, currMat)), unname(expected))
  expect_equal(unname(heading(prevMat, currVec)), unname(expected))
})
