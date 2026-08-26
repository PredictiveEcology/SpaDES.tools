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

test_that("crw round-trips a SpatialPoints* object via SpatVector", {
  testInit("terra")
  skip_if_not_installed("sp")
  skip_if_not_installed("sf")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  ## sp in, sp out -- and the class is now preserved across a second call,
  ## which it was not when the SpatialPoints branch built its own result
  set.seed(2)
  expect_warning(spdf <- crw(sp::SpatialPoints(starts), stepLength = 5, stddev = 10),
                 "deprecated")
  expect_s4_class(spdf, "SpatialPointsDataFrame")

  set.seed(3)
  expect_warning(spdfNew <- crw(spdf, stepLength = 5, stddev = 10), "deprecated")
  expect_s4_class(spdfNew, "SpatialPointsDataFrame")

  ## the previous location is carried as attributes, not silently dropped
  expect_setequal(names(spdf), c("x1", "y1"))
  expect_equal(unname(as.matrix(spdfNew@data[, c("x1", "y1")])),
               unname(sp::coordinates(spdf)))

  ## every agent moved exactly stepLength, on both calls
  expect_equal(unname(sqrt(rowSums((sp::coordinates(spdf) - starts) ^ 2))), rep(5, N))
  expect_equal(unname(sqrt(rowSums((sp::coordinates(spdfNew) -
                                      sp::coordinates(spdf)) ^ 2))), rep(5, N))
})

test_that("crw gives the same walk for sp and SpatVector input", {
  testInit("terra")
  skip_if_not_installed("sp")
  skip_if_not_installed("sf")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  ## the sp path is now the SpatVector path with conversions bolted on either
  ## end, so the two must agree exactly
  set.seed(2)
  viaVect <- crw(terra::vect(starts), stepLength = 5, stddev = 10)
  set.seed(2)
  expect_warning(viaSp <- crw(sp::SpatialPoints(starts), stepLength = 5, stddev = 10))

  expect_equal(unname(terra::crds(viaVect)), unname(sp::coordinates(viaSp)))
})

test_that("crw(returnMatrix = TRUE) skips the sp round-trip", {
  testInit("terra")
  skip_if_not_installed("sp")
  skip_if_not_installed("sf")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  set.seed(2)
  expect_warning(m <- crw(sp::SpatialPoints(starts), stepLength = 5, stddev = 10,
                          returnMatrix = TRUE))
  expect_true(is.matrix(m))
  expect_setequal(colnames(m), c("x", "y", "x1", "y1"))
})

test_that("move() dispatches to crw", {
  testInit("terra")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  set.seed(2)
  viaMove <- move("crw", starts, stepLength = 5, stddev = 10)
  set.seed(2)
  direct <- crw(starts, stepLength = 5, stddev = 10)

  expect_equal(viaMove, direct)
})

test_that("crw carries x1/y1 forward when handed its own matrix output", {
  testInit("terra")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  ## the documented fast path: feed the matrix straight back in. The second
  ## call must read the real previous position out of x1/y1 rather than
  ## inventing one, so the walk stays correlated.
  first <- crw(starts, stepLength = 5, stddev = 10)
  second <- crw(first, stepLength = 5, stddev = 10)

  expect_true(is.matrix(second))
  expect_equal(unname(second[, c("x1", "y1")]), unname(first[, c("x", "y")]))
  expect_equal(unname(sqrt(rowSums((second[, c("x", "y")] -
                                      first[, c("x", "y")]) ^ 2))), rep(5, N))
})

test_that("crw(returnMatrix = TRUE) keeps a SpatVector's attributes", {
  testInit("terra")

  set.seed(1)
  N <- 10L
  starts <- cbind(x = stats::runif(N, -50, 50), y = stats::runif(N, -50, 50))

  ## a SpatVector that already carries x1/y1 as attributes
  withAtts <- crw(terra::vect(starts), stepLength = 5, stddev = 10)
  expect_setequal(names(withAtts), c("x1", "y1"))

  asMatrix <- crw(withAtts, stepLength = 5, stddev = 10, returnMatrix = TRUE)
  expect_true(is.matrix(asMatrix))
  ## the attributes come across into the matrix rather than being dropped
  expect_setequal(colnames(asMatrix), c("x", "y", "x1", "y1"))
})

test_that("crw requires a logical lonlat", {
  testInit("terra")

  set.seed(1)
  starts <- cbind(x = stats::runif(5, -50, 50), y = stats::runif(5, -50, 50))

  expect_error(crw(starts, stepLength = 5, stddev = 10, lonlat = NULL), "lonlat")
  expect_error(crw(starts, stepLength = 5, stddev = 10, lonlat = "yes"), "lonlat")
})
