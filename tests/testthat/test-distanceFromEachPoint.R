test_that("distanceFromEachPoint returns every from-to pair, not the nearest", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
  set.seed(2)
  n <- 3L
  coords <- cbind(x = round(stats::runif(n, 0, 20)) + 0.5,
                  y = round(stats::runif(n, 0, 20)) + 0.5)

  out <- distanceFromEachPoint(coords, landscape = ras)

  expect_true(is.matrix(out))
  expect_identical(colnames(out), c("x", "y", "dists"))
  ## this is the documented difference from terra::distance(): every pair,
  ## rather than the minimum over the set of points
  expect_identical(nrow(out), as.integer(n * terra::ncell(ras)))
  expect_true(all(out[, "dists"] >= 0))

  ## each from-point is at distance 0 from its own cell
  expect_equal(sum(out[, "dists"] == 0), n)

  ## distances agree with plain Euclidean geometry
  first <- out[1, ]
  expect_equal(unname(first["dists"]),
               unname(sqrt((first["x"] - coords[1, "x"]) ^ 2 +
                             (first["y"] - coords[1, "y"]) ^ 2)))
})

test_that("distanceFromEachPoint returns angles when asked", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
  coords <- cbind(x = 10.5, y = 10.5)

  out <- distanceFromEachPoint(coords, landscape = ras, angles = TRUE)
  expect_identical(colnames(out), c("x", "y", "dists", "angles"))
  expect_true(all(is.finite(out[, "angles"])))

  ## Pin the convention: zero at SOUTH, increasing eastward.
  ##
  ## Note this disagrees with directionFromEachPoint() in this same file,
  ## which puts zero at NORTH (as heading() does). The two are inverted in y
  ## with respect to each other. Pinned here rather than changed, since
  ## either could be what callers already depend on.
  at <- function(dx, dy) {
    i <- which(abs(out[, "x"] - (10.5 + dx)) < 1e-9 &
                 abs(out[, "y"] - (10.5 + dy)) < 1e-9)
    unname(out[i, "angles"])
  }
  expect_equal(at(0, -1), 0)          # south
  expect_equal(at(1, 0), pi / 2)      # east
  expect_equal(at(0, 1), pi)          # north
  expect_equal(at(-1, 0), -pi / 2)    # west
})

test_that("maxDistance trims the returned surface", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
  set.seed(2)
  coords <- cbind(x = round(stats::runif(3, 0, 20)) + 0.5,
                  y = round(stats::runif(3, 0, 20)) + 0.5)

  full <- distanceFromEachPoint(coords, landscape = ras)
  trimmed <- distanceFromEachPoint(coords, landscape = ras, maxDistance = 5)

  expect_lt(nrow(trimmed), nrow(full))
  expect_lte(max(trimmed[, "dists"]), 5)
  ## nothing within the radius was dropped
  expect_identical(nrow(trimmed), sum(full[, "dists"] <= 5))
})

test_that("cumulativeFn accumulates across from-points into one surface", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
  set.seed(2)
  coords <- cbind(x = round(stats::runif(3, 0, 20)) + 0.5,
                  y = round(stats::runif(3, 0, 20)) + 0.5)

  cum <- distanceFromEachPoint(coords, landscape = ras, cumulativeFn = `+`)

  ## one row per cell rather than one per from-to pair
  expect_identical(nrow(cum), as.integer(terra::ncell(ras)))
  expect_true(all(c("x", "y") %in% colnames(cum)))
})

test_that("directionFromEachPoint gives all pairs, or matched pairs by id", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
  set.seed(2)
  n <- 3L
  coords <- cbind(x = round(stats::runif(n, 0, 20)) + 0.5,
                  y = round(stats::runif(n, 0, 20)) + 0.5)

  ## all pairs
  allPairs <- directionFromEachPoint(coords, landscape = ras)
  expect_true(all(c("x", "y", "angles", "id") %in% colnames(allPairs)))
  expect_identical(nrow(allPairs), as.integer(n * terra::ncell(ras)))
  expect_true(all(is.finite(allPairs[, "angles"])))

  ## an "id" column on both sides matches them up one-for-one instead of
  ## taking the cross product
  from2 <- cbind(x = c(10.5, 10.5, 10.5), y = c(10.5, 10.5, 10.5), id = 1:3)
  to2 <- cbind(x = c(10.5, 11.5, 10.5), y = c(9.5, 10.5, 11.5), id = 1:3)
  matched <- directionFromEachPoint(from = from2, to = to2)

  expect_identical(nrow(matched), 3L)
  expect_true("angles" %in% colnames(matched))
  ## south, east, north -- zero at NORTH here, the opposite of
  ## distanceFromEachPoint(angles = TRUE) above. See the note there.
  expect_equal(unname(matched[, "angles"]), c(pi, pi / 2, 0), tolerance = 1e-8)

  ## and the all-pairs path uses the same convention as the matched one
  focalOnly <- directionFromEachPoint(cbind(x = 10.5, y = 10.5), landscape = ras)
  north <- focalOnly[abs(focalOnly[, "x"] - 10.5) < 1e-9 &
                       abs(focalOnly[, "y"] - 11.5) < 1e-9, "angles"]
  expect_equal(unname(north), 0, tolerance = 1e-8)
})
