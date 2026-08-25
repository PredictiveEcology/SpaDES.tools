test_that("wrapTorus wraps coordinates back inside the bounds", {
  testInit("terra")

  hab <- terra::rast(terra::ext(c(-50, 50, -50, 50)))
  hab[] <- 0

  ## a point 10 beyond each edge comes back 10 inside the opposite edge
  outside <- cbind(x = c(-60, 60, 0, 0), y = c(0, 0, -60, 60))
  wrapped <- wrapTorus(outside, bounds = terra::ext(hab))

  expect_true(is.matrix(wrapped))
  expect_equal(unname(wrapped[, 1]), c(40, -40, 0, 0))
  expect_equal(unname(wrapped[, 2]), c(0, 0, 40, -40))

  ## points already inside are left alone
  inside <- cbind(x = c(-10, 10), y = c(5, -5))
  expect_equal(wrapTorus(inside, bounds = terra::ext(hab)), inside)
})

test_that("wrapTorus handles SpatVector input", {
  testInit("terra")

  hab <- terra::rast(terra::ext(c(-50, 50, -50, 50)))
  hab[] <- 0
  ## a SpatVector carrying the previous location, as crw() supplies
  v <- terra::vect(data.frame(x1 = 0, y1 = 0,
                              x = c(-60, 60), y = c(0, 0)), geom = c("x", "y"))

  got <- wrapTorus(v, bounds = terra::ext(hab))
  expect_s4_class(got, "SpatVector")
  expect_equal(unname(terra::crds(got)[, 1]), c(40, -40))
})

test_that("wrap() is deprecated in favour of wrapTorus()", {
  testInit("terra")

  hab <- terra::rast(terra::ext(c(-50, 50, -50, 50)))
  hab[] <- 0
  outside <- cbind(x = 60, y = 0)

  ## Namespace-qualified deliberately: attaching terra masks this package's
  ## wrap() with terra::wrap(), which is the whole reason for the rename.
  expect_warning(got <- SpaDES.tools::wrap(outside, bounds = terra::ext(hab)),
                 "deprecated")
  expect_equal(got, wrapTorus(outside, bounds = terra::ext(hab)))
})

test_that("the reason for the rename still holds: terra::wrap masks ours", {
  testInit("terra")

  ## If this ever stops being true, the deprecation note should be revisited.
  ## terra::wrap() serialises a SpatRaster/SpatVector; ours wraps a torus.
  expect_true(identical(wrap, terra::wrap))
  expect_false(identical(SpaDES.tools::wrapTorus, terra::wrap))
})

test_that("crw(torus = TRUE) does not emit the wrap deprecation", {
  testInit("terra")

  set.seed(3)
  starts <- cbind(x = stats::runif(20, -50, 50), y = stats::runif(20, -50, 50))
  ## crw() calls wrapTorus() directly, so no deprecation reaches the user
  expect_no_warning(
    crw(starts, extent = terra::ext(-50, 50, -50, 50),
        stepLength = 30, stddev = 20, torus = TRUE)
  )
})
