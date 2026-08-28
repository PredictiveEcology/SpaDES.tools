test_that("randomStudyArea is reproducible and respects the requested size", {
  testInit("terra")

  a <- randomStudyArea(seed = 123)
  expect_s4_class(a, "SpatVector")
  expect_identical(terra::geomtype(a), "polygons")
  expect_false(is.na(terra::crs(a, proj = TRUE)))

  ## the same seed gives the same study area ...
  expect_equal(terra::crds(a), terra::crds(randomStudyArea(seed = 123)))

  ## ... and a different one does not
  b <- randomStudyArea(seed = 456)
  expect_false(isTRUE(all.equal(terra::crds(a), terra::crds(b))))

  ## `size` is honoured, approximately
  small <- randomStudyArea(size = 1e4, seed = 321)
  large <- randomStudyArea(size = 1e8, seed = 321)
  expect_lt(terra::expanse(small), terra::expanse(large))
})

test_that("randomStudyArea does not disturb the caller's RNG stream", {
  ## it sets a seed internally, so it must restore what it found
  set.seed(11)
  before <- .Random.seed
  invisible(randomStudyArea(seed = 999))
  expect_identical(.Random.seed, before)
})
