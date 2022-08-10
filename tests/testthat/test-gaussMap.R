test_that("gaussMap does not work", {
  skip_if_not_installed("RandomFields", "3.1.24")

  a <- raster::raster(raster::extent(0, 1e1, 0, 1e1), res = 1)
  theMessage <- capture_messages({
    hab <- gaussMap(a, speedup = 10, method = "RMexp2")
  })
  expect_true(startsWith(theMessage, "method is not yet implemented, defaulting to RMexp"))

  expect_silent(gaussMap(a, speedup = 10, method = "RMexp"))
  expect_silent(gaussMap(a, speedup = 10, method = "RMgauss"))
  expect_silent(gaussMap(a, speedup = 10, method = "RMstable"))

  expect_error({
    hab <- gaussMap(a, speedup = 10, method = "RMstable", alpha = 3)
  })
  expect_error({
    hab <- gaussMap(a, speedup = 10, method = RMexp)
  })
  expect_error({
    hab <- gaussMap(a, var = -1)
  })
})
