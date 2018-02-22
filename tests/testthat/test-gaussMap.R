test_that("gaussMap does not work", {
  library(raster)

  on.exit({
    detach("package:raster")
  }, add = TRUE)

  a <- raster(extent(0, 1e1, 0, 1e1), res = 1)
  theMessage <- capture_messages(hab <- gaussMap(a, speedup = 10, method = "RMexp2") )
  expect_true(startsWith(theMessage, "That method is not yet implemented, defaulting to RMexp"))

  theMessage <- capture_messages(hab <- gaussMap(a, speedup = 10, method = "RMexp") )
  expect_true(length(theMessage)==0)

  theMessage <- capture_messages(hab <- gaussMap(a, speedup = 10, method = "RMgauss") )
  expect_true(length(theMessage)==0)
  theMessage <- capture_messages(hab <- gaussMap(a, speedup = 10, method = "RMstable") )
  expect_true(length(theMessage)==0)
  expect_error(hab <- gaussMap(a, speedup = 10, method = "RMstable", alpha = 3))
  expect_error(hab <- gaussMap(a, speedup = 10, method = RMexp))
  expect_error(hab <- gaussMap(a, var = -1))
})
