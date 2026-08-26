test_that("rasterizeReduced expands a lookup table back onto a raster", {
  testInit(c("terra", "data.table"))

  ras <- terra::rast(terra::ext(0, 4, 0, 4), res = 1)
  terra::values(ras) <- rep(1:4, each = 4)
  tbl <- data.table::data.table(pixelGroup = 1:4, val = c(10, 20, 30, 40))

  out <- rasterizeReduced(tbl, ras, "val", "pixelGroup")

  expect_s4_class(out, "SpatRaster")
  expect_identical(terra::ncell(out), terra::ncell(ras))
  ## each group's cells carry that group's value
  vals <- terra::values(out)[, 1]
  grp <- terra::values(ras)[, 1]
  for (g in 1:4) expect_true(all(vals[grp == g] == tbl$val[tbl$pixelGroup == g]))

  ## a group missing from the table leaves NA rather than a wrong value
  partial <- tbl[pixelGroup %in% c(1, 3)]
  outPartial <- rasterizeReduced(partial, ras, "val", "pixelGroup")
  vp <- terra::values(outPartial)[, 1]
  expect_true(all(is.na(vp[grp %in% c(2, 4)])))
  expect_true(all(!is.na(vp[grp %in% c(1, 3)])))
})

test_that("inRange tests closed intervals and passes NA through", {
  expect_true(inRange(5, 1, 10))
  expect_true(inRange(1, 1, 10))    # closed at the lower bound
  expect_true(inRange(10, 1, 10))   # and the upper
  expect_false(inRange(0.9, 1, 10))
  expect_false(inRange(10.1, 1, 10))

  expect_identical(inRange(c(0, 5, 11), 1, 10), c(FALSE, TRUE, FALSE))
  expect_true(is.na(inRange(NA_real_, 1, 10)))
  expect_null(inRange(NULL, 1, 10))
})

test_that("middlePixel returns an approximately central cell", {
  testInit("terra")

  ## "approximate" is the documented contract -- it is integer arithmetic on
  ## nrow/ncol, not the exact geometric centre -- so assert centrality rather
  ## than an exact coordinate.
  for (dim in c(5, 10, 21, 40)) {
    ras <- terra::rast(terra::ext(0, dim, 0, dim), res = 1)
    mid <- middlePixel(ras)

    expect_true(mid >= 1 && mid <= terra::ncell(ras))
    xy <- terra::xyFromCell(ras, mid)
    ## within one cell of the middle in each axis
    expect_lt(abs(unname(xy[, 1]) - dim / 2), 1.5)
    expect_lt(abs(unname(xy[, 2]) - dim / 2), 1.5)
  }
})

test_that("resampleZeroProof does not choke when probabilities sum to zero", {
  ## sample.int() errors when all probabilities are zero; this exists to
  ## avoid that. It returns nothing rather than sampling impossible draws.
  rzp <- SpaDES.tools:::resampleZeroProof

  set.seed(1)
  got <- rzp(spreadProbHas0 = TRUE, x = 1:10, n = 5, prob = rep(0.5, 10))
  expect_length(got, 5L)
  expect_true(all(got %in% 1:10))

  ## all zero -- no error, and an empty result
  expect_identical(rzp(spreadProbHas0 = TRUE, x = 1:10, n = 3,
                       prob = rep(0, 10)), integer())

  ## the guard is "are there more non-zero probabilities than draws wanted":
  ## 2 non-zero and n = 3 is not, so empty ...
  expect_identical(rzp(spreadProbHas0 = TRUE, x = 1:10, n = 3,
                       prob = c(1, 1, rep(0, 8))), integer())
  ## ... while 5 non-zero and n = 3 samples only from those five
  set.seed(2)
  ok <- rzp(spreadProbHas0 = TRUE, x = 1:10, n = 3,
            prob = c(rep(1, 5), rep(0, 5)))
  expect_length(ok, 3L)
  expect_true(all(ok %in% 1:5))

  ## the no-zeros branch just normalises and samples
  set.seed(3)
  plain <- rzp(spreadProbHas0 = FALSE, x = 1:10, n = 4, prob = rep(2, 10))
  expect_length(plain, 4L)
  expect_true(all(plain %in% 1:10))
})
