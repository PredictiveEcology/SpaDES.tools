## options(spades.useSpreadCpp = TRUE) hands spread() and spread2() calls to
## spreadCpp(), converting arguments and return values.

mkRasOpt <- function(side = 60L) {
  terra::rast(terra::ext(0, side, 0, side), resolution = 1, vals = 0)
}

test_that("spreadCpp accepts a raster spreadProb, same as its values", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRasOpt()
  withr::local_seed(2)
  sp <- terra::setValues(ras, runif(terra::ncell(ras), 0, 0.3))
  a <- withr::with_seed(1, spreadCpp(ras, c(100L, 2000L), sp, maxSize = 50))
  b <- withr::with_seed(1, spreadCpp(ras, c(100L, 2000L), terra::values(sp, mat = FALSE),
                                     maxSize = 50))
  expect_identical(a, b)
})

test_that("option off: spread2 runs its own algorithm", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = FALSE)
  out <- spread2(mkRasOpt(), start = c(100L, 2000L), spreadProb = 0.2, asRaster = FALSE)
  expect_false(is.null(attr(out, "spreadState")))
})

test_that("option on: spread2 exactSize goes to spreadCpp, in spread2's shape", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = TRUE)
  ras <- mkRasOpt()
  start <- c(100L, 2000L, 3000L)
  sz <- c(20, 100, 300)
  out <- withr::with_seed(1, spread2(ras, start = start, spreadProb = 0.2, exactSize = sz,
                                     maxRetriesPerID = 20, asRaster = FALSE,
                                     allowOverlap = FALSE))
  ref <- withr::with_seed(1, spreadCpp(ras, start, 0.2, maxSize = sz, minSize = sz,
                                       minSizeTries = 0L, jumpTries = 2L))
  expect_identical(names(out), c("initialPixels", "pixels", "state"))
  expect_identical(out$pixels, ref$indices)
  expect_identical(out$initialPixels, ref$initialLocus)
  expect_true(all(out$state == "inactive"))
  expect_identical(out[, .N, by = "initialPixels"][match(start, initialPixels), N],
                   as.integer(sz))

  r <- withr::with_seed(1, spread2(ras, start = start, spreadProb = 0.2, exactSize = sz,
                                   maxRetriesPerID = 20))
  expect_s4_class(r, "SpatRaster")
  expect_identical(attr(r, "pixel"), out)
  expect_identical(as.integer(terra::values(r, mat = FALSE)[ref$indices]), ref$id)
})

test_that("option on: spread goes to spreadCpp for each return type", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = TRUE)
  ras <- mkRasOpt()
  loci <- c(100L, 2000L)
  ref <- withr::with_seed(1, spreadCpp(ras, loci, 0.2, maxSize = c(30, 60)))

  out <- withr::with_seed(1, spread(ras, loci = loci, spreadProb = 0.2, maxSize = c(30, 60),
                                    returnIndices = TRUE))
  expect_identical(out, ref)
  wh <- withr::with_seed(1, spread(ras, loci = loci, spreadProb = 0.2, maxSize = c(30, 60),
                                   returnIndices = 2))
  expect_identical(wh, ref$indices)
  r <- withr::with_seed(1, spread(ras, loci = loci, spreadProb = 0.2, maxSize = c(30, 60),
                                  id = TRUE))
  v <- terra::values(r, mat = FALSE)
  expect_identical(as.integer(v[ref$indices]), ref$id)
  expect_identical(sum(v > 0), nrow(ref))

  ex <- withr::with_seed(1, spread(ras, loci = loci, spreadProb = 0.2, maxSize = c(30, 60),
                                   exactSizes = TRUE, returnIndices = TRUE))
  expect_identical(ex[, .N, by = "id"]$N, c(30L, 60L))
})

test_that("option on: an unsupported argument falls back, with one message", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = TRUE)
  .pkgEnv$spreadCppFallbackMessaged <- NULL
  withr::defer(.pkgEnv$spreadCppFallbackMessaged <- NULL)
  ras <- mkRasOpt()
  expect_message(
    out <- spread2(ras, start = 1830L, spreadProb = 0.2, asymmetry = 2,
                   asymmetryAngle = 90, asRaster = FALSE),
    "asymmetry"
  )
  expect_false(is.null(attr(out, "spreadState")))
  expect_silent(spread(ras, loci = 1830L, spreadProb = 0.2, persistence = 0.1,
                       returnIndices = TRUE))
})
