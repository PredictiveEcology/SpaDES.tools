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

test_that("option on: spread2 with iterations below the default runs spread2(), resumable", {
  testInit(c("terra", "data.table", "withr"))
  .pkgEnv$spreadCppFallbackMessaged <- NULL
  withr::defer(.pkgEnv$spreadCppFallbackMessaged <- NULL)
  ras <- mkRasOpt()
  start <- c(100L, 1830L)
  off <- withr::with_options(
    list(spades.useSpreadCpp = FALSE),
    withr::with_seed(
      1,
      spread2(ras, start = start, spreadProb = 0.5, iterations = 1, asRaster = FALSE)
    )
  )
  withr::local_options(spades.useSpreadCpp = TRUE)
  expect_message(
    on <- withr::with_seed(
      1,
      spread2(ras, start = start, spreadProb = 0.5, iterations = 1, asRaster = FALSE)
    ),
    "iterations"
  )
  expect_true(any(on$state == "activeSource"))
  expect_identical(on, off)
  more <- withr::with_seed(2, spread2(ras, start = on, spreadProb = 0.5, asRaster = FALSE))
  expect_gt(nrow(more), nrow(on))

  ## the default number of iterations, or more, still goes to spreadCpp()
  ref <- withr::with_seed(1, spreadCpp(ras, start, 0.5, minSizeTries = 0L, jumpTries = 1L))
  dflt <- withr::with_seed(1, spread2(ras, start = start, spreadProb = 0.5, asRaster = FALSE))
  expect_identical(dflt$pixels, ref$indices)
  expect_null(attr(dflt, "spreadState"))
  big <- withr::with_seed(
    1,
    spread2(ras, start = start, spreadProb = 0.5, iterations = Inf, asRaster = FALSE)
  )
  expect_identical(big, dflt)
})

test_that("option on: spread with iterations below the default runs spread(), resumable", {
  testInit(c("terra", "data.table", "withr"))
  .pkgEnv$spreadCppFallbackMessaged <- NULL
  withr::defer(.pkgEnv$spreadCppFallbackMessaged <- NULL)
  ras <- mkRasOpt()
  loci <- c(100L, 1830L)
  off <- withr::with_options(
    list(spades.useSpreadCpp = FALSE),
    withr::with_seed(
      1,
      spread(ras, loci = loci, spreadProb = 0.5, iterations = 1, returnIndices = TRUE)
    )
  )
  withr::local_options(spades.useSpreadCpp = TRUE)
  expect_message(
    on <- withr::with_seed(
      1,
      spread(ras, loci = loci, spreadProb = 0.5, iterations = 1, returnIndices = TRUE)
    ),
    "iterations"
  )
  expect_true(any(on$active))
  expect_identical(on, off)
  more <- withr::with_seed(2, spread(ras, spreadProb = 0.5, spreadState = on, returnIndices = TRUE))
  expect_gt(nrow(more), nrow(on))

  ## the default number of iterations still goes to spreadCpp()
  ref <- withr::with_seed(1, spreadCpp(ras, loci, 0.5))
  dflt <- withr::with_seed(1, spread(ras, loci = loci, spreadProb = 0.5, returnIndices = TRUE))
  expect_identical(dflt, ref)
})

test_that("option on: an NA maxSize or exactSize means no size limit, as without the option", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = TRUE)
  ras <- mkRasOpt()
  start <- c(100L, 1830L)
  sp2 <- function(...) {
    withr::with_seed(1, spread2(ras, start = start, spreadProb = 0.25, asRaster = FALSE, ...))
  }
  uncapped <- sp2()
  expect_null(attr(uncapped, "spreadState"))
  expect_true(all(uncapped[, .N, by = "initialPixels"]$N > 5))
  expect_identical(sp2(maxSize = NA), uncapped)
  expect_identical(sp2(maxSize = NA_real_), uncapped)
  ## spread2() lifts the cap for every event when any maxSize is NA
  expect_identical(sp2(maxSize = c(5, NA)), uncapped)
  expect_identical(sp2(exactSize = NA_real_), uncapped)

  sp <- function(...) {
    withr::with_seed(1, spread(ras, loci = start, spreadProb = 0.25, returnIndices = TRUE, ...))
  }
  expect_identical(sp(maxSize = NA), sp())
})

test_that("option on: a fractional size is rounded down, as without the option", {
  testInit(c("terra", "data.table", "withr"))
  withr::local_options(spades.useSpreadCpp = TRUE)
  ras <- mkRasOpt()
  start <- c(100L, 1830L)
  ## spreadProb = 1 fills every fire to its size
  sizes2 <- function(...) {
    out <- withr::with_seed(1, spread2(ras, start = start, spreadProb = 1, asRaster = FALSE, ...))
    out[, .N, by = "initialPixels"]$N
  }
  expect_identical(sizes2(maxSize = 30.5), c(30L, 30L))
  expect_identical(sizes2(exactSize = 30.5), c(30L, 30L))
  sizes <- function(...) {
    out <- withr::with_seed(1, spread(ras, loci = start, spreadProb = 1, returnIndices = TRUE, ...))
    out[, .N, by = "id"]$N
  }
  expect_identical(sizes(maxSize = 30.5), c(30L, 30L))
  expect_identical(sizes(maxSize = 30.5, exactSizes = TRUE), c(30L, 30L))
})
