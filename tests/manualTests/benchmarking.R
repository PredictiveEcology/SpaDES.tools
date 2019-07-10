if (interactive()) {
  library(microbenchmark)
  library(profvis)
}

test_that("spread benchmarking", {
  skip("This is just benchmarking, not testing")

  library(raster)

  on.exit({
    detach("package:raster")
  }, add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50

  library(microbenchmark)
  initialLoci <- as.integer(sample(1:ncell(hab), 3))
  maxVal <- 20
  stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal

  microbenchmark(times = 200,
                 excludePixel = spread(hab, spreadProb = 1, circle = TRUE,
                                       loci = initialLoci, stopRule = stopRule2,
                                       id = TRUE, allowOverlap = TRUE,
                                       stopRuleBehavior = "excludePixel",
                                       maxVal = maxVal, returnIndices = TRUE),
                 excludeRing = spread(hab, spreadProb = 1, circle = TRUE,
                                      loci = initialLoci, stopRule = stopRule2,
                                      id = TRUE, allowOverlap = TRUE,
                                      stopRuleBehavior = "excludeRing",
                                      maxVal = maxVal, returnIndices = TRUE),
                 includePixel = spread(hab, spreadProb = 1, circle = TRUE,
                                       loci = initialLoci, stopRule = stopRule2,
                                       id = TRUE, allowOverlap = TRUE,
                                       stopRuleBehavior = "includePixel",
                                       maxVal = maxVal, returnIndices = TRUE),
                 includeRing = spread(hab, spreadProb = 1, circle = TRUE,
                                      loci = initialLoci, stopRule = stopRule2,
                                      id = TRUE, allowOverlap = TRUE,
                                      stopRuleBehavior = "includeRing",
                                      maxVal = maxVal, returnIndices = TRUE)
  )
  #Unit: milliseconds # with data.table
  #expr              min       lq     mean   median       uq       max neval
  #excludePixel 38.90842 41.71832 45.00469 44.33181 47.14418  62.58632   100
  #excludeRing  22.63004 23.80755 26.63432 25.76519 27.95789  41.47834   100
  #includePixel 38.46955 42.51963 48.04159 44.32482 47.41415 333.52346   100
  #includeRing  33.72337 36.62840 39.55411 38.71796 41.31295  63.29517   100

  # Remove data.table
  # Unit: milliseconds
  # expr              min       lq     mean   median       uq      max neval
  # excludePixel 27.31582 29.57508 33.80717 32.51402 35.86901 85.20527   200
  # excludeRing  15.59501 16.21633 19.32698 17.73696 20.59371 60.33322   200
  # includePixel 27.43088 29.19868 33.27228 31.67183 34.27935 94.79831   200
  # includeRing  22.76565 24.52749 27.56035 26.56609 29.32072 49.58507   200

  includePixel <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                         stopRule = stopRule2, id = TRUE, allowOverlap = TRUE,
                         stopRuleBehavior = "includePixel",
                         maxVal = maxVal, returnIndices = TRUE)

  ## Make distance surface
  maxRadius <- 10
  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                  id = TRUE, circleMaxRadius = maxRadius, allowOverlap = TRUE)
  clumps <- raster::clump(circs)
  bounds <- raster::boundaries(clumps, classes = TRUE, type = "outer")
  spreadProb <- raster(clumps)
  spreadProb[] <- 1
  spreadProb[clumps == 1 & bounds == 0] <- 0

  clumps[is.na(clumps)] <- 0
  if (interactive()) Plot(clumps, new = TRUE, zero.color = "white", cols = "Reds")

  whCells <- which(bounds[] > 0)
  xy <- xyFromCell(circs, whCells)
  microbenchmark(times = 2,
                 dists = spread(circs, loci = whCells, spreadProb = spreadProb,
                                id = FALSE, circle = TRUE, allowOverlap = TRUE,
                                iterations = 20, directions = 8, returnIndices = FALSE),
                 dists2 = distanceFromPoints(circs, xy = xy))
  if (interactive()) Plot(dists, dists2, new = TRUE)

  library(raster); on.exit(detach("package:raster"), add = TRUE)

  a <- raster(extent(0, 436, 0, 296), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxVal <- 250
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape) > maxVal
  microbenchmark(stopRuleA <- spread(hab, loci = startCells, 1, 0,
                                     NULL, maxSize = 1e6, 8, 1e6, id = TRUE,
                                     circle = TRUE, stopRule = stopRule1))
  if (interactive()) Plot(stopRuleA, new = TRUE)

  # Internal conversion to vector -- almost 3x faster
  #     min       lq     mean   median       uq      max neval
  #34.91276 35.26146 37.86623 35.81296 40.09197 61.20151    50

  # Keep as raster
  #     min       lq     mean   median       uq      max neval
  #97.65601 102.6857 118.7154 115.3167 126.9112 173.6077    50

  # ARbitrary stopRule is much slower than sizes -- 5x for this example
  a <- raster(extent(0, 100, 0, 100), res = 1)
  a[] <- 1
  sizes <- rep(3300, 3)
  set.seed(343)
  microbenchmark(times = 100, maxSize = spread(a, loci = c(100, 3500, 8000),
                                               spreadProb = 1, id = TRUE,
                                               maxSize = sizes))
  set.seed(343)
  microbenchmark(times = 20,
                 stopRule = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                   stopRuleBehavior = "excludePixel", id = TRUE,
                                   stopRule = function(cells, id) length(cells) > sizes[id]))
  # Unit: milliseconds
  #    expr      min       lq     mean   median       uq     max neval
  # maxSize 35.44505 37.38652 41.36509  38.2398 47.60329 84.6241   100
  #stopRule 102.4452 205.5738 216.8115 211.6224  214.788  451.72    20

  quality <- raster(hab)
  quality[] <- runif(ncell(quality), 0, 1)
  stopRule4 <- function(landscape, quality, cells) {
    (sum(landscape) > 200) | (mean(quality[cells]) < 0.5)
  }
  set.seed(23432)
  microbenchmark(circs <- spread(hab, spreadProb = 1, loci = initialLoci,
                                 circle = TRUE, directions = 8, id = TRUE,
                                 stopRule = stopRule4, quality = quality,
                                 stopRuleBehavior = "includeRing"), times = 50)

  # ARbitrary stopRule is much slower than sizes -- 5x for this example
  a <- raster(extent(0, 300, 0, 300), res = 1)
  a[] <- 1
  sizes <- rep(6600, 3)
  set.seed(343)
  microbenchmark(times = 20, maxSize = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                              id = TRUE, maxSize = sizes))
  set.seed(343)
  microbenchmark(times = 20,
                 stopRule = spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                                   stopRuleBehavior = "excludePixel", id = TRUE,
                                   stopRule = function(cells, id) length(cells) > sizes[id]))
  # With 300x300 raster and 6600 sizes
  # maxSizes
  # Unit: milliseconds
  #     expr      min       lq     mean   median       uq      max neval
  # maxSize  50.39086 54.11104 74.02115  57.60774 101.3887 129.1427    20
  # stopRule  423.923  470.764 552.4836  521.938  594.7501 886.5732    20
  library(profvis)
  on.exit({
    detach("package:profvis", unload = TRUE)
  }, add = TRUE)

  pv <- profvis(spread(a, loci = c(100, 3500, 8000), spreadProb = 1,
                       stopRuleBehavior = "excludePixel", id = TRUE,
                       stopRule = function(cells, id) length(cells) > sizes[id]))
  pv

  microbenchmark(times = 10, long = {
    ord <- order(foo[, "id"])
    foo1 <- foo[ord, ]
    ids <- unique(foo1[, "id"])
    fooB <- unlist(lapply(ids, function(id) {
      duplicated(foo1[foo1[, "id"] == id, "indices"])
    }))
  }, short = {
    fooA <- unlist(tapply(foo1[, "indices"], foo1[, "id"], duplicated))
  })

})


##############################################################
test_that("spread2 benchmarking", {
  skip("This is just benchmarking, not testing")
  skip("benchmarking spread2")
  exactSizes <- 60:61
  microbenchmark(
    times = 10,
    a = {
      out <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                     exactSize = exactSizes, asRaster = FALSE)
      for (i in 1:25) {
        out <- spread2(a, start = out, spreadProb = 0.225, iterations = 1,
                       exactSize = exactSizes, asRaster = FALSE)
      }
    },
    b = {
      out2 <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                      exactSize = exactSizes, asRaster = FALSE)
      for (i in 1:25) {
        attr(out2, "spreadState") <- NULL
        out2 <- spread2(a, start = out2, spreadProb = 0.225, iterations = 1,
                        exactSize = exactSizes, asRaster = FALSE)
      }
    }
  )

  a <- raster(extent(0, 1000, 0, 1000), res = 1)
  set.seed(123)
  sams <- sample(innerCells, 30)
  set.seed(123)
  profvis::profvis({
    out <- spread2(a, start = sams, spreadProb = 0.235, asRaster = FALSE)
  })
  set.seed(123)
  profvis::profvis({
    out <- spread2(a, start = sams, spreadProb = 0.235, asRaster = FALSE, allowOverlap = TRUE)
  })

  set.seed(123)
  microbenchmark(times = 30, {
    out1 <- spread2(a, start = sams, spreadProb = 0.235, asRaster = FALSE)
  },
  b = {
    out2 <- spread(a, loci = sams, spreadProb = 0.235, id = TRUE)
  },
  c = {
    out2 <- spread(a, loci = sams, spreadProb = 0.235, id = TRUE, lowMemory = TRUE)
  })
  set.seed(123)
  profvis::profvis({
    out <- spread2(a, start = sams, spreadProb = 0.235, asRaster = FALSE, allowOverlap = TRUE)
  })

  ######## Benchmarking ##########
  iterativeFun <- function(a, skipChecks, n, sp) {
    sams <- sample(innerCells, n)
    out <- spread2(a, iterations = 1, start = sams, skipChecks = skipChecks,
                   asRaster = FALSE, spreadProb = sp)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$state == "activeSource")
      out <- spread2(a, spreadProb = sp, iterations = 1, start = out,
                     asRaster = FALSE, skipChecks = skipChecks)
    }
    out
  }

  nonIterativeFun <- function(a, skipChecks, n, sp) {
    sams <- sample(innerCells, n)
    out <- spread2(a, start = sams, asRaster = FALSE, skipChecks = skipChecks, spreadProb = sp)
    out
  }

  origSpread <- function(a, quick, n, sp) {
    sams <- sample(innerCells, n)
    out <- spread(a, spreadProb = sp, loci = sams, id = TRUE, returnIndices = TRUE,
                  quick = quick)
    out
  }

  origSpreadIterations <- function(a, quick, n, sp) {
    sams <- sample(innerCells, n)
    out <- spread(a, spreadProb = sp, iterations = 1, loci = sams, returnIndices = TRUE)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$active)
      out <- spread(a, spreadProb = sp, iterations = 1, spreadState = out,
                    returnIndices = TRUE, quick = quick)
    }
    out
  }

  n <- 2
  ras <- raster(extent(0, 160, 0, 160), res = 1)
  n <- 2000
  ras <- raster(extent(0, 6000, 0, 6000), res = 1)
  sp <- 0.225
  b <- raster(ras)
  b[] <- 1
  bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
  innerCells <- which(bb[] %==% 1)

  microbenchmark(
    times = 3,
    iterativeFun(ras, TRUE, n, sp),
    nonIterativeFun(ras, TRUE, n, sp),
    origSpread(ras, TRUE, n, sp),
    origSpreadIterations(ras, TRUE, n, sp)
  )
  #         iterativeFun(ras, TRUE, n, sp) 2.581752 2.667209 2.727886 2.752666 2.800953 2.849239
  #      nonIterativeFun(ras, TRUE, n, sp) 2.009914 2.268422 2.486540 2.526930 2.724854 2.922777
  #           origSpread(ras, TRUE, n, sp) 1.003594 1.043065 1.085511 1.082536 1.126470 1.170404
  # origSpreadIterations(ras, TRUE, n, sp) 7.267927 7.630435 7.908982 7.992943 8.229510 8.46607
  #
  # without "skipChecks"
  microbenchmark(
    times = 100,
    iterativeFun(ras, FALSE, n, sp),
    nonIterativeFun(ras, FALSE, n, sp),
    origSpread(ras, FALSE, n, sp),
    origSpreadIterations(ras, FALSE, n, sp)
  )
  ## Unit: milliseconds
  ##                                    expr       min        lq      mean   median        uq       max neval
  ##         iterativeFun(ras, FALSE, n, sp)  3.096979 12.642477  73.02248 35.17520  91.10528  764.4073   300
  ##      nonIterativeFun(ras, FALSE, n, sp)  1.509484  6.555565  31.18444 14.91066  42.78317  158.5237   300
  ##           origSpread(ras, FALSE, n, sp)  5.154006  7.555631  14.87158 11.49005  17.50599  231.5487   300
  ## origSpreadIterations(ras, FALSE, n, sp) 10.754669 51.524368 141.48620 93.61996 169.10808 2110.2683   300
  ##
  profvis::profvis({
    set.seed(3451)
    for (i in 1:8)
      iterativeFun(ras, TRUE, n, sp = sp)
  })
  profvis::profvis({
    nonIterativeFun()
  })

  library(raster)
  library(data.table)
  library(fpCompare)
  n <- 2
  ras <- raster(extent(0, 160, 0, 160), res = 1)
  sp <- 0.225
  b <- raster(ras)
  b[] <- 1
  bb <-
    focal(
      b,
      matrix(1 / 9, nrow = 3, ncol = 3),
      fun = sum,
      pad = TRUE,
      padValue = 0
    )
  innerCells <- which(bb[] %==% 1)

  iterativeNeigh <- function(a, skipChecks, n, sp, exactSizes,
                             neighProbs, ...) {
    sams <- sample(innerCells, n)
    out <- spread2(a, iterations = 1, spreadProb = sp, start = sort(sams),
                   neighProbs = neighProbs, exactSize = exactSizes,
                   skipChecks = skipChecks, asRaster = FALSE, ...)
    stillActive <- TRUE
    while (stillActive) {
      out <- spread2(a, iterations = 1, spreadProb = sp, start = out,
                     neighProbs = neighProbs, exactSize = exactSizes,
                     skipChecks = skipChecks, asRaster = FALSE, ...)
      stillActive <- any(length(attr(out, "spreadState")$whActive) |
                           length(attr(out, "spreadState")$whNeedRetry))
    }
    out
  }

  origSpreadIterationsNeighs <- function(a, quick, n, sp, neighProbs, exactSize) {
    sams <- sample(innerCells, n)
    out <- spread(a, spreadProb = sp, neighProbs = neighProbs, iterations = 1,
                  loci = sams, exactSizes = TRUE, returnIndices = TRUE, maxSize = exactSize)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$active)
      out <- spread(a, spreadProb = sp, iterations = 1, neighProbs = neighProbs,
                    spreadState = out, maxSize = exactSize, exactSizes = TRUE,
                    returnIndices = TRUE, quick = quick)
    }
    out
  }

  sp <- randomPolygons(ras, numTypes = 35)
  sp[] <- (sp[] %% 5 + 1) / 10
  exactSizes <- c(123, 2240)
  neighProbs <- c(0.5, 0.3, 0.2)

  microbenchmark(
    times = 9,
    a = {
      iterativeNeigh(ras, TRUE, length(exactSizes), sp = sp, exactSize = exactSizes,
                     neighProbs = neighProbs)
    },
    b = {
      origSpreadIterationsNeighs(ras, TRUE, length(exactSizes), sp = sp,
                                 exactSize = exactSizes, neighProbs = neighProbs)
    }
  )

  profvis::profvis({
    for (i in 1:5) {
      iterativeNeigh(ras, TRUE, length(exactSizes), sp = sp, exactSize = exactSizes,
                     neighProbs = neighProbs)
    }
  })

  profvis::profvis({
    for (i in 1:5) {
      origSpreadIterationsNeighs(ras, TRUE, length(exactSizes), sp = sp,
                                 exactSize = exactSizes, neighProbs = neighProbs)
    }
  })

  dev()
  iterativeNeigh(ras, TRUE, length(exactSizes), sp = sp,  exactSize = exactSizes, plot.it = TRUE)
  iterativeNeigh(ras, TRUE, length(exactSizes), sp = sp)#,  plot.it = TRUE)
  iterativeNeigh(ras, TRUE, length(exactSizes), sp = sp)#,  plot.it = TRUE)


  # compare original spread and spread2 -- seems pretty dead on
  nn <- 1000
  outNew <- out <- numeric(nn)
  for (i in 1:nn) {
    outNew[i] <- NROW(nonIterativeFun(ras, TRUE, n, sp))
    out[i] <- NROW(origSpread(ras, TRUE, n, sp))
  }

  out <- data.table(x = out)
  outNew <- data.table(x = outNew)
  mean(out$x)
  mean(outNew$x)
  sd(out$x)
  sd(outNew$x)

  n <- 5
  ras <- raster(extent(0, 1000, 0, 1000), res = 1)
  sp <- 0.295
  set.seed(123)
  microbenchmark(
    times = 100,
    nonIterativeFun(ras, TRUE, n, sp)
  )


})


test_that("rings and cir", {
  library(sp)
  library(raster)
  library(fpCompare)
  library(data.table)
  library(quickPlot)

  on.exit({
    detach("package:quickPlot")
    detach("package:data.table")
    detach("package:fpCompare")
    detach("package:raster")
    detach("package:sp")
  }, add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  n <- 2
  caribou <- SpatialPoints(coords = cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                                          y = stats::runif(n, xmin(hab), xmax(hab))))

  radius <- 15
  cirsEx <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius, simplify = TRUE,
                includeBehavior = "excludePixels")
  cirsIncl <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius, simplify = TRUE,
                  includeBehavior = "includePixels")

  expect_true(NROW(cirsEx) < NROW(cirsIncl))

  # With including pixels, then distances are not strictly within the bounds of minRadius
  #   and maxRadius, because every cell is included if it has a point anywhere within
  #   the cell, causing cells whose centres are beyond maxRadius or shorter than minRadius
  #   to be accepted
  b <- cbind(coordinates(caribou), id = seq_along(caribou))
  a <- as.matrix(cirsIncl)
  colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
  dists <- distanceFromEachPoint(b, a)
  expect_true(radius * 1.5 < max(dists[, "dists"]))
  expect_true(radius > min(dists[, "dists"]))

  # With excluding pixels, then distances are strictly within the bounds
  b <- cbind(coordinates(caribou), id = seq_along(caribou))
  a <- as.matrix(cirsEx)
  colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
  dists <- distanceFromEachPoint(b, a)
  expect_true((radius * 1.5) %>=% max(dists[, "dists"]))
  expect_true(radius %<=% min(dists[, "dists"]))

  ras1 <- raster(hab)
  ras1[] <- 0
  cirsOverlap <- data.table(cirsEx)[, list(sumIDs = sum(id)), by = indices]
  ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
  if (interactive()) {
    clearPlot()
    Plot(ras1)
  }

  ras3 <- raster(hab)
  ras3[] <- 0
  cirsOverlap <- data.table(cirsIncl)[, list(sumIDs = sum(id)), by = indices]
  ras3[cirsOverlap$indices] <- cirsOverlap$sumIDs
  ras3 <- ras1 * 10 + ras3
  if (interactive()) Plot(ras3)
  expect_true(all(getValues(ras3) != 10)) # None should have only ras1, i.e., only circEx cells
  expect_true(all(getValues(ras3) != 20)) # None should have only ras1, i.e., only circEx cells

  cirsExSkinny <- data.table(cir(hab, caribou, maxRadius = radius, simplify = TRUE,
                                 includeBehavior = "excludePixels"))
  expect_true(NROW(cirsExSkinny) == 0)

  # Compare rings and cir -- if start in centre of cells, then should be identical
  n <- 2
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5))

  loci <- cellFromXY(hab, coordinates(caribou)[1, ])
  cirs <- data.table(cir(hab, caribou[1, ], maxRadius = radius * 1.5001, minRadius = radius,
                         simplify = TRUE, allowOverlap = TRUE,
                         includeBehavior = "excludePixels", returnDistances = TRUE))
  cirs2 <- rings(hab, loci, minRadius = radius, maxRadius = radius * 1.5001,
                 allowOverlap = TRUE, returnIndices = TRUE, includeBehavior = "includeRing")

  expect_true(all.equal(range(cirs$dists), range(cirs2$dists)))
  setkey(cirs2, dists, indices)
  setkey(cirs,  dists, indices)
  ras1 <- raster(hab)
  ras1[] <- 0
  cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
  ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
  if (interactive()) Plot(ras1, new = TRUE)

  ras2 <- raster(hab)
  ras2[] <- 0
  cirsOverlap2 <- cirs2[, list(sumIDs = sum(id)), by = indices]
  ras2[cirsOverlap2$indices] <- cirsOverlap2$sumIDs
  ras3 <- ras1 - ras2
  if (interactive()) Plot(ras2, ras3, zero.color = "transparent")
  expect_equal(0, sum(abs(getValues(ras3))))

  n <- 2
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5))

  loci <- cellFromXY(hab, coordinates(caribou))
  dists1 <- rings(hab, loci, minRadius = 0, maxRadius = ncol(hab), returnDistances = TRUE,
                  includeBehavior = "includeRing")
  dists2 <- distanceFromPoints(hab, coordinates(caribou))
  dists3 <- cir(landscape = hab, loci = loci, minRadius = 0, maxRadius = ncol(hab),
                includeBehavior = "includePixels", allowOverlap = FALSE,
                returnIndices = FALSE, closest = TRUE, returnDistances = TRUE)
  if (interactive()) Plot(dists1, dists2, dists3, new = TRUE)
  diffDists12 <- abs(dists1 - dists2)
  diffDists23 <- abs(dists2 - dists3)
  tabs12 <- table(round(getValues(diffDists12)))
  tabs23 <- table(round(getValues(diffDists23)))

  # This tests that the two approaches are 99% similar
  expect_true(tabs12[names(tabs12) == 0] / ncell(diffDists12) > 0.99)
  expect_true(tabs23[names(tabs23) == 0] / ncell(diffDists23) > 0.99)

  if (interactive()) Plot(diffDists12, diffDists23, new = TRUE)

  skip("Below here is just benchmarking, not testing")

  library(microbenchmark)
  on.exit({
    detach("package:microbenchmark", detach = TRUE)
  }, add = TRUE) # nolint

  microbenchmark(
    times = 10,
    dists1 <- rings(hab, loci, minRadius = 0, maxRadius = ncol(hab),
                    returnDistances = TRUE, includeBehavior = "includeRing"),
    dists2 <- distanceFromPoints(hab, coordinates(caribou)),
    dists3 <- cir(landscape = hab, loci = loci, minRadius = 0, maxRadius = ncol(hab),
                  includeBehavior = "includePixels", allowOverlap = FALSE,
                  returnIndices = FALSE, closest = TRUE, returnDistances = TRUE)
  )
})

test_that("distanceFromPoints does not work correctly", {
  library(raster)
  library(quickPlot)
  library(fpCompare)
  library(data.table)

  on.exit({
    detach("package:data.table")
    detach("package:fpCompare")
    detach("package:quickPlot")
    detach("package:raster")
  }, add = TRUE)

  hab <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(hab, speedup = 1) # if raster is large (>1e6 pixels), use speedup > 1
  names(hab) <- "hab"
  n <- 1
  coords <- cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                  y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5)
  distsDFP1Pt <- distanceFromPoints(hab, coords[1, , drop = FALSE])
  distsDFEP1Pt <- distanceFromEachPoint(coords[1, , drop = FALSE], landscape = hab)
  ras1 <- raster(hab)
  ras1[] <- distsDFEP1Pt[, "dists"]
  expect_identical(0, unique(round(getValues(distsDFP1Pt - ras1), 7)))
  if (interactive()) Plot(distsDFP1Pt, ras1, new = TRUE)

  maxDistance <- 30
  distsDFEPMaxD <- dists6 <- distanceFromEachPoint(coords, landscape = hab,
                                                   maxDistance = maxDistance)

  # test that maxDistance arg is working
  expect_true(round(max(distsDFEPMaxD[, "dists"]), 7) == maxDistance)

  # evaluate cumulativeFn
  n <- 5
  hab <- raster(extent(0, 10, 0, 10), res = 1)
  coords <- cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                  y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5)
  dfep20 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab)
  idw <- tapply(dfep20[, c("dists")], cellFromXY(hab, dfep20[, c("x", "y")]), function(x) {
    sum(1 / (1 + x))
  })
  dfep <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                landscape = hab, cumulativeFn = `+`)
  expect_true(sum(idw - dfep[, "val"]) %==% 0)

  skip("this is currently only benchmarking")

  library(microbenchmark); on.exit(detach("package:microbenchmark"), add = TRUE)

  loci <- cellFromXY(hab, xy = coords)
  distsCir <-  dists7 <- cir(coords, landscape = hab, maxRadius = 30,
                             minRadius = 0, returnDistances = TRUE)
  distsRings <-  dists8 <- rings(loci = loci, landscape = hab, maxRadius = 30,
                                 minRadius = 0, allowOverlap = TRUE, returnIndices = TRUE)

  hab <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(hab, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  n <- 10
  coords <- cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                  y = stats::runif(n, xmin(hab), xmax(hab)))
  indices <- 1:ncell(hab)

  microbenchmark(
    times = 3,
    distsDFP10Pts = dists2 <- distanceFromPoints(hab, coords[, c("x", "y")]),
    distsDFP1Pt = dists3 <- distanceFromPoints(hab, coords[1, c("x", "y"), drop = FALSE]),
    distsDFEP10Pts = dists1 <- distanceFromEachPoint(coords, landscape = hab),
    distsDFEP1Pt = dists5 <- distanceFromEachPoint(coords[1, , drop = FALSE], landscape = hab),
    distsDFEPMaxD =  dists6 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 30),
    distsCir =  dists7 <- cir(coords[, c("x", "y")], landscape = hab, maxRadius = 30,
                              minRadius = 0, returnDistances = TRUE,
                              returnIndices = TRUE, allowOverlap = TRUE),
    distsRings =  dists8 <- rings(loci = loci, landscape = hab, maxRadius = 30, minRadius = 0,
                                  allowOverlap = TRUE, returnIndices = TRUE)
  )

  #  for(i in 1:10) {
  nPix <- seq(100, 500, length.out = 5)
  nLoci <- seq(1, log(1000), length.out = 5)
  results <- list()

  for (numPix in nPix) {
    a <- as.character(numPix)
    results[[a]] <- list()
    for (numLoci in round(exp(nLoci))) {
      b <- as.character(numLoci)
      results[[a]][[b]] <- list()
      hab <- raster(extent(0, numPix, 0, numPix), res = 1)
      n <- numLoci
      coords <- cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                      y = stats::runif(n, xmin(hab), xmax(hab)))
      results[[a]][[b]] <- summary(microbenchmark(
        times = 1,
        dfp  = distanceFromPoints(hab, coords[, c("x", "y"), drop = FALSE]),
        dfep20 = distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab),
        dfep20Cum = distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                          landscape = hab, cumulativeFn = `+`),
        cir20 = cir(coords = coords[, c("x", "y")], landscape = hab, maxRadius = 20,
                    minRadius = 0, returnDistances = TRUE, allowOverlap = TRUE)
      ))
    }
  }

  numPix <- 3300
  hab <- raster(extent(0, numPix, 0, numPix), res = 1)
  n <- 200
  coords <- cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                  y = stats::runif(n, xmin(hab), xmax(hab)))
  a <- Sys.time()
  dfep20Cum <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE], landscape = hab,
                                     cumulativeFn = `+`, maxDistance = 50)
  b <- Sys.time()
  cir20 <- cir(coords = coords[, c("x", "y")], landscape = hab, maxRadius = 50,
               minRadius = 0, returnDistances = TRUE, allowOverlap = TRUE)
  d <- Sys.time()

  idwRaster <- raster(hab)
  idwRaster[] <- dfep20Cum[, "val"]
  if (interactive()) Plot(idwRaster)

  cells <- cellFromXY(hab, cir20[, c("x", "y")])
  cir20DT <- data.table(cir20, cells, key = "cells")
  idw <- cir20DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  # min        lq      mean    median        uq       max neval
  # 376.16213 407.44622 437.55936 437.79256 482.09462 484.06806     6
  # 476.83388 553.83873 571.78810 562.29938 585.29849 690.15876     6
  # 26.33451  26.34125  30.87619  32.25658  33.34990  34.71832     6
  # 70.91884  70.97720  71.32302  71.19613  71.76523  71.88459     6
  #}
  # sum of idw
  cells <- cellFromXY(hab, dists7[, c("x", "y")])
  dists7DT <- data.table(dists7, cells, key = "cells")
  idw <- dists7DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  # sum of idw
  cells <- cellFromXY(hab, dists6[, c("x", "y")])
  dists6DT <- data.table(dists6, cells, key = "cells")
  idw <- dists6DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw

  cells <- cellFromXY(hab, dists7[, c("x", "y")])
  dists7DT <- data.table(dists7, cells, key = "cells")
  idw <- dists7DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]
  distsRasCir <- raster(hab)
  distsRasCir[] <- 0
  distsRasCir[idw$cells] <- idw$idw

  dists8DT <- data.table(dists8, key = "indices")
  idw <- dists8DT[, list(idw = sum(1 / sqrt(1 + dists))), by = indices]
  distsRasRings <- raster(hab)
  distsRasRings[] <- 0
  distsRasRings[idw$indices] <- idw$idw

  distDiff1 <- round(distsRas - distsRasCir, 2)
  tabs <- table(getValues(distDiff1))

  # This  tests that the two approaches are 93% similar:
  expect_true(tabs[names(tabs) == 0] / ncell(distDiff1) > 0.93)

  if (interactive()) {
    Plot(distsRasCir, distsRasRings, distsRas, dists3, new = TRUE)
    Plot(distDiff1, zero.color = "white", new = TRUE)
  }
  sum(abs(getValues(distsRasCir - distsRasRings)))

  hab <- raster(extent(0, 1e3, 0, 1e3), res = 1, val = 0)
  n <- 10
  coords <- cbind(x = stats::runif(n, xmin(hab), xmax(hab)),
                  y = stats::runif(n, xmin(hab), xmax(hab)))
  coords <- cbind(coords, id = cellFromXY(hab, coords))

  microbenchmark(times = 2,
                 d1 = dists10 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 10),
                 d2 = dists30 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 30),
                 d3 =  dists7 <- cir(landscape = hab, coords = coords[, c("x", "y")],
                                     maxRadius = 30, minRadius = 0, returnDistances = TRUE,
                                     allowOverlap = TRUE),
                 d4 = dists50 <- distanceFromEachPoint(coords, landscape = hab, maxDistance = 300),
                 d5 = distsDFP <- distanceFromPoints(xy = coords[, c("x", "y")], object = hab)
  )

  # sum of idw
  cells <- cellFromXY(hab, dists50[, c("x", "y")])
  dists50DT <- data.table(dists50, cells, key = "cells")
  idw <- dists50DT[, list(idw = sum(1 / sqrt(1 + dists))), by = cells]

  distsRas <- raster(hab)
  distsRas[] <- 0
  distsRas[idw$cells] <- idw$idw
  if (interactive()) Plot(distsRas, new = TRUE)

  #Unit: milliseconds
  #          expr      min       lq     mean   median       uq      max neval
  #distsDFEP10Pts 856.8681 1058.968 1131.578 1165.994 1225.173 1246.662    10


  #
  head(dists5)
  distsRas <- raster(hab)
  distsRas[] <- dists5[, "dists"]
  if (interactive()) Plot(distsRas, dists3, new = TRUE)

  distsRas1 <- raster(hab)
  indices <- cellFromXY(hab, dists1[, c("x", "y")])
  invDist <- tapply(dists1[, "dists"], indices, function(x) sum(1 / (1 + x)))
  distsRas1[] <- as.vector(invDist)
  if (interactive()) Plot(distsRas1)

  dists5b <- do.call(rbind, dists5)
  all.equal(dists1b, dists3[, 1:4])

  dists4b <- do.call(rbind, dists4)
  all.equal(dists4b, dists1b[, c(1, 3:4)])

  library(microbenchmark)
  microbenchmark(times = 100,
                 cirs <- cir(hab, caribou, maxRadius = radius * 1.5, minRadius = radius,
                             simplify = TRUE, includeBehavior = "excludePixels"),
                 cirs2 <- {
                   loci <- cellFromXY(hab, coordinates(caribou))
                   cirs2 <- rings(hab, loci, minRadius = radius, maxRadius = radius * 1.5)
                 })


  loci <- cellFromXY(hab, coordinates(caribou))
  radius <- 15
  n <- 10
  caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                                          y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5))
  microbenchmark(
    times = 200,
    cirs2 <- rings(hab, loci, minRadius = 0, maxRadius = radius, returnIndices = TRUE,
                   allowOverlap = FALSE),

    aNoDists = cir(hab, coords = coordinates(caribou), allowOverlap = FALSE,
                   returnDistances = FALSE, maxRadius = radius, minRadius = 0,
                   includeBehavior = "includePixels"),

    aDists = cir(hab, coords = coordinates(caribou), allowOverlap = FALSE,
                 returnDistances = TRUE, maxRadius = radius, minRadius = 0,
                 includeBehavior = "includePixels", returnAngles = TRUE)
  )

  #Unit: milliseconds
  #expr           min        lq      mean    median        uq       max neval
  #cirs2    25412.453 25755.583 33040.352 26688.340 36125.005  87270.60   200
  #aNoDists   934.076  954.4595  1098.388   963.697  975.8685  8149.194   400
  #aDists    1617.404 1639.2535  1855.630  1656.263 1694.3885  9116.704   400

  microbenchmark(times = 200,
                 noOverlap = cir(hab, coords = coordinates(caribou),
                                 allowOverlap = FALSE, returnDistances = FALSE,
                                 maxRadius = radius, minRadius = 0,
                                 includeBehavior = "includePixels", returnAngles = TRUE),

                 yesOverlap = cir(hab, coords = coordinates(caribou),
                                  allowOverlap = TRUE, returnDistances = FALSE,
                                  maxRadius = radius, minRadius = 0,
                                  includeBehavior = "includePixels", returnAngles = TRUE)
  )
  for (i in 1:600) {
    cir(hab, coords = coordinates(caribou),
        allowOverlap = FALSE, returnDistances = FALSE,
        maxRadius = radius, minRadius = 0, includeBehavior = "includePixels",
        returnAngles = FALSE)
  }

  tst <- TRUE
  count <- 0
  tmp <- data.frame(len = numeric(), size1 = numeric(), j = numeric(), oneClump = logical())
  while (tst) {
    size1 <- sample(1.1 ^ (2:10 * 8), 1)
    hab <- raster(extent(0, size1, 0, size1), res = 1)
    n <- 1
    radius <- ncol(hab) / 5
    caribou <- SpatialPoints(coords = cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                                            y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5))
    count <- count + 1
    seed <- sample(1e6, 1)
    set.seed(seed)
    tmp1 <- capture.output(cirs <- cir(hab, caribou, maxRadius = radius, minRadius = 0,
                                       simplify = TRUE, includeBehavior = "excludePixels"))
    tmp[count, 1] <- as.numeric(strsplit(tmp1, split = " ")[[1]][2])
    ras1 <- raster(hab)
    ras1[] <- 1
    cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
    ras1[cirsOverlap$indices] <- 0 #cirsOverlap$sumIDs
    ras1Clump <- clump(ras1)
    if (interactive()) Plot(ras1, ras1Clump, new = TRUE)
    smallerEx <- extent(ras1) - 2
    ras1ClumpSm <- crop(ras1Clump, smallerEx)
    tmp[count, 2:4] <- c(size1, j, all(table(getValues(ras1ClumpSm)) > 2))
    tst <- all(table(getValues(ras1ClumpSm)) > 2)
  }
  if (interactive()) Plot(ras1, ras1Clump, new = TRUE)

  tmpDT <- data.table(tmp)
  setkey(tmpDT, oneClump)
  tmpDT[, min(len, na.rm = TRUE), by = oneClump]
  setkey(tmpDT, oneClump, len)
  tmpDT[oneClump == 0]
})

test_that("cir angles arg doesn't work", {
  library(fpCompare)
  library(raster)

  on.exit({
    detach("package:raster")
    detach("package:fpCompare")
  }, add = TRUE)

  ras <- raster(extent(0, 100, 0, 100), res = 1)
  ras[] <- 0
  n <- 2
  coords <- cbind(x = stats::runif(n, xmin(ras), xmax(ras)),
                  y = stats::runif(n, xmin(ras), xmax(ras)))
  angles <- seq(0, 2 * pi, length.out = 21)[-21]
  circ <- cir(ras, coords, angles = angles, maxRadius = 3, minRadius = 0,
              returnIndices = TRUE, allowOverlap = TRUE, returnAngles = TRUE)
  anglesTab <- table(circ[, "angles"])
  expect_true(all(as.numeric(names(anglesTab)) %==% angles))
  expect_true(all(length(anglesTab) == (length(angles))))

  skip("microbenchmarking below this")

  library(microbenchmark); on.exit(detach("package:microbenchmark"), add = TRUE)
  ras <- raster(extent(0, 330, 0, 330), res = 1)
  ras[] <- 0
  n <- 1e3
  coords <- cbind(x = stats::runif(n, xmin(ras), xmax(ras)),
                  y = stats::runif(n, xmin(ras), xmax(ras)))
  angles <- seq(0, 2 * pi, length.out = 21)[-21]
  newWay <- FALSE
  microbenchmark(times = 100,
                 circ <- cir(ras, coords, angles = angles,
                             maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                             allowOverlap = TRUE, returnAngles = TRUE)
  )
  #min       lq     mean   median       uq      max neval
  #65.16964 76.17871 100.7652 90.87756 118.5901 390.5539   100

  newWay <- TRUE
  microbenchmark(times = 100,
                 circ <- cir(ras, coords, angles = angles,
                             maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                             allowOverlap = TRUE, returnAngles = TRUE)
  )
  #     min       lq     mean   median       uq      max neval
  #22.44104 26.70138 32.47423 30.65686 35.89569 45.72201    10

  newWay <- FALSE; circOW <- cir(ras, coords, angles = angles,
                                 maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                                 allowOverlap = TRUE, returnAngles = TRUE)
  newWay <- TRUE; circNW <- cir(ras, coords, angles = angles,
                                maxRadius = 3, minRadius = 0, returnIndices = TRUE,
                                allowOverlap = TRUE, returnAngles = TRUE)
})
