test_that("spread produces legal RasterLayer", {
  set.seed(123)

  library(raster); on.exit(detach("package:raster"), add = TRUE)

  # inputs for x
  a <- raster(extent(0, 20, 0, 20), res = 1)
  b <- raster(extent(a), res = 1, vals = stats::runif(ncell(a), 0, 1))

  # check it makes a RasterLayer
  expect_that(spread(a, loci = ncell(a) / 2, stats::runif(1, 0.15, 0.25)), is_a("RasterLayer"))

  #check wide range of spreadProbs
  for (i in 1:20) {
    expect_that(spread(a, loci = ncell(a) / 2, stats::runif(1, 0, 1)), is_a("RasterLayer"))
  }

  # Test for NAs in a numeric vector of spreadProb values
  numNAs <- 50
  sps <- sample(c(rep(NA_real_, numNAs), runif(ncell(a) - numNAs, 0, 0.5)))
  expect_that(out <- spread(a, loci = ncell(a) / 2, spreadProb = sps), is_a("RasterLayer"))

  # check spreadProbs outside of legal returns an "spreadProb is not a probability"
  expect_that(spread(a, loci = ncell(a) / 2, 1.1), throws_error("spreadProb is not a probability"))
  expect_that(spread(a, loci = ncell(a) / 2, -0.1), throws_error("spreadProb is not a probability"))

  # checks if maxSize is working properly
  # One process spreading
  expect_equal(ncell(a), tabulate(spread(a, spreadProb = 1, id = TRUE)[]))

  # several processes spreading
  sizes <- rep_len(50, 3)
  expect_equal(sizes,
               tabulate(spread(a, loci = c(40, 200, 350), spreadProb = 1,
                               id = TRUE, maxSize = sizes)[]))

  # Check that with maxSize, the active cells are removed when maxSize is reached
  b <- raster(extent(0, 20, 0, 20), res = 1)
  loci <- sample(ncell(b), size = 1)
  spreadProb <- 0.27
  seed <- 914953
  set.seed(seed)
  maxSize1 <- 1e2
  spreadState <- SpaDES.tools::spread(
    landscape = b,
    loci = loci,
    spreadProb = spreadProb,
    returnIndices = TRUE,
    maxSize = maxSize1)

  expect_true(length(unique(spreadState[["indices"]])) == maxSize1)
  expect_true(length(spreadState[["indices"]]) == maxSize1)

  # Test that spreadState with a data.table works
  fires <- list()
  fires[[1]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)),
                       returnIndices = TRUE, spreadProb = 0.235, persistence = 0,
                       mask = NULL, maxSize = 1e8, 8, iterations = 2, id = TRUE)
  stopped <- list()
  stopped[[1]] <- fires[[1]][, sum(active), by = id][V1 == 0, id]
  for (i in 2:4) {
    j <- sample(1:1000, 1);
    set.seed(j);
    fires[[i]] <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices = TRUE,
                         spreadProb = 0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE,
                         spreadState = fires[[i - 1]])
    stopped[[i]] <- fires[[i]][, sum(active), by = id][V1 == 0, id]

    # Test that any fire that stopped previously is not rekindled
    expect_true(all(stopped[[i - 1]] %in% stopped[[i]]))
  }

  # Test that passing NA to loci returns a correct data.table
  set.seed(123)
  fires <- spread(a, loci = as.integer(sample(1:ncell(a), 10)), returnIndices = TRUE,
                  0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE)
  fires2 <- spread(a, loci = NA_real_, returnIndices = TRUE,
                   0.235, 0, NULL, 1e8, 8, iterations = 2, id = TRUE,
                   spreadState = fires)
  expect_true(all(fires2[, unique(id)] %in% fires[, unique(id)]))
  expect_true(all(fires[, unique(id)] %in% fires2[, unique(id)]))

  if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.0") {
    # Skip this because not necessary for older versions. This will be deprecated soon enough
    #expect_true(all(fires2[, length(initialLocus), by = id][, V1] ==
    #                  c(4L, 8L, 7L, 9L, 1L, 25L, 13L, 13L, 20L, 1L)))
  } else {
    expect_true(all(fires2[, length(initialLocus), by = id][, V1] ==
                      c(8L, 9L, 2L, 2L, 28L, 17L, 21L, 3L, 6L, 7L)))
  }
})

test_that("allowOverlap -- produces exact result", {
  testInitOut <- testInit(needGoogle = FALSE, c("sp", "raster"))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  N <- 10
  a <- raster::raster(extent(0, N, 0, N), res = 1)
  ao <- c(FALSE, TRUE)
  mp <- middlePixel(a)
  mps <- mp+(-3:3)

  # ._spread_3 <- ._spread_19 <-._spread_14 <- 1;
  b <- list()
  Nreps <- 100
  sams <- sample(1e7, Nreps)
  for (i in seq_along(ao)) {
    b[[i]] <- list()
    for (j in seq_len(Nreps)) {
      set.seed(sams[j])
  #    set.seed(1892809)
      b[[i]][[j]] <- spread(a, loci = mp, spreadProb = 0.22, id = TRUE,
                            allowOverlap = ao[i], returnIndices = TRUE)
    }
  }
  bs <- lapply(b, function(x) rbindlist(x, idcol = "rep"))
  expect_true(all.equal(bs[[1]], bs[[2]]))
  # aBigger <- sum(unlist(lapply(purrr::transpose(b), function(x) NROW(x[[1]]) < NROW(x[[2]]))))
  # bBigger <- sum(unlist(lapply(purrr::transpose(b), function(x) NROW(x[[1]]) > NROW(x[[2]]))))
  # (comp <- aBigger - bBigger)
  # out <- abs(comp) < Nreps/20
  # expect_true(out)

  ##################################################
  i <- 0L
  b <- list()
  Nreps <- 100
  sams <- sample(1e7, Nreps)

  #._spread_14 <- 1
  for (i in seq_along(ao)) {
    b[[i]] <- list()
    for (j in seq_len(Nreps)) {
      set.seed(sams[j])
      b[[i]][[j]] <- spread(a, loci = mps, spreadProb = 0.22, id = TRUE,
                            allowOverlap = ao[i], returnIndices = TRUE)
    }
  }
  bs <- lapply(b, function(x) rbindlist(x, idcol = "rep"))
  ras <- list()
  for (i in seq_along(bs)) {
    ras[[i]] <- raster(a)
    ras[[i]][] <- 0
    v <- bs[[i]][, .N, by = 'indices']
    ras[[i]][v$indices] <- v$N
  }
  s <- raster::stack(ras)
  s <- crop(s, extent(s[[1]], 2, 9, 2, 9))
  o <- raster::calc(s, function(x) x[2] >= x[1])
  expect_true(sum(o[] == 1) > (ncell(s) - 10))

})
test_that("spread stopRule does not work correctly", {
  library(raster)
  library(quickPlot)

  on.exit({
    detach("package:quickPlot")
    detach("package:raster")
  }, add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50

  ## stopRule examples
  #  examples with stopRule, which means that the eventual size is driven by the
  #  values on the raster passed in to the landscape argument
  set.seed(1234)
  startCells <- as.integer(sample(1:ncell(hab), 10))
  stopRule1 <- function(landscape) sum(landscape) > maxVal
  stopRuleA <- spread(hab, loci = startCells, spreadProb = 1, persistence = 0,
                      mask = NULL, maxSize = 1e6, directions = 8,
                      iterations = 1e6, id = TRUE,
                      circle = TRUE, stopRule = stopRule1)
  foo <- cbind(vals = hab[stopRuleA], id = stopRuleA[stopRuleA > 0]);
  expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) > maxVal))

  # using stopRuleBehavior = "excludePixel"
  set.seed(1234)
  stopRuleB <- spread(hab, loci = startCells, 1, 0, NULL, maxSize = 1e6, 8, 1e6,
                      id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  foo <- cbind(vals = hab[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) <= maxVal))

  # If boolean, then it is exact
  stopRuleB <- spread(hab2, loci = startCells, 1, 0, NULL, maxSize = 1e6, 8, 1e6,
                      id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == maxVal))

  # Test vector maxSize and stopRule when they interfere
  maxSizes <- sample(maxVal * 2, length(startCells))
  stopRuleB <- spread(hab2, loci = startCells, 1, 0, NULL, maxSize = maxSizes,
                      8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(stopRuleB, new = TRUE)
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == pmin(maxSizes, maxVal)))

  # Test non integer maxSize and stopRule when they interfere
  maxSizes <- runif(length(startCells), 1, maxVal * 2)
  stopRuleB <- spread(hab2, loci = startCells, 1, 0, NULL, maxSize = maxSizes, 8,
                      1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                      stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(stopRuleB, new = TRUE)
  foo <- cbind(vals = hab2[stopRuleB], id = stopRuleB[stopRuleB > 0]);
  expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == pmin(floor(maxSizes), maxVal)))

  ####################################
  # Test for stopRuleBehavior
  ####################################
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 2))
  set.seed(53432)
  circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                  id = TRUE, stopRule = stopRule2, stopRuleBehavior = "includeRing")
  cirs <- getValues(circs)
  vals <- tapply(hab[circs], cirs[cirs > 0], sum)
  expect_true(all(vals >= maxVal))

  set.seed(53432)
  circs2 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludeRing")
  cirs <- getValues(circs2)
  vals <- tapply(hab[circs2], cirs[cirs > 0], sum)
  expect_true(all(vals <= maxVal))

  set.seed(53432)
  circs3 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "includePixel")
  cirs <- getValues(circs3)
  vals <- tapply(hab[circs3], cirs[cirs > 0], sum)
  expect_true(all(vals <= (maxVal + maxValue(hab))))

  set.seed(53432)
  circs4 <- spread(hab, spreadProb = 1, circle = TRUE, loci = startCells,
                   id = TRUE, stopRule = stopRule2, stopRuleBehavior = "excludePixel")
  cirs <- getValues(circs4)
  vals <- tapply(hab[circs4], cirs[cirs > 0], sum)
  expect_true(all(vals >= (maxVal - maxValue(hab))))

  # There should be 1 extra cell
  expect_true(sum(getValues(circs4) > 0) + length(startCells) == sum(getValues(circs3) > 0))
  # Order should be includeRing, includePixel, excludePixel, excludeRing
  expect_true(sum(getValues(circs) > 0) > sum(getValues(circs3) > 0))
  expect_true(sum(getValues(circs3) > 0) > sum(getValues(circs4) > 0))
  expect_true(sum(getValues(circs4) > 0) > sum(getValues(circs2) > 0))

  ####################################
  # Test for circles using maxDist
  ####################################

  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = startCells,
                  id = TRUE, circleMaxRadius = maxRadius)
  cells <- which(getValues(circs) == 1)
  centre <- xyFromCell(hab2, startCells)
  allCells <- xyFromCell(hab2, cells)
  pd <- pointDistance(centre, allCells, lonlat = FALSE)
  expect_true(maxRadius == max(pd))

  # Test for circles using maxDist
  set.seed(543345)
  numCircs <- 4
  set.seed(53432)
  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), numCircs))

  circs <- spread(hab2, spreadProb = 1, circle = TRUE,
                  loci = startCells,
                  id = TRUE, circleMaxRadius = maxRadius)
  if (interactive()) Plot(circs, new = TRUE)

  for (whCirc in 1:numCircs) {
    cells <- which(getValues(circs) == whCirc)
    centre <- xyFromCell(hab2, startCells)
    allCells <- xyFromCell(hab2, cells)
    pd <- pointDistance(centre[whCirc, ], allCells, lonlat = FALSE)
    circEdge <- circs
    circEdge[] <- 0
    circEdge[cells[pd == maxRadius]] <- 1
    expect_true(all(circs[cells[pd == maxRadius]] == whCirc))
    if (!is.null(circs[as.vector(adj(hab2, cells[pd == maxRadius], pairs = FALSE))])) {
      # Test that there are both 0 and whCirc values, i.e,. it is on an edge
      expect_true(all(c(0, whCirc) %in%
                        circs[as.vector(adj(hab2, cells[pd == maxRadius], pairs = FALSE))]))
    }
    if (interactive()) Plot(circEdge, addTo = "circs",
                            cols = c("transparent", rainbow(numCircs)[whCirc]))
  }

  # Test complex functions
  initialLoci <- (ncell(hab) - ncol(hab)) / 2 + c(4, -4)
  endSizes <- seq_along(initialLoci) * 200
  stopRule3 <- function(landscape, id, endSizes) sum(landscape) > endSizes[id]

  twoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                               directions = 8, id = TRUE, stopRule = stopRule3,
                               endSizes = endSizes, stopRuleBehavior = "excludePixel")
  if (interactive()) Plot(twoCirclesDiffSize, new = TRUE)
  cirs <- getValues(twoCirclesDiffSize)
  vals <- tapply(hab[twoCirclesDiffSize], cirs[cirs > 0], sum)
  expect_true(all(vals < endSizes))

  # Test allowOverlap
  initialLoci <- as.integer(sample(1:ncell(hab), 10))

  #expect_error({
    circs <- spread(hab2, spreadProb = 1, circle = TRUE, loci = initialLoci,
                    id = TRUE, circleMaxRadius = maxRadius, allowOverlap = TRUE)
  #}) ## TODO: fix error when allowOverlap = TRUE

  #expect_error({
    circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                    maxSize = 10, allowOverlap = TRUE)
  #}) ## TODO: fix error when allowOverlap = TRUE

  #expect_error({
    circs <- spread(hab2, spreadProb = 1, loci = initialLoci,
                    maxSize = seq_along(initialLoci) * 3, allowOverlap = TRUE)
  #}) ## TODO: fix error when allowOverlap = TRUE

  # Test allowOverlap and stopRule
  for (i in 1:6) {
    maxVal <- sample(10:300, 1)
    stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal

    #expect_error({
      circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                      stopRule = stopRule2, maxVal = maxVal, returnIndices = TRUE,
                      id = TRUE, allowOverlap = TRUE, stopRuleBehavior = "includeRing")
    # }) ## TODO: fix error when allowOverlap = TRUE

    #vals <- tapply(hab[circs$indices], circs$id, sum) ## TODO: fix allowOverlap error
    #expect_true(all(vals > maxVal)) ## TODO: fix allowOverlap error
  }

  #stopRuleBehavior the allowOverlap
  maxVal <- 20
  stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal
  #expect_error({
    circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                    stopRule = stopRule2, maxVal = maxVal, returnIndices = TRUE,
                    id = TRUE, allowOverlap = TRUE, stopRuleBehavior = "excludePixel")
  #}) ## TODO: fix error when allowOverlap = TRUE
  #vals <- tapply(hab[circs$indices], circs$id, sum) ## TODO: fix allowOwerlap error
  #expect_true(all(vals <= maxVal)) ## TODO: fix allowOverlap error

  maxVal <- sample(10:100, 10)
  stopRule2 <- function(landscape, id, maxVal) sum(landscape) > maxVal[id]
  #expect_error({
    circs <- spread(hab, spreadProb = 1, circle = TRUE, loci = initialLoci,
                    stopRule = stopRule2, id = TRUE, allowOverlap = TRUE,
                    stopRuleBehavior = "excludePixel", maxVal = maxVal,
                    returnIndices = TRUE)
  #}) ## TODO: fix error when allowOverlap = TRUE
  #vals <- tapply(hab[circs$indices], circs$id, sum) ## TODO: fix allowOverlap error
  #expect_true(all(vals <= maxVal)) ## TODO: fix allowOverlap error
  # Test that maxSize can be a non integer value (i.e, Real)

  # Test arbitrary raster as part of stopRule
  # Stop if sum of landscape is big or mean of quality is too small
  for (i in 1:6) {
    initialLoci <- as.integer(sample(1:ncell(hab), 10))
    quality <- raster(hab)
    quality[] <- runif(ncell(quality), 0, 1)
    sumLandscapeRule <- 100
    meanHabitatRule <- 0.4
    stopRule4 <- function(landscape, quality, cells) {
      (sum(landscape) > sumLandscapeRule) | (mean(quality[cells]) < meanHabitatRule)
    }

    circs <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                    directions = 8, id = TRUE, stopRule = stopRule4, quality = quality,
                    stopRuleBehavior = "includePixel", returnIndices = TRUE)

    ras <- raster(quality)
    ras[] <- 0
    circsVals <- circs[, numEvents := sum(unique(id)), by = indices]
    ras[circsVals$indices] <- circsVals$numEvents
    a1 <- cbind(quality = quality[ras], hab = hab[ras], id = ras[ras])
    a2 <- tapply(a1[, "hab"], a1[, "id"], sum)
    a3 <- tapply(a1[, "quality"], a1[, "id"], mean)
    wh <- which(a3 < meanHabitatRule)
    a4 <- tapply(a1[, "quality"], a1[, "id"], length)
    expect_true(all(a2[wh] < sumLandscapeRule))
    expect_true(all(a2[-wh] >= sumLandscapeRule))
    expect_true(all(a3[-wh] >= meanHabitatRule))
    expect_true(all(a3[wh] < meanHabitatRule))
    if (interactive()) {
      clearPlot()
      Plot(ras)
    }
  }
})

test_that("asymmetry doesn't work properly", {
  library(CircStats)
  library(raster)
  library(quickPlot)

  on.exit({
    detach("package:quickPlot")
    detach("package:raster")
    detach("package:CircStats")
  }, add = TRUE)

  a <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab) <- "hab"
  hab2 <- hab > 0
  maxRadius <- 25
  maxVal <- 50
  set.seed(53432)

  stopRule2 <- function(landscape) sum(landscape) > maxVal
  startCells <- as.integer(sample(1:ncell(hab), 1))

  n <- 16
  avgAngles <- numeric(n)
  lenAngles <- numeric(n)

  # function to calculate mean angle -- returns in degrees
  meanAngle <- function(angles) {
    deg(atan2(mean(sin(rad(angles))), mean(cos(rad(angles)))))
  }

  if (interactive()) clearPlot()
  seed <- sample(1e6, 1)
  set.seed(seed)
  for (asymAng in (2:n)) {
    circs <- spread(hab, spreadProb = 0.25, loci = ncell(hab) / 2 - ncol(hab) / 2,
                    id = TRUE, returnIndices = TRUE,
                    asymmetry = 40, asymmetryAngle = asymAng * 20)
    ci <- raster(hab)
    ci[] <- 0
    ci[circs$indices] <- circs$id
    ciCentre <- raster(ci)
    ciCentre[] <- 0
    ciCentre[unique(circs$initialLocus)] <- 1
    newName <- paste0("ci", asymAng * 20)
    assign(newName, ci)

    where2 <- function(name, env = parent.frame()) {
      # simplified from pryr::where
      if (exists(name, env, inherits = FALSE)) env else where2(name, parent.env(env))
    }
    env <- where2(newName)
    if (interactive()) {
      obj <- get(newName, envir = env)
      Plot(obj)
      Plot(ciCentre, cols = c("transparent", "black"), addTo = newName)
    }
    a <- cbind(id = circs$id, to = circs$indices, xyFromCell(hab, circs$indices))
    initialLociXY <- cbind(id = unique(circs$id), xyFromCell(hab, unique(circs$initialLocus)))
    dirs <- directionFromEachPoint(from = initialLociXY, to = a)
    dirs[, "angles"] <- CircStats::deg(dirs[, "angles"])
    avgAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], meanAngle) %% 360
    lenAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], length)
  }

  whBig <- which(lenAngles > 50)
  pred <- (1:n)[whBig] * 20
  expect_true(abs(coef(lm(avgAngles[whBig]~pred))[[2]] - 1) < 0.1)
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
  #expect_error({
    cirs2 <- rings(hab, loci, minRadius = radius, maxRadius = radius * 1.5001,
                   allowOverlap = TRUE, returnIndices = TRUE, includeBehavior = "includeRing")
  #}) ## TODO: fix error when allowOverlap = TRUE

  #expect_true(all.equal(range(cirs$dists), range(cirs2$dists)))
  #setkey(cirs2, dists, indices)
  setkey(cirs,  dists, indices)
  ras1 <- raster(hab)
  ras1[] <- 0
  cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
  ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
  if (interactive()) Plot(ras1, new = TRUE)

  ras2 <- raster(hab)
  ras2[] <- 0
  #cirsOverlap2 <- cirs2[, list(sumIDs = sum(id)), by = indices]
  #ras2[cirsOverlap2$indices] <- cirsOverlap2$sumIDs
  #ras3 <- ras1 - ras2
  #if (interactive()) Plot(ras2, ras3, zero.color = "transparent")
  #expect_equal(0, sum(abs(getValues(ras3))))

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
})

test_that("simple cir does not work correctly", {
  set.seed(1234)
  library(fpCompare)
  library(raster)

  on.exit({
    detach("package:raster")
    detach("package:fpCompare")
  }, add = TRUE)

  hab <- raster(extent(0, 1e1, 0, 1e1), res = 1)

  circleRas <- cir(hab, maxRadius = 1, includeBehavior = "excludePixels")
  expect_true(NROW(circleRas) == 4)
  expect_true(all(circleRas[, "indices"] == c(35, 44, 55, 46)))
  expect_true(all(mean(circleRas[, "x"]) == (ncol(hab) / 2 - 0.5)))
  expect_true(all(mean(circleRas[, "y"]) == (nrow(hab) / 2 + 0.5)))

  n <- 1
  coords <- cbind(x1 = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                  y1 = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5)
  expect_error(cir(hab, coords = coords), "coords must have columns named x and y")

  # test id column in coords
  n <- 2
  coords <- cbind(x = (stats::runif(n, xmin(hab) + 0.5, xmax(hab) - 0.5)),
                  y = (stats::runif(n, xmin(hab) + 0.5, xmax(hab) - 0.5)),
                  id = c(45, 56))
  cirs <- cir(hab, coords = coords, maxRadius = 1, minRadius = 0,
              includeBehavior = "includePixels", returnIndices = TRUE)
  expect_true(all(unique(cirs[, "id"]) == c(45, 56)))
  expect_true(all(distanceFromEachPoint(coords, cirs)[, "dists"] %<=% 1))

  # test closest
  n <- 1
  coords <- cbind(x = c(5, 6), y = c(5, 5))
  cirsClosestT <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
                      includeBehavior = "includePixels", closest = TRUE,
                      returnIndices = TRUE, allowOverlap = FALSE)
  cirsClosestF <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
                      includeBehavior = "includePixels", closest = FALSE,
                      returnIndices = TRUE, allowOverlap = FALSE)
  expect_true(all(table(cirsClosestF[, "id"]) == c(17, 4)))
  expect_true(all(table(cirsClosestT[, "id"]) - table(cirsClosestF[, "id"]) == c(-5, 5)))

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
               includeBehavior = "includePixels", closest = FALSE,
               returnIndices = FALSE, allowOverlap = FALSE, returnDistances = FALSE)
  expect_is(cirs2, "Raster")
  expect_true(max(getValues(cirs2)) == 2)
  expect_true(min(getValues(cirs2)) == 0)

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
               includeBehavior = "includePixels", closest = FALSE,
               returnIndices = FALSE, allowOverlap = TRUE, returnDistances = FALSE)
  expect_is(cirs2, "Raster")
  expect_true(min(getValues(cirs2)) == 0)

  cirs2 <- cir(hab, coords = coords, maxRadius = 2, minRadius = 0,
               includeBehavior = "includePixels", closest = FALSE,
               returnIndices = FALSE, allowOverlap = TRUE, returnDistances = TRUE)
  expect_is(cirs2, "Raster")
  expect_true(min(getValues(cirs2)) == 0)

  hab <- raster(extent(0, 1e1, 0, 1e1), res = c(1, 2))
  expect_error(cir(hab, maxRadius = 1, includeBehavior = "excludePixels"),
               "cir function only accepts rasters with identical resolution in x and y dimensions")

  hab <- raster(extent(0, 1e1, 0, 1e1), res = 1)
  expect_error(cir(hab, maxRadius = 1, includeBehavior = "excludeRings"),
               "includeBehavior can only be \"includePixels\" or \"excludePixels\"")
})

test_that("wrap does not work correctly", {
  library(raster)

  on.exit({
    detach("package:raster")
  }, add = TRUE)

  xrange <- yrange <- c(-50, 50)
  hab <- raster(extent(c(xrange, yrange)))
  hab[] <- 0

  # initialize caribou agents
  n <- 10

  # previous points
  x1 <- rep(0, n)
  y1 <- rep(0, n)
  # initial points, outside of range
  starts <- cbind(x = stats::runif(n, xrange[1] - 10, xrange[1]),
                  y = stats::runif(n, yrange[1] - 10, yrange[1]))

  expect_false(all(wrap(starts, bounds = extent(hab)) == starts))
  expect_false(all(wrap(starts, bounds = hab) == starts))
  expect_false(all(wrap(starts, bounds = bbox(hab)) == starts))
  expect_error(wrap(starts, bounds = starts),
               "Must use either a bbox, Raster\\*, or Extent for 'bounds'")

  # create spdf
  spdf <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
  expect_true(all(coordinates(wrap(spdf, bounds = hab)) == wrap(starts, hab)))
  expect_true(all(coordinates(wrap(spdf, bounds = hab, withHeading = FALSE)) ==
                    wrap(starts, hab)))
  expect_true(all(coordinates(wrap(spdf, bounds = bbox(hab), withHeading = FALSE)) ==
                    wrap(starts, hab)))
  expect_error(wrap(spdf, bounds = starts, withHeading = FALSE),
               "Must use either a bbox, Raster\\*, or Extent for 'bounds'")

  # errrors
  starts <- cbind(x1 = stats::runif(n, xrange[1] - 10, xrange[1]),
                  y = stats::runif(n, yrange[1] - 10, yrange[1]))
  spdf <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
  expect_error(wrap(spdf, bounds = extent(hab)),
               "When X is a matrix, it must have 2 columns, x and y,")
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

})

test_that("multi-core version of distanceFromEachPoints does not work correctly", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (interactive()) {
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    library(parallel);

    hab <- randomPolygons(raster(extent(0, 1e2, 0, 1e2)), res = 1)

    # evaluate cumulativeFn
    n <- 50
    coords <- cbind(x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
                    y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5)
    dfep <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                  landscape = hab, cumulativeFn = `+`)

    ## using parallel package cluster
    system.time({
      cl1 <- makeCluster(1, rscript_args = "--vanilla --no-environ")
      clusterEvalQ(cl1, {
        library(SpaDES.tools)})
    })
    system.time(
      dfepCluster <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                           landscape = hab, cumulativeFn = `+`,
                                           cl = cl1)
    )
    stopCluster(cl1)
    expect_true(all.equal(dfep, dfepCluster))

    ## using raster package cluster
    system.time({
      beginCluster(1)
    })
    system.time(
      dfepCluster2 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                            landscape = hab, cumulativeFn = `+`)
    )
    endCluster()
    expect_true(all.equal(dfep, dfepCluster2))
  }
})

test_that("spreadProb with relative values does not work correctly", {
  library(raster)

  on.exit({
    detach("package:raster")
  }, add = TRUE)

  seed <- 64350
  set.seed(seed)
  emptyRas <- raster(extent(0, 1e2, 0, 1e2), res = 1)
  hab <- randomPolygons(emptyRas, numTypes = 40)
  names(hab) <- "hab"

  hab3 <- (hab > 20) * 200 + 1
  sam <- sample(which(hab3[] == 1), 1)
  set.seed(seed)
  events1 <- spread(hab3, spreadProb = hab3, loci = sam, directions = 8,
                    neighProbs = c(0, 1), maxSize = c(100), exactSizes = TRUE)

  # Compare to absolute probability version
  set.seed(seed)
  events2 <- spread(hab3, id = TRUE, loci = sam, directions = 8,
                    neighProbs = c(0, 1), maxSize = c(100), exactSizes = TRUE)

  #if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.4") {
    ## many more high value hab pixels spread to in event1
  #  expect_true(sum(hab3[events1[] > 0]) > sum(hab3[events2[] > 0]))
  #} else {
    ## equal number on R-devel
    expect_equal(sum(hab3[events1[] > 0]), sum(hab3[events2[] > 0]))
  #}


  # Check numeric vector with NAs is equivalent to raster with NAs
  numNAs <- 50
  sps <- sample(c(rep(NA_real_, numNAs), runif(ncell(hab3) - numNAs, 0, 0.5)))
  ras <- raster(hab3)
  ras[] <- sps
  set.seed(seed)
  expect_that(out1 <- spread(hab3, loci = ncell(hab3) / 2, spreadProb = ras), is_a("RasterLayer"))
  set.seed(seed)
  expect_that(out2 <- spread(hab3, loci = ncell(hab3) / 2, spreadProb = sps), is_a("RasterLayer"))
  expect_identical(out1, out2)

})

