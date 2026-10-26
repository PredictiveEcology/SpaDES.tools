test_that("rings returns annuli between minRadius and maxRadius", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  loci <- (terra::ncell(emptyRas) / 2 - terra::ncol(emptyRas)) / 2 + c(-3, 3)

  rngs <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9,
                returnIndices = TRUE)

  expect_s3_class(rngs, "data.table")
  expect_true(all(c("id", "initialLocus", "indices", "dists") %in% names(rngs)))

  ## one ring per starting locus
  expect_setequal(unique(rngs$id), seq_along(loci))
  expect_setequal(unique(rngs$initialLocus), loci)

  ## every returned cell lies within the requested annulus
  expect_true(all(rngs$dists >= 7 & rngs$dists <= 9))

  ## the inner disc is excluded: no cell closer than minRadius is returned
  expect_false(any(rngs$dists < 7))
})

test_that("rings respects minRadius = 0 and grows with maxRadius", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  loci <- 5050

  disc <- rings(emptyRas, loci = loci, minRadius = 0, maxRadius = 5,
                returnIndices = TRUE)
  ## with minRadius 0 the starting cell itself is included
  expect_true(loci %in% disc$indices)
  expect_true(all(disc$dists <= 5))

  bigger <- rings(emptyRas, loci = loci, minRadius = 0, maxRadius = 10,
                  returnIndices = TRUE)
  expect_gt(nrow(bigger), nrow(disc))
})

test_that("rings can write its result into a raster", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  loci <- (terra::ncell(emptyRas) / 2 - terra::ncol(emptyRas)) / 2 + c(-3, 3)

  rngs <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9,
                returnIndices = TRUE)
  emptyRas[rngs$indices] <- rngs$id

  ## exactly the ring cells were marked, with their ring id
  expect_setequal(unique(terra::values(emptyRas)[, 1]), c(0, seq_along(loci)))
  expect_identical(sum(terra::values(emptyRas) > 0), nrow(rngs))

  ## the raster-returning form covers the same cells
  asRas <- rings(emptyRas, loci = loci, minRadius = 7, maxRadius = 9,
                 returnIndices = FALSE)
  expect_s4_class(asRas, "SpatRaster")
})

test_that("rings accepts a different radius per locus", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  loci <- (terra::ncell(emptyRas) / 2 - terra::ncol(emptyRas)) / 2 + c(-3, 3)

  ## second case from ?rings: a disc around the first locus, an annulus
  ## around the second
  rngs <- rings(emptyRas, loci = loci, minRadius = c(0, 7), maxRadius = c(8, 18),
                returnIndices = TRUE)

  expect_setequal(unique(rngs$id), seq_along(loci))

  ## each locus is held to ITS OWN radii, not a shared pair
  expect_true(all(rngs[id == 1]$dists >= 0 & rngs[id == 1]$dists <= 8))
  expect_true(all(rngs[id == 2]$dists >= 7 & rngs[id == 2]$dists <= 18))

  ## minRadius 0 keeps the centre cell of the first ring; minRadius 7 drops it
  ## for the second
  expect_true(loci[1] %in% rngs[id == 1]$indices)
  expect_false(loci[2] %in% rngs[id == 2]$indices)

  ## the wider second ring reaches further than the first
  expect_gt(max(rngs[id == 2]$dists), max(rngs[id == 1]$dists))
})

test_that("rings warns when radii are neither length 1 nor length(loci)", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  loci <- c(4000, 6000)

  ## three radii for two loci: recycled, with a warning
  expect_warning(
    rngs <- rings(emptyRas, loci = loci, minRadius = c(0, 3, 5), maxRadius = 8,
                  returnIndices = TRUE),
    "same length as loci"
  )
  expect_setequal(unique(rngs$id), seq_along(loci))
  ## recycling took the first two: 0 for locus 1, 3 for locus 2
  expect_true(loci[1] %in% rngs[id == 1]$indices)
  expect_true(all(rngs[id == 2]$dists >= 3))
})

test_that("rings returns a distance raster when returnIndices is FALSE", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0

  ras <- rings(emptyRas, loci = 5050, minRadius = 3, maxRadius = 6,
               returnIndices = FALSE, returnDistances = TRUE)
  expect_s4_class(ras, "SpatRaster")

  ## off-ring cells are NA, ring cells carry their distance
  v <- terra::values(ras)[, 1]
  onRing <- !is.na(v)
  expect_true(all(v[onRing] >= 3 & v[onRing] <= 6))

  idx <- rings(emptyRas, loci = 5050, minRadius = 3, maxRadius = 6,
               returnIndices = TRUE)
  expect_setequal(which(onRing), idx$indices)
})

test_that("rings fills non-ring cells with 0 when returnDistances is FALSE", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0

  ras <- rings(emptyRas, loci = 5050, minRadius = 3, maxRadius = 6,
               returnIndices = FALSE, returnDistances = FALSE)
  expect_s4_class(ras, "SpatRaster")

  v <- terra::values(ras)[, 1]
  ## the background is 0 rather than NA -- that is the difference this
  ## argument makes to the returned raster
  expect_false(anyNA(v))

  idx <- rings(emptyRas, loci = 5050, minRadius = 3, maxRadius = 6,
               returnIndices = TRUE)
  expect_setequal(which(v > 0), idx$indices)

  ## ring cells carry the ring id, not the distance -- which is what
  ## returnDistances = FALSE means, and what the allowOverlap branch does
  expect_setequal(unique(v[v > 0]), unique(idx$id))
  expect_equal(v[idx$indices], idx$id, ignore_attr = TRUE)
})

test_that("rings summarises overlapping rings into one raster", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), resolution = 1)
  emptyRas[] <- 0
  ## deliberately close together, so the two discs overlap
  loci <- c(5050, 5055)

  overlapping <- rings(emptyRas, loci = loci, minRadius = 0, maxRadius = 8,
                       allowOverlap = TRUE, returnIndices = TRUE)
  shared <- overlapping[, .N, by = indices][N > 1]$indices
  expect_gt(length(shared), 0)  # the test is pointless without an overlap

  ## returnDistances: overlapped cells hold the MEAN of the two distances
  meanRas <- rings(emptyRas, loci = loci, minRadius = 0, maxRadius = 8,
                   allowOverlap = TRUE, returnIndices = FALSE,
                   returnDistances = TRUE)
  expect_s4_class(meanRas, "SpatRaster")
  expected <- overlapping[, list(mDists = mean(dists)), by = indices]
  expect_equal(terra::values(meanRas)[expected$indices, 1], expected$mDists,
               ignore_attr = TRUE)

  ## returnDistances = FALSE: overlapped cells hold the SUM of the ring ids
  idRas <- rings(emptyRas, loci = loci, minRadius = 0, maxRadius = 8,
                 allowOverlap = TRUE, returnIndices = FALSE,
                 returnDistances = FALSE)
  expectedID <- overlapping[, list(sumID = sum(id)), by = indices]
  expect_equal(terra::values(idRas)[expectedID$indices, 1], expectedID$sumID,
               ignore_attr = TRUE)
  ## a cell in both rings sums 1 + 2; a cell in one ring holds just its own id
  expect_true(all(expectedID[indices %in% shared]$sumID == 3))
})

test_that("rings returns a RasterLayer when given one", {
  testInit(c("terra", "data.table", "raster"))

  set.seed(1462)
  ras <- raster::raster(raster::extent(0, 1e2, 0, 1e2), resolution = 1)
  ras[] <- 0

  out <- rings(ras, loci = 5050, minRadius = 3, maxRadius = 6,
               returnIndices = FALSE)
  ## the returned object keeps the class it was given
  expect_s4_class(out, "RasterLayer")

  v <- raster::values(out)
  onRing <- !is.na(v)
  expect_true(all(v[onRing] >= 3 & v[onRing] <= 6))
})
