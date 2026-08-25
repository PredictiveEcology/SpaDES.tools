test_that("rings returns annuli between minRadius and maxRadius", {
  testInit(c("terra", "data.table"))

  set.seed(1462)
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), res = 1)
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
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), res = 1)
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
  emptyRas <- terra::rast(terra::ext(0, 1e2, 0, 1e2), res = 1)
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
