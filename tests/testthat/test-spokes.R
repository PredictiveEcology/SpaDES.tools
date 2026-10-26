test_that("spokes casts rays at the requested angles from each starting point", {
  testInit(c("terra", "data.table"))

  set.seed(1234)
  ras <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1, vals = 0)
  rp <- randomPolygons(ras, numTypes = 10)

  angles <- seq(0, pi * 2, length.out = 17)
  angles <- angles[-length(angles)]
  loci <- sample(terra::ncell(rp), 2L)
  coords <- terra::vect(terra::xyFromCell(rp, loci))
  stopRule <- function(landscape) landscape < 3

  ## the function announces itself as experimental
  expect_message(
    out <- spokes(rp, coords = coords, stopRule = stopRule,
                  minRadius = 0, maxRadius = 50,
                  returnAngles = TRUE, returnDistances = TRUE,
                  allowOverlap = TRUE, angles = angles, returnIndices = TRUE),
    "experimental"
  )

  expect_true(is.matrix(out))
  expect_true(all(c("id", "angles", "x", "y", "indices", "dists", "stop") %in%
                    colnames(out)))

  ## every requested angle is represented, and no others. Compared with a
  ## tolerance rather than setequal: the returned angles round-trip through
  ## the ray computation and come back differing in the last ulp.
  expect_equal(sort(unique(out[, "angles"])), sort(angles))

  ## rays start at the given points, so one id per starting coordinate
  expect_setequal(unique(out[, "id"]), seq_along(loci))

  ## distances stay within the requested radii
  expect_true(all(out[, "dists"] >= 0 & out[, "dists"] <= 50))

  ## returned cells are real cell numbers on the input raster
  idx <- out[, "indices"]
  idx <- idx[!is.na(idx)]
  expect_true(all(idx >= 1 & idx <= terra::ncell(rp)))
})

test_that("spokes honours minRadius and is reproducible", {
  testInit(c("terra", "data.table"))

  set.seed(1234)
  ras <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1, vals = 0)
  rp <- randomPolygons(ras, numTypes = 10)
  angles <- seq(0, pi * 2, length.out = 9)[-9]
  coords <- terra::vect(terra::xyFromCell(rp, 55L))
  stopRule <- function(landscape) landscape < 3

  set.seed(7)
  a <- suppressMessages(spokes(rp, coords = coords, stopRule = stopRule,
                               minRadius = 2, maxRadius = 50, angles = angles,
                               returnDistances = TRUE, returnIndices = TRUE,
                               allowOverlap = TRUE))
  set.seed(7)
  b <- suppressMessages(spokes(rp, coords = coords, stopRule = stopRule,
                               minRadius = 2, maxRadius = 50, angles = angles,
                               returnDistances = TRUE, returnIndices = TRUE,
                               allowOverlap = TRUE))
  expect_equal(a, b)

  ## nothing closer than minRadius comes back (bar floating-point dust)
  expect_true(all(a[, "dists"] >= 2 - 1e-8))
})
