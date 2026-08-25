test_that("probInit builds a probability surface from a map", {
  testInit(c("terra", "data.table"))

  map <- terra::rast(system.file("extdata", "map.tif", package = "SpaDES.tools"))
  names(map) <- "layer"

  pr <- probInit(map, p = (map[] / terra::minmax(map)[2]) ^ 2)

  expect_s4_class(pr, "SpatRaster")
  expect_identical(terra::ncell(pr), terra::ncell(map))

  ## probabilities, so bounded to [0, 1]
  vals <- terra::values(pr)[, 1]
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))

  ## monotone in the map it was derived from: higher map value, higher p
  mapVals <- terra::values(map)[, 1]
  keep <- !is.na(vals) & !is.na(mapVals)
  expect_gt(stats::cor(mapVals[keep], vals[keep]), 0.9)
})

test_that("initiateAgents places the requested number of agents", {
  testInit(c("terra", "data.table"))

  map <- terra::rast(system.file("extdata", "map.tif", package = "SpaDES.tools"))
  names(map) <- "layer"
  pr <- probInit(map, p = (map[] / terra::minmax(map)[2]) ^ 2)

  set.seed(1)
  agents <- initiateAgents(map, 100, pr, asSpatialPoints = "sf")

  expect_s4_class(agents, "SpatVector")
  expect_identical(terra::geomtype(agents), "points")
  expect_equal(nrow(agents), 100)

  ## agents land on the map, not off it
  expect_true(terra::relate(agents, terra::as.polygons(terra::ext(map)),
                            relation = "intersects") |> all())

  ## a raster is returned when points are not requested
  set.seed(1)
  asRas <- initiateAgents(map, 50, pr, asSpatialPoints = FALSE)
  expect_s4_class(asRas, "SpatRaster")
})

test_that("initiateAgents follows the supplied probabilities", {
  testInit(c("terra", "data.table"))

  map <- terra::rast(system.file("extdata", "map.tif", package = "SpaDES.tools"))
  names(map) <- "layer"
  pr <- probInit(map, p = (map[] / terra::minmax(map)[2]) ^ 2)

  ## The documented example makes this point by eye, with a plot. Assert it
  ## instead: selection rate should rise with map value, since p goes as the
  ## square of the (scaled) map value.
  set.seed(42)
  agents <- initiateAgents(map, 1000, pr, asSpatialPoints = "sf")

  dt1 <- data.table::data.table(
    table(round(terra::extract(map, agents), 0)[, "layer"]))
  data.table::setnames(dt1, old = "N", new = "count")
  dt2 <- data.table::data.table(table(round(map[], 0)))
  data.table::setnames(dt2, old = "N", new = "available")

  dt <- dt1[dt2, on = "V1"]
  data.table::setnames(dt, old = "V1", new = "mapValue")
  dt[, selection := count / available]
  dt[is.na(selection), selection := 0]
  dt[, mapValue := as.numeric(mapValue)]

  expect_gt(stats::cor(dt$mapValue, dt$selection, use = "complete.obs"), 0.5)
})
