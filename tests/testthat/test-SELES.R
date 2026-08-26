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

test_that("agentLocation converts zeros to NA for rasters and passes sp classes through", {
  testInit("raster")  ## raster attaches sp itself; attaching both breaks teardown

  ## Raster* input: zeros become NA
  r <- raster::raster(matrix(c(0, 1, 2, 0), nrow = 2))
  got <- agentLocation(r)
  expect_s4_class(got, "RasterLayer")
  expect_identical(sum(is.na(raster::getValues(got))), 2L)
  expect_setequal(stats::na.omit(raster::getValues(got)), c(1, 2))

  ## SpatialPoints and SpatialPolygons pass through untouched
  pts <- sp::SpatialPoints(cbind(1:3, 1:3))
  expect_identical(agentLocation(pts), pts)

  sq <- sp::Polygons(list(sp::Polygon(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))), "a")
  poly <- sp::SpatialPolygons(list(sq))
  expect_identical(agentLocation(poly), poly)

  ## anything else is refused
  expect_error(agentLocation(1:10), "only raster, Spatialpoints or SpatialPolygons")
})

test_that("numAgents returns N and validates it", {
  expect_identical(numAgents(10, probInit = NULL), 10)
  expect_error(numAgents(c(1, 2), probInit = NULL))
  expect_error(numAgents("ten", probInit = NULL))
})

test_that("transitions is defunct", {
  ## Never fully implemented: its body assigned into sp::coordinates(), which
  ## errors on any Spatial* object whose coordinates are already set, so no
  ## caller can have depended on it. Now signals an error explicitly.
  expect_error(transitions(p = 1, agent = NULL), "defunct")
})

test_that("patchSize counts cells per patch", {
  testInit("terra")

  ras <- terra::rast(terra::ext(0, 4, 0, 4), res = 1)
  terra::values(ras) <- c(rep(1, 6), rep(2, 10))

  got <- patchSize(ras)
  expect_true(is.data.frame(got) || is.matrix(got))
  counts <- as.data.frame(got)
  expect_setequal(counts$count, c(6, 10))
})
