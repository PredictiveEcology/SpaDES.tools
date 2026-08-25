test_that("randomPolygon: does not work properly", {
  # no need for `raster` testing as this is using randomPolygon
  testInit("terra")

  set.seed(1234) ## TODO: some seeds produce failing area test below!!
  # latLong <- crs("epsg:4326")
  latLong <- terra::crs("+proj=longlat +datum=WGS84 +no_defs")

  area <- 1e4
  center <- cbind(-110, 59)
  poly1 <- randomPolygon(center, area = area)
  if (interactive()) {
    terra::plot(poly1)
  }

  poly1InUTM <- project(poly1, utmCRS(poly1))
  ## check that polygon area approximately matches that given by hectares
  polyArea <- expanse(poly1InUTM)

  ## scale the test to area. This means in current area of 1e4, polygon area should be more than 2500; reasonable
  expect_true(base::abs(base::abs(polyArea - area)) <  (area / 4))  ## TODO: why is this area/4?

  ## check that polygon center is approximately centered on x
  centerSP <- vect(center, crs = latLong)
  centerSP_UTM <- project(centerSP, crs(poly1InUTM))
  polyCenter <- terra::centroids(poly1InUTM)

  ## scale the test to area. This means in current area of 1e4, that centroid is 100 m from center; reasonable
  expect_true(terra::distance(centerSP_UTM, polyCenter) < (area/100))
})

test_that("randomPolygon: deprecated sp methods delegate to the SpatVector path", {
  testInit(c("terra", "sp", "sf"))

  latLong <- terra::crs("+proj=longlat +datum=WGS84 +no_defs")
  area <- 1e7

  ## SpatialPoints ----------------------------------------------------------
  spPts <- sp::SpatialPoints(cbind(-110, 59),
                             proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

  set.seed(321)
  expect_warning(polySP <- randomPolygon(spPts, area = area), "deprecated")

  expect_s4_class(polySP, "SpatialPolygons")
  expect_identical(length(polySP), 1L)

  ## same area check the SpatVector path is held to
  polySPUTM <- terra::project(terra::vect(polySP), utmCRS(terra::vect(polySP)))
  expect_true(abs(terra::expanse(polySPUTM) - area) < (area / 4))

  ## The sp entry point must consume the RNG exactly as the SpatVector one
  ## does, since it is now the same code: same seed, same polygon.
  set.seed(321)
  polyTerra <- randomPolygon(terra::vect(cbind(-110, 59), crs = latLong), area = area)
  expect_equal(terra::crds(terra::vect(polySP)), terra::crds(polyTerra),
               tolerance = 1e-6)

  ## SpatialPolygons -------------------------------------------------------
  spPoly <- sf::st_polygon(list(cbind(
    x = c(-122.98, -116.1, -99.2, -106, -122.98),
    y = c(59.9, 65.73, 63.58, 54.79, 59.9)
  ))) |>
    sf::st_sfc(crs = "EPSG:4326") |>
    sf::st_sf(geometry = _) |>
    as("Spatial")

  set.seed(654)
  expect_warning(polyFromPoly <- randomPolygon(spPoly, area = 1e10), "deprecated")

  expect_s4_class(polyFromPoly, "SpatialPolygons")
  ## the rejection loop guarantees the result sits inside the input polygon
  expect_true(terra::is.related(terra::vect(polyFromPoly), terra::vect(spPoly),
                                relation = "intersects"))
})

test_that("randomPolygon: deprecated `hectares` scaling is preserved per method", {
  testInit(c("terra", "sp", "sf"))

  spPts <- sp::SpatialPoints(cbind(-110, 59),
                             proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

  ## SpatialPoints scales hectares by 1e4; passing the scaled value as `area`
  ## must therefore give the same polygon for the same seed.
  set.seed(99)
  suppressWarnings(byHectares <- randomPolygon(spPts, hectares = 1e3))
  set.seed(99)
  suppressWarnings(byArea <- randomPolygon(spPts, area = 1e3 * 1e4))

  expect_equal(terra::crds(terra::vect(byHectares)), terra::crds(terra::vect(byArea)),
               tolerance = 1e-6)
})
