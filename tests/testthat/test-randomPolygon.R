test_that("randomPolygon: does not work properly", {
  library(sp)
  library(rgeos)
  library(raster)

  on.exit({
    detach("package:raster")
    detach("package:rgeos")
  }, add = TRUE)

  set.seed(1234) ## TODO: some seeds produce failing area test below!!
  latLong <-   sp::CRS("+init=epsg:4326")

  area <- 1e4
  center <- cbind(-110, 59)
  poly1 <- randomPolygon(center, area = area)
  if (interactive()) {
    plot(poly1)
  }

  poly1InUTM <- sp::spTransform(poly1, utmCRS(poly1))
  ## check that polygon area approximately matches that given by hectares
  polyArea <- rgeos::gArea(poly1InUTM)
  expect_true(base::abs(base::abs(polyArea - area)) <  area/4) ## TODO: why is this area/4?

  ## check that polygon center is approximately centered on x
  centerSP <- sp::SpatialPoints(center, proj4string = latLong)
  centerSP_UTM <- sp::spTransform(centerSP, CRSobj = raster::crs(poly1InUTM))
  polyCenter <- rgeos::gCentroid(poly1InUTM, byid = TRUE)
  expect_true(raster::pointDistance(centerSP_UTM, polyCenter, lonlat = FALSE) < 100) ## TODO: appropriate test?
})
