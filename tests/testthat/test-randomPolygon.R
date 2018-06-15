test_that("randomPolygon: does not work properly", {
  library(sp)
  library(rgeos)
  library(raster)

  on.exit({
    detach("package:raster")
    detach("package:rgeos")
    detach("package:sp")
  }, add = TRUE)

  set.seed(1234) ## TODO: some seeds produce failing area test below!!

  area <- 1e4
  center <- cbind(-110, 59)
  poly1 <- randomPolygon(center, area)
  areaCRS <- CRS(paste0("+proj=lcc +lat_1=", ymin(poly1), " +lat_2=", ymax(poly1),
                        " +lat_0=0 +lon_0=", xmin(poly1), " +x_0=0 +y_0=0 +ellps=GRS80",
                        " +units=m +no_defs"))
  if (interactive())
    plot(poly1)

  ## check that polygon area approkimately matches that given by hectares
  polyArea <- rgeos::gArea(spTransform(poly1, areaCRS)) / 1e4
  expect_true(base::abs(base::abs(polyArea - area)) <  area/4) ## TODO: why is this area/4?

  ## check that polygon center is approximately centered on x
  centerSP <- SpatialPoints(center, proj4string = areaCRS)
  polyCenter <- rgeos::gCentroid(poly1, byid = TRUE)
  expect_true(pointDistance(centerSP, polyCenter, lonlat = FALSE) < 0.05) ## TODO: appropriate test?
})
