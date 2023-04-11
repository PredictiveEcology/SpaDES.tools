test_that("randomPolygon: does not work properly", {
  library(terra)

  set.seed(1234) ## TODO: some seeds produce failing area test below!!
  latLong <- crs("+init=epsg:4326")

  area <- 1e4
  center <- cbind(-110, 59)
  poly1 <- randomPolygon(center, area = area)
  if (interactive()) {
    plot(poly1)
  }

  poly1InUTM <- project(poly1, utmCRS(poly1))
  ## check that polygon area approximately matches that given by hectares
  polyArea <- expanse(poly1InUTM)
  expect_true(base::abs(base::abs(polyArea - area)) <  area/4) ## TODO: why is this area/4?

  ## check that polygon center is approximately centered on x
  centerSP <- vect(center, crs = latLong)
  centerSP_UTM <- project(centerSP, crs(poly1InUTM))
  polyCenter <- terra::centroids(poly1InUTM)
  expect_true(terra::distance(centerSP_UTM, polyCenter) < 100) ## TODO: appropriate test?
})
