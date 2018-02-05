test_that("randomPolygon: does not work properly", {
  library(sp)
  library(raster)

  on.exit({
    detach("package:raster")
    detach("package:sp")
  }, add = TRUE)

  area <- 1e4
  poly1 <- randomPolygon(cbind(-110, 59), area)
  areaCRS <- CRS(paste0("+proj=lcc +lat_1=", ymin(poly1), " +lat_2=", ymax(poly1),
                        " +lat_0=0 +lon_0=", xmin(poly1), " +x_0=0 +y_0=0 +ellps=GRS80",
                        " +units=m +no_defs"))

  if (interactive())
    plot(poly1)

  if (requireNamespace("rgeos"))
    expect_true(abs(abs(rgeos::gArea(spTransform(poly1, areaCRS))/1e4 - area)) <  area/4)


}
)
