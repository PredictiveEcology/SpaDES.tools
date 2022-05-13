test_that("mergeRaster will return a message if tiles are resampled", {
  library(raster)
  library(magrittr)
  nx = 3
  ny = 3
  ras <- raster::raster(xmn = -30^2, xmx = 30^2,
                        ymn = -60^2, ymx = 60^2,
                        resolution = c(30, 30),
                        crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                        vals = round(runif(n = 14400 , min = 1, max = 10)))
  splitted <- SpaDES.tools::splitRaster(r = ras, nx = nx, ny = ny, buffer = c(100, 100))
  expect_is(splitted, "list")
  expect_length(splitted, nx*ny)
  splitted <- lapply(X = 1:length(splitted), FUN = function(tiles) {
    y <- raster::raster(xmn = raster::xmin(splitted[[tiles]]),
                        xmx = raster::xmax(splitted[[tiles]]),
                        ymn = raster::ymin(splitted[[tiles]]),
                        ymx = raster::ymax(splitted[[tiles]]),
                        resolution = c(250, 250),
                        crs = raster::crs(splitted[[tiles]]))
    expect_false(any(raster::res(y) == raster::res(ras)))
    r <- raster::resample(x = ras, y = y)
    return(r)
  })
  expect_is(splitted, "list")
  expect_message({
    merged <- mergeRaster(x = splitted)
  })
  expect_is(merged, "RasterLayer")
})

test_that("mergeRaster will produce a raster layer", {
  library(raster)
  library(magrittr)
  nx = 3
  ny = 3
  ras <- raster::raster(xmn = -30^2, xmx = 30^2,
                        ymn = -60^2, ymx = 60^2,
                        resolution = c(30, 30),
                        crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                        vals = round(runif(n = 14400 , min = 1, max = 10)))
  splitted <- SpaDES.tools::splitRaster(r = ras, nx = nx, ny = ny, buffer = c(100, 100))
  expect_is(splitted, "list")
  expect_length(splitted, nx*ny)
  merged <- mergeRaster(x = splitted)
  expect_is(merged, "RasterLayer")
})

test_that("mergeRaster will produce error if only one raster passed", {
  ras <- raster::raster(xmn = -30^2, xmx = 30^2,
                        ymn = -60^2, ymx = 60^2,
                        resolution = c(30, 30),
                        crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                        vals = round(runif(n = 14400 , min = 1, max = 10)))
  expect_error({
    merged <- mergeRaster(x = ras)
  })
})

test_that("mergeRaster will use mosaic with default mean if rasters are resampled and fun if passed", {
  library(raster)
  library(magrittr)
  nx = 3
  ny = 3
  ras <- raster::raster(xmn = -30^2, xmx = 30^2,
                        ymn = -60^2, ymx = 60^2,
                        resolution = c(30, 30),
                        crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                        vals = round(runif(n = 14400 , min = 1, max = 10)))
  splitted <- SpaDES.tools::splitRaster(r = ras, nx = nx, ny = ny, buffer = c(100, 100))
  expect_is(splitted, "list")
  expect_length(splitted, nx*ny)
  splitted <- lapply(X = 1:length(splitted), FUN = function(tiles){
    y <- raster::raster(xmn = raster::xmin(splitted[[tiles]]),
                        xmx = raster::xmax(splitted[[tiles]]),
                        ymn = raster::ymin(splitted[[tiles]]),
                        ymx = raster::ymax(splitted[[tiles]]),
                        resolution = c(250, 250),
                        crs = raster::crs(splitted[[tiles]]))
    expect_false(any(raster::res(y) == raster::res(ras)))
    r <- raster::resample(x = ras, y = y)
    return(r)
  })
  expect_is(splitted, "list")
  expect_message({
    merged <- mergeRaster(x = splitted)
  })
  expect_message({
    merged2 <- mergeRaster(x = splitted, fun = max)
  })
  expect_is(merged, "RasterLayer")
  expect_is(merged2, "RasterLayer")
})
