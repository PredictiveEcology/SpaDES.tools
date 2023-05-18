test_that("mergeRaster will return a message if tiles are resampled", {

  df <- data.frame(pkg = c("raster", "terra"),
                   cls = c("RasterLayer", "SpatRaster"),
                   read = c("raster::raster", "terra::rast"))

  nx <- ny <- 3
  for (i in seq(NROW(df))) {
    pkg <- df$pkg[i]
    cls <- df$cls[i]
    read <- df$read[i]

    withr::local_package(pkg)
    withr::local_options(reproducible.rasterRead = read)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs, list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                      resolution = c(30, 30),
                                      vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(reproducible::rasterRead, rastArgs)

    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(3, 3))
    expect_is(splitted, "list")
    expect_length(splitted, nx * ny)

    splitted <- lapply(X = seq_along(splitted), FUN = function(tiles, pkg, ras) {
      rastArgs <- list(xmn = xmin(splitted[[tiles]]),
                       xmx = xmax(splitted[[tiles]]),
                       ymn = ymin(splitted[[tiles]]),
                       ymx = ymax(splitted[[tiles]]))
      if (pkg == "terra") {
        names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
      }
      rastArgs <- append(rastArgs, list(resolution = c(250, 250), crs = crs(splitted[[tiles]])))
      y <- do.call(reproducible::rasterRead, rastArgs)

      expect_false(any(res(y) == res(ras)))
      r <- terra::resample(x = ras, y = y)  ## always use `terra`, it's compatible with RasterLayer - getFromNameSpace("resample", "raster") doesn't work.
      return(r)
    }, pkg = pkg, ras = ras)

    expect_is(splitted, "list")
    expect_message({
      merged <- mergeRaster(x = splitted)
    })
    expect_is(merged, cls)
  }
})

test_that("mergeRaster will produce a raster layer", {

  df <- data.frame(pkg = c("raster", "terra"),
                   cls = c("RasterLayer", "SpatRaster"),
                   read = c("raster::raster", "terra::rast"))

  nx <- ny <- 3
  for (i in seq(NROW(df))) {
    pkg <- df$pkg[i]
    cls <- df$cls[i]
    read <- df$read[i]

    withr::local_package(pkg)
    withr::local_options(reproducible.rasterRead = read)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs, list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                      resolution = c(30, 30),
                                      vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(reproducible::rasterRead, rastArgs)

    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(5, 5))
    expect_is(splitted, "list")
    expect_length(splitted, nx * ny)
    merged <- mergeRaster(x = splitted)
    expect_is(merged, cls)
  }


})

test_that("mergeRaster will produce error if only one raster passed", {
  df <- data.frame(pkg = c("raster", "terra"),
                   read = c("raster::raster", "terra::rast"))

  for (i in seq(NROW(df))) {
    pkg <- df$pkg[i]
    read <- df$read[i]

    withr::local_package(pkg)
    withr::local_options(reproducible.rasterRead = read)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs, list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                      resolution = c(30, 30),
                                      vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(reproducible::rasterRead, rastArgs)
    expect_error({
      merged <- mergeRaster(x = ras)
    })
  }
})


test_that("mergeRaster will use mosaic with default mean if rasters are resampled and fun if passed", {
  df <- data.frame(pkg = c("raster", "terra"),
                   read = c("raster::raster", "terra::rast"))

  nx <- ny <- 3

  for (i in seq(NROW(df))) {
    pkg <- df$pkg[i]
    cls <- df$cls[i]
    read <- df$read[i]

    withr::local_package(pkg)
    withr::local_options(reproducible.rasterRead = read)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs, list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                      resolution = c(30, 30),
                                      vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(reproducible::rasterRead, rastArgs)


    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(100, 100))
    expect_is(splitted, "list")
    expect_length(splitted, nx * ny)
    splitted <- lapply(X = seq_along(splitted), FUN = function(tiles, pkg) {
      rastArgs <- list(xmn = xmin(splitted[[tiles]]),
                       xmx = xmax(splitted[[tiles]]),
                       ymn = ymin(splitted[[tiles]]),
                       ymx = ymax(splitted[[tiles]]))
      if (pkg == "terra") {
        names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
      }
      rastArgs <- append(rastArgs, list(resolution = c(250, 250),
                                        crs = raster::crs(splitted[[tiles]])))
      y <- do.call(reproducible::rasterRead, rastArgs)
      expect_false(any(res(y) == res(ras)))
      r <- terra::resample(x = ras, y = y) ## use terra always. compatible with RasterLayer
      return(r)
    }, pkg = pkg)
    expect_is(splitted, "list")
    expect_message({
      merged <- mergeRaster(x = splitted)
    })
    expect_message({
      merged2 <- mergeRaster(x = splitted, fun = max)
    })
    expect_is(merged, cls)
    expect_is(merged2, cls)
  }
})
