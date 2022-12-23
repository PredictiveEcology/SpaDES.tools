test_that("neutralLandscapeMap produces consistent rasters", {
  skip_if_not_installed("NLMR", "1.1.1")

  library(raster)

  n <- 20
  nx <- sample.int(300, n)
  ny <- sample.int(300, n)
  rlist <- list()
  crslist <- list(
    latlon = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
    lcc = paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                 "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
    aea = paste("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0",
                "+datum=NAD83 +units=m +no_defs +type=crs")
  )

  for (i in seq_along(n)) {
    r <- raster(nrows = ny[i], ncols = nx[i],
                xmn = -nx[i]/2, xmx = nx[i]/2,
                ymn = -ny[i]/2, ymx = ny[i]/2)

    ## give some a projection
    if (i %in% seq_along(length(crslist))) {
      crs(r) <- crslist[[i]]
    }

    rlist[[i]] <- suppressWarnings({
      neutralLandscapeMap(r, roughness = 0.65, rand_dev = 100, rescale = FALSE, verbose = FALSE)
    })

    expect_identical(ncol(rlist[[i]]), nx[i])
    expect_identical(nrow(rlist[[i]]), ny[i])
    expect_false(any(is.na(rlist[[i]][])))
    expect_true(compareRaster(r, rlist[[i]], res = TRUE, orig = TRUE))
  }
})
