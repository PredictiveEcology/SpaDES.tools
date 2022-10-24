test_that("neutralLandscapeMap produces consistent rasters", {
  skip_if_not_installed("NLMR", "1.1.1")

  library(raster)

  n <- 20
  nx <- sample.int(300, n)
  ny <- sample.int(300, n)
  rlist <- list()

  for (i in seq_along(n)) {
    r <- raster(nrows = ny[i], ncols = nx[i],
                xmn = -nx[i]/2, xmx = nx[i]/2,
                ymn = -ny[i]/2, ymx = ny[i]/2)
    rlist[[i]] <- suppressWarnings({
      neutralLandscapeMap(r, roughness = 0.65, rand_dev = 200, rescale = FALSE, verbose = FALSE)
    })

    expect_identical(ncol(rlist[[i]]), nx[i])
    expect_identical(nrow(rlist[[i]]), ny[i])
  }
})
