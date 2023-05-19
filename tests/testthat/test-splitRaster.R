test_that("splitRaster and mergeRaster work on small in-memory rasters", {
  # withr::local_package("reproducible")
  withr::local_package("tools")
  rastDF <- needTerraAndRaster() #
  testFiles = data.frame(pkg = c("raster", "terra"),
                         testFile = c("external/rlogo.grd", "ex/logo.tif"))
  rastDF <- merge(testFiles, rastDF)

  data.table::setDTthreads(1)

  for (ii in seq(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))
    extFun <- eval(parse(text = rastDF$ext[ii]))

    testFile <- system.file(rastDF$testFile[ii], package = pkg)

    withr::local_package(pkg)
    # withr::local_options(reproducible.rasterRead = read)

    owd <- getwd()
    on.exit({
      setwd(owd)
    }, add = TRUE)

    tmpdir <- file.path(tempdir(), "splitRaster-test", pkg) |> checkPath(create = TRUE)
    setwd(tmpdir)

    b <- read(testFile)
    r <- b[[1]] # use first layer only
    nx <- 3
    ny <- 4
    expect_equal(terra::xres(r), 1)
    expect_equal(terra::yres(r), 1)

    # change the extent of r
    extnt(r) <- extnt(xmin(r) - 30, xmax(r) - 30, ymin(r) - 20, ymax(r) - 20)

    # no buffer
    y0 <- splitRaster(r, nx, ny, path = file.path(tmpdir, "red"))
    expect_equal(class(y0), "list")
    expect_false(unique(unlist(lapply(y0, inMemory))))

    for (i in 1:12) {
      expect_true(file.exists(file.path(tmpdir, "red", paste0("red_tile", i, ".tif"))))
    }

    xextents <- c()
    yextents <- c()
    for (i in seq_along(y0)) {
      xextents <- c(xextents, xmin(y0[[i]]), xmax(y0[[i]]))
      yextents <- c(yextents, ymin(y0[[i]]), ymax(y0[[i]]))
    }
    expect_equal(sort(unique(xextents)), c(-30, 4, 37, 71))
    expect_equal(sort(unique(yextents)), c(-20, -1, 19, 38, 57))
    rm(xextents, yextents)

    expect_equal(length(unique(lapply(y0, crs))), 1L)
    expect_equal(unique(lapply(y0, crs))[[1]], crs(r))

    m0 <- mergeRaster(y0)
    expect_equal(dim(m0), dim(r))
    expect_equal(extnt(m0), extnt(r))
    expect_equal(res(m0), res(r))
    expect_equal(max(values(m0)), max(values(r)))
    expect_equal(min(values(m0)), min(values(r)))

    # as a stack/brick
    if (requireNamespace("purrr", quietly = TRUE)) {
      if (pkg == "raster") {
        ## ensure the raster method for as.list used (instead of base version)
        asRasterList <- selectMethod("as.list", "Raster")
        ys0 <- lapply(purrr::transpose(lapply(X = asRasterList(b),
                                              FUN = splitRaster, nx = nx, ny = ny)),
                      raster::stack)
      } else if (pkg == "terra") {
        ys0 <- lapply(purrr::transpose(lapply(X = as.list(b),
                                              FUN = splitRaster, nx = nx, ny = ny)),
                      terra::rast)
      }
      ms0 <- mergeRaster(ys0)
      expect_identical(names(ms0), names(ys0[[1]]))
    }

    # with buffer (integer pixels) and with specified path
    y1 <- splitRaster(r, nx, ny, c(3L, 4L), path = file.path(tmpdir, "red1"))
    expect_false(unique(unlist(lapply(y1, inMemory))))

    for (i in 1:12) {
      expect_true(file.exists(file.path(tmpdir, "red1", paste0("red_tile", i, ".tif"))))
    }

    xextents <- c()
    yextents <- c()
    for (i in seq_along(y1)) {
      xextents <- c(xextents, xmin(y1[[i]]), xmax(y1[[i]]))
      yextents <- c(yextents, ymin(y1[[i]]), ymax(y1[[i]]))
    }
    expect_equal(sort(unique(xextents)), c(-30, 1, 7, 34, 40, 71))
    expect_equal(sort(unique(yextents)), c(-20, -5, 3, 15, 23, 34, 42, 57))
    rm(xextents, yextents)

    expect_equal(length(unique(lapply(y1, crs))), 1L)
    expect_equal(unique(lapply(y1, crs))[[1]], crs(r))

    m1 <- mergeRaster(y1)
    expect_equal(dim(m1), dim(r))
    expect_equal(extnt(m1), extnt(r))
    expect_equal(res(m1), res(r))
    expect_equal(max(values(m1)), max(values(r)))
    expect_equal(min(values(m1)), min(values(r)))

    # with no path specified, file is in memory
    y1 <- splitRaster(r, nx, ny, c(3L, 4L))

    for (i in 1:12) {
      expect_false(file.exists(reproducible::Filenames(y1[[i]])))
    }

    # with buffer (proportion of cells)
    y2 <- splitRaster(r, nx, ny, c(0.5, 0.3), path = file.path(tmpdir, "red2"))
    xextents <- c()
    yextents <- c()
    for (i in seq_along(y2)) {
      xextents <- c(xextents, xmin(y2[[i]]), xmax(y2[[i]]))
      yextents <- c(yextents, ymin(y2[[i]]), ymax(y2[[i]]))
    }
    expect_equal(sort(unique(xextents)), c(-30, -13, 20, 21, 54, 71))
    expect_equal(sort(unique(yextents)), c(-20, -7, 5, 13, 25, 32, 44, 57))
    rm(xextents, yextents)

    expect_equal(length(unique(lapply(y2, crs))), 1L)
    expect_equal(unique(lapply(y2, crs))[[1]], crs(r))

    m2 <- mergeRaster(y2)
    expect_equal(dim(m2), dim(r))
    expect_equal(extnt(m2), extnt(r))
    expect_equal(res(m2), res(r))
    expect_equal(max(values(m2)), max(values(r)))
    expect_equal(min(values(m2)), min(values(r)))

    # different raster resolutions
    r1 <- r
    res(r1) <- c(5, 6) # no values assigned
    r1[] <- 1:ncell(r1)
    y3 <- splitRaster(r, nx, ny)
    rows <- 1:ny
    for (j in c(0, 4, 8)) {
      colmin <- c()
      colmax <- c()
      for (i in rows + j) {
        colmin <- unique(c(colmin, xmin(y3[[i]])))
        colmax <- unique(c(colmax, xmax(y3[[i]])))
      }
      if (j > 0) {
        expect_true(colmin == colmaxtemp)
      }
      colmaxtemp <- colmax
    }

    cols <- c(1, 5, 9)
    for (j in 0:3) {
      rowmin <- c()
      rowmax <- c()
      for (i in cols + j) {
        rowmin <- unique(c(rowmin, ymin(y3[[i]])))
        rowmax <- unique(c(rowmax, ymax(y3[[i]])))
      }
      if (j > 0) {
        expect_true(rowmin == rowmaxtemp)
      }
      rowmaxtemp <- rowmax
    }

    ## compatibility with different raster datatypes
    y4 <- splitRaster(r, nx, ny, rType = "INT1U")
    if (pkg == "raster") {
      expect_identical(reproducible::dataType2(y4[[1]]), "INT1U")
    } else if (pkg == "terra") {
      ## in-memory SpatRasters have empty datatype
      expect_identical(reproducible::dataType2(y4[[1]]), "")
      expect_true(terra::is.int(y4[[1]]))
    }

    expect_warning({
      y5 <- splitRaster(r, nx, ny, rType = "INT") # INT invalid; defaults to INT4S
    }, "INT is not a valid datatype")
    if (pkg == "raster") {
      expect_identical(reproducible::dataType2(y5[[1]]), "INT4S")
    } else if (pkg == "terra") {
      expect_identical(reproducible::dataType2(y5[[1]]), "")
      expect_true(terra::is.int(y5[[1]]))
    }

    y6 <- splitRaster(r, nx, ny) # defaults to FLT4S
    if (pkg == "raster") {
      expect_identical(reproducible::dataType2(y6[[1]]), "FLT4S")
    } else if (pkg == "terra") {
      expect_identical(reproducible::dataType2(y6[[1]]), "")
      expect_true(terra::is.int(y6[[1]]))
    }

    ## use different file extensions
    y7 <- splitRaster(r, nx, ny, path = tmpdir, fExt = ".grd")
    expect_true(all(tools::file_ext(reproducible::Filenames(y7[[1]])) %in% c("grd", "gri")))

    y8 <- splitRaster(r, nx, ny, path = tmpdir, fExt = ".tif")
    expect_true(tools::file_ext(reproducible::Filenames(y8[[1]])) == "tif")

    setwd(owd)
  }

  unlink(dirname(tmpdir), recursive = TRUE)
})

test_that("splitRaster works in parallel", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("snow") # needed for beginCluster
  skip_if_not_installed("raster")
  skip_if_not(interactive())

  tmpdir <- file.path(tempdir(), "splitRaster-test-parallel") |>
    checkPath(create = TRUE)

  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # b <- raster::brick(system.file("external/rlogo.grd", package = "raster"))
  b <- raster::raster(system.file("ex/logo.tif", package = "terra"))
  r <- b[[1]] # use first layer only
  nx <- 3
  ny <- 4
  expect_equal(terra::xres(r), 1)
  expect_equal(terra::yres(r), 1)

  # change the extent of r
  raster::extent(r) <- raster::extent(xmin(r) - 30, xmax(r) - 30, ymin(r) - 20, ymax(r) - 20)

  # test parallel cropping
  n <- pmin(parallel::detectCores(), 4) # use up to 4 cores
  raster::beginCluster(n)
  on.exit(raster::endCluster(), add = TRUE)

  cl <- raster::getCluster()

  y11 <- splitRaster(r, nx, ny, c(3L, 4L), path = file.path(tmpdir, "red11"))
  expect_true(unique(!unlist(lapply(y11, terra::inMemory))))

  for (i in 1:12) {
    expect_true(file.exists(file.path(tmpdir, "red11", paste0("red_tile", i, ".tif"))))
  }

  xextents <- c()
  yextents <- c()
  for (i in seq_along(y11)) {
    xextents <- c(xextents, xmin(y11[[i]]), xmax(y11[[i]]))
    yextents <- c(yextents, ymin(y11[[i]]), ymax(y11[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, 1, 7, 34, 40, 71))
  expect_equal(sort(unique(yextents)), c(-20, -5, 3, 15, 23, 34, 42, 57))
  rm(xextents, yextents)

  expect_equal(length(unique(lapply(y11, crs))), 1L)
  expect_equal(unique(lapply(y11, crs))[[1]], crs(r))

  m11 <- mergeRaster(y11)
  expect_equal(dim(m11), dim(r))
  expect_equal(extent(m11), extent(r))
  expect_equal(names(m11), names(r))
  expect_equal(res(m11), res(r))
  expect_equal(max(values(m11)), max(values(r)))
  expect_equal(min(values(m11)), min(values(r)))
  raster::endCluster()
})

test_that("splitRaster and mergeRaster work on large on-disk rasters", {
  skip_on_cran()
  skip_on_ci()
  #skip_if_not(interactive() && Sys.info()[["user"]] %in% c("achubaty", "emcintir"))
  skip("this is a BIG test!")

  library(reproducible)
  library(terra)

  tmpdir <- file.path(tempdir(), "splitRaster-test-large") |> checkPath(create = TRUE)

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  ## use a large raster (1.3 GB)
  url <- paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/",
                "canada-landcover_canada-couverture-du-sol/CanadaLandcover2010.zip")
  destfile <- file.path(tmpdir, basename(url))

  r <- Cache(prepInputs(url = url, destinationPath = tmpdir))

  # class       : SpatRaster
  # dimensions  : 160001, 190001, 1  (nrow, ncol, nlyr)
  # resolution  : 30, 30  (x, y)
  # extent      : -2600030, 3100000, -885090, 3914940  (xmin, xmax, ymin, ymax)
  # coord. ref. : NAD_1983_Canada_Atlas_Lambert (EPSG:3978)
  # source      : CAN_LC_2010_CAL.tif
  # name        : CAN_LC_2010_CAL
  # min value   :               0
  # max value   :              19

  ## without buffer
  s <- splitRaster(r, 4, 4, path = file.path(tmpdir, "s")) ## takes a while to run...
  expect_equal(length(unique(lapply(s, crs))), 1)
  expect_true(compareGeom(r, s[[1]], crs = TRUE, ext = FALSE, rowcol = FALSE, res = TRUE,
                          stopOnError = FALSE))

  e <- lapply(s, ext)
  u <- e[[1]]
  for (i in seq_along(e)[-1]) {
    u <- union(e[[i]], u)
  }
  expect_equal(ext(r), u)

  m <- mergeRaster(s) ## takes a while to run...
  expect_equal(dim(m), dim(r))
  expect_equal(ext(m), ext(r))
  expect_equal(names(m), names(r))
  expect_equal(res(m), res(r))
  # expect_equal(max(m)), max(r))) ## Error: cannot allocate vector of size 4.8 Gb
  # expect_equal(min(values(m)), min(values(r))) ## Error: cannot allocate vector of size 4.8 Gb

  # with buffer
  s1 <- splitRaster(r, 4, 4, c(50, 50), path = file.path(tmpdir, "s1"))
  expect_equal(length(unique(lapply(s, crs))), 1)
  expect_true(compareGeom(r, s[[1]], crs = TRUE, ext = FALSE, rowcol = FALSE, res = TRUE,
                          stopOnError = FALSE))

  e1 <- lapply(s1, ext)
  u1 <- e1[[1]]
  for (i in seq_along(e1)[-1]) {
    u1 <- union(e1[[i]], u1)
  }
  expect_equal(ext(r), u1)

  m1 <- mergeRaster(s1) # takes a while to run...
  expect_equal(dim(m1), dim(r))
  expect_equal(ext(m1), ext(r))
  expect_equal(names(m1), names(r))
  expect_equal(res(m1), res(r))
  # expect_equal(max(values(m1)), max(values(r)))
  # expect_equal(min(values(m1)), min(values(r)))
})
