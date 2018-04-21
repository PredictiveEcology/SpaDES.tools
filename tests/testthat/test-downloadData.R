test_that("downloadData downloads and unzips module data", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules")
  datadir <- file.path(tmpdir, m, "data") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  filenames <- c("DEM.tif", "habitatQuality.tif")
  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    f <- downloadModule(m, tmpdir, quiet = TRUE)
    t1 <- system.time(downloadData(m, tmpdir, quiet = TRUE))
    result <- checksums(m, tmpdir)$result
    expect_true(all(file.exists(file.path(datadir, filenames))))
    expect_true(all(result == "OK"))

    # shouldn't need a redownload because file exists
    t2 <- system.time(downloadData(m, tmpdir, quiet = TRUE))
    expect_true(t1[3] > t2[3]) # compare elapsed times

    # if one file is missing, will fill in correctly
    unlink(file.path(datadir, filenames)[1])
    downloadData(m, tmpdir, quiet = TRUE)
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there, but one is incorrectly named
    file.rename(from = file.path(datadir, filenames[1]),
                to = file.path(datadir, "test.tif"))
    downloadData(m, tmpdir, quiet = TRUE) # renames the file back to expected
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there with correct names, but wrong content
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    if (require(rgdal, quietly = TRUE)) {
      on.exit(detach("package:rgdal"), add = TRUE)
      ras <- raster(file.path(datadir, filenames[2]))
      ras[4] <- maxValue(ras) + 1
      writeRaster(ras, filename = file.path(datadir, filenames[2]), overwrite = TRUE)
      downloadData(m, tmpdir, quiet = TRUE)
      expect_true(all(file.exists(file.path(datadir, filenames))))
    }
  }
})
